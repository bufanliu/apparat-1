/*
 * This file is part of Apparat.
 *
 * Copyright (C) 2010 Joa Ebert
 * http://www.joa-ebert.com/
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 */
package apparat.taas.frontend.abc

import apparat.taas.frontend.TaasFrontend
import apparat.taas.ast._
import collection.mutable.ListBuffer
import apparat.abc._

/**
 * @author Joa Ebert
 */
class AbcFrontend(main: Abc, libraries: List[Abc]) extends TaasFrontend {
	object Synthetic extends TaasLibrary(
		ListBuffer(
			TaasPackage(Symbol(""),
				ListBuffer(
					TaasInterface('IBitmapDrawable, TaasPublic, None, ListBuffer.empty, ListBuffer.empty),
					TaasInterface('IGraphicsFill, TaasPublic, None, ListBuffer.empty, ListBuffer.empty),
					TaasInterface('IGraphicsData, TaasPublic, None, ListBuffer.empty, ListBuffer.empty),
					TaasInterface('IGraphicsPath, TaasPublic, None, ListBuffer.empty, ListBuffer.empty),
					TaasInterface('IGraphicsStroke, TaasPublic, None, ListBuffer.empty, ListBuffer.empty),
					TaasInterface('IDynamicPropertyOutput, TaasPublic, None, ListBuffer.empty, ListBuffer.empty),
					TaasInterface('IDataInput, TaasPublic, None, ListBuffer.empty, ListBuffer.empty),
					TaasInterface('IDataOutput, TaasPublic, None, ListBuffer.empty, ListBuffer.empty)
				)
			),
			TaasPackage(Symbol("flash.net"),
				ListBuffer(
					TaasInterface('IDataInput, TaasPublic, None, ListBuffer.empty, ListBuffer.empty),
					TaasInterface('IDataOutput, TaasPublic, None, ListBuffer.empty, ListBuffer.empty)
				)
			)
		)
	)

	private val ast: TaasAST = new TaasAST(ListBuffer.empty)

	override lazy val getAST = {
		main.loadBytecode()
		libraries foreach { _.loadBytecode() }

		val target = TaasTarget(ListBuffer.empty)
		val lib = TaasLibrary(ListBuffer.empty)
		ast.children append Synthetic
		ast.children append target
		ast.children append lib

		libraries foreach parse(lib) _
		parse(target)(main)

		ast.init()
	}

	private def parse(unit: TaasUnit)(abc: Abc) = new AbcParser(ast, abc, unit).parseAbc()
}
