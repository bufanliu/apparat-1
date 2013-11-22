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
package apparat.abc

import apparat.utils.{IndentingPrintWriter, Dumpable}
import apparat.bytecode.operations.GetLex

class AbcScript(var init: AbcMethod, var traits: Array[AbcTrait]) extends Dumpable with HasTraits {
  init.anonymous = false

  def accept(visitor: AbcVisitor) = visitor visit this

  override def dump(writer: IndentingPrintWriter) = {
    writer <= "Script:"
    writer withIndent {
      writer <= "Init:"
      writer withIndent {
        init dump writer
      }
      dumpTraits(writer)
    }
  }

  def isDefinedImport(name: AbcQName) = {
    (init != null) && init.body.exists {
      _.bytecode.exists {
        _.ops.exists {
          case GetLex(aName) if aName == name => true
          case _ => false
        }
      }
    }
  }
}
