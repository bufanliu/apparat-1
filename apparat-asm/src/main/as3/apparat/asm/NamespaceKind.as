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

package apparat.asm {
	public class NamespaceKind {
		public static var NAMESPACE:NamespaceKind; // = 0x08
		public static var PACKAGE:NamespaceKind; //= 0x16
		public static var PUBLIC:NamespaceKind; //= 0x16
		public static var PACKAGEINTERNAL:NamespaceKind; // = 0x17
		public static var PROTECTED:NamespaceKind;// = 0x18
		public static var EXPLICIT:NamespaceKind;// = 0x19
		public static var STATICPROTECTED:NamespaceKind;// = 0x1a
		public static var PRIVATE:NamespaceKind;// = 0x05
	}
}
