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
/**
 * @author Patrick Le Clec'h
 */
package apparat.bytecode.optimization

import apparat.bytecode.operations._
import apparat.abc._
import annotation.tailrec
import apparat.bytecode.{Marker, Bytecode}
import apparat.tools.ApparatLog
import apparat.bytecode.analysis.StackAnalysis
import collection.SeqView

object AsmExpansion {
	// TODO callMethod,debug,hasNext2,newCatch,newClass,newFunction,pushNamespace

	private val AsmNamespace = AbcNamespace(AbcNamespaceKind.Package, Symbol("apparat.asm"))
	val __asm = AbcQName('__asm, AsmNamespace)
	private val __maxStack = AbcQName('__maxStack, AsmNamespace)
	private val __dumpAfterASM = AbcQName('__dumpAfterASM, AsmNamespace)
	private val __nakedName = AbcQName('__naked, AsmNamespace)
	private val __as3 = AbcQName('__as3, AsmNamespace)
	private val __cint = AbcQName('__cint, AsmNamespace)
	private val __beginRepeat = AbcQName('__beginRepeat, AsmNamespace)
	private val __endRepeat = AbcQName('__endRepeat, AsmNamespace)

	private lazy val intName = AbcQName('int, AbcNamespace(AbcNamespaceKind.Package, Symbol("")))

	private lazy val abcQName = AbcQName('AbcQName, AsmNamespace)
	private lazy val abcQNameA = AbcQName('AbcQNameA, AsmNamespace)
	private lazy val abcRTQName = AbcQName('AbcRTQName, AsmNamespace)
	private lazy val abcRTQNameA = AbcQName('AbcRTQNameA, AsmNamespace)
	private lazy val abcRTQNameL = AbcQName('AbcRTQNameL, AsmNamespace)
	private lazy val abcRTQNameLA = AbcQName('AbcRTQNameLA, AsmNamespace)
	private lazy val abcMultiname = AbcQName('AbcMultiname, AsmNamespace)
	private lazy val abcMultinameA = AbcQName('AbcMultinameA, AsmNamespace)
	private lazy val abcMultinameL = AbcQName('AbcMultinameL, AsmNamespace)
	private lazy val abcMultinameLA = AbcQName('AbcMultinameLA, AsmNamespace)
	private lazy val abcNamespace = AbcQName('AbcNamespace, AsmNamespace)
	private lazy val namespaceKind = AbcQName('NamespaceKind, AsmNamespace)
	private lazy val abcNamespaceSet = AbcQName('AbcNamespaceSet, AsmNamespace)

	private lazy val add = AbcQName('Add, AsmNamespace)
	private lazy val addInt = AbcQName('AddInt, AsmNamespace)
	private lazy val applyType = AbcQName('ApplyType, AsmNamespace)
	private lazy val asType = AbcQName('AsType, AsmNamespace)
	private lazy val asTypeLate = AbcQName('AsTypeLate, AsmNamespace)
	private lazy val bitAnd = AbcQName('BitAnd, AsmNamespace)
	private lazy val bitNot = AbcQName('BitNot, AsmNamespace)
	private lazy val bitOr = AbcQName('BitOr, AsmNamespace)
	private lazy val bitXor = AbcQName('BitXor, AsmNamespace)
	private lazy val breakpoint = AbcQName('Breakpoint, AsmNamespace)
	private lazy val breakpointLine = AbcQName('BreakpointLine, AsmNamespace)
	private lazy val call = AbcQName('Call, AsmNamespace)
	private lazy val callMethod = AbcQName('CallMethod, AsmNamespace)
	private lazy val callProperty = AbcQName('CallProperty, AsmNamespace)
	private lazy val callPropLex = AbcQName('CallPropLex, AsmNamespace)
	private lazy val callPropVoid = AbcQName('CallPropVoid, AsmNamespace)
	private lazy val callStatic = AbcQName('CallStatic, AsmNamespace)
	private lazy val callSuper = AbcQName('CallSuper, AsmNamespace)
	private lazy val callSuperVoid = AbcQName('CallSuperVoid, AsmNamespace)
	private lazy val checkFilter = AbcQName('CheckFilter, AsmNamespace)
	private lazy val coerce = AbcQName('Coerce, AsmNamespace)
	private lazy val coerceAny = AbcQName('CoerceAny, AsmNamespace)
	private lazy val coerceBoolean = AbcQName('CoerceBoolean, AsmNamespace)
	private lazy val coerceDouble = AbcQName('CoerceDouble, AsmNamespace)
	private lazy val coerceInt = AbcQName('CoerceInt, AsmNamespace)
	private lazy val coerceObject = AbcQName('CoerceObject, AsmNamespace)
	private lazy val coerceString = AbcQName('CoerceString, AsmNamespace)
	private lazy val coerceUInt = AbcQName('CoerceUInt, AsmNamespace)
	private lazy val construct = AbcQName('Construct, AsmNamespace)
	private lazy val constructProp = AbcQName('ConstructProp, AsmNamespace)
	private lazy val constructSuper = AbcQName('ConstructSuper, AsmNamespace)
	private lazy val convertBoolean = AbcQName('ConvertBoolean, AsmNamespace)
	private lazy val convertInt = AbcQName('ConvertInt, AsmNamespace)
	private lazy val convertDouble = AbcQName('ConvertDouble, AsmNamespace)
	private lazy val convertObject = AbcQName('ConvertObject, AsmNamespace)
	private lazy val convertString = AbcQName('ConvertString, AsmNamespace)
	private lazy val convertUInt = AbcQName('ConvertUInt, AsmNamespace)
	private lazy val debug = AbcQName('Debug, AsmNamespace)
	private lazy val debugFile = AbcQName('DebugFile, AsmNamespace)
	private lazy val debugLine = AbcQName('DebugLine, AsmNamespace)
	private lazy val decLocal = AbcQName('DecLocal, AsmNamespace)
	private lazy val decLocalInt = AbcQName('DecLocalInt, AsmNamespace)
	private lazy val decrement = AbcQName('Decrement, AsmNamespace)
	private lazy val decrementInt = AbcQName('DecrementInt, AsmNamespace)
	private lazy val deleteProperty = AbcQName('DeleteProperty, AsmNamespace)
	private lazy val divide = AbcQName('Divide, AsmNamespace)
	private lazy val dup = AbcQName('Dup, AsmNamespace)
	private lazy val defaultXMLNamespace = AbcQName('DefaultXMLNamespace, AsmNamespace)
	private lazy val defaultXMLNamespaceLate = AbcQName('DefaultXMLNamespaceLate, AsmNamespace)
	private lazy val equals = AbcQName('Equals, AsmNamespace)
	private lazy val escapeXMLAttribute = AbcQName('EscapeXMLAttribute, AsmNamespace)
	private lazy val escapeXMLElement = AbcQName('EscapeXMLElement, AsmNamespace)
	private lazy val findDef = AbcQName('FindDef, AsmNamespace)
	private lazy val findProperty = AbcQName('FindProperty, AsmNamespace)
	private lazy val findPropStrict = AbcQName('FindPropStrict, AsmNamespace)
	private lazy val getDescendants = AbcQName('GetDescendants, AsmNamespace)
	private lazy val getGlobalScope = AbcQName('GetGlobalScope, AsmNamespace)
	private lazy val getGlobalSlot = AbcQName('GetGlobalSlot, AsmNamespace)
	private lazy val getLex = AbcQName('GetLex, AsmNamespace)
	private lazy val getLocal = AbcQName('GetLocal, AsmNamespace)
	private lazy val getLocal0 = AbcQName('GetLocal0, AsmNamespace)
	private lazy val getLocal1 = AbcQName('GetLocal1, AsmNamespace)
	private lazy val getLocal2 = AbcQName('GetLocal2, AsmNamespace)
	private lazy val getLocal3 = AbcQName('GetLocal3, AsmNamespace)
	private lazy val getProperty = AbcQName('GetProperty, AsmNamespace)
	private lazy val getScopeObject = AbcQName('GetScopeObject, AsmNamespace)
	private lazy val getSlot = AbcQName('GetSlot, AsmNamespace)
	private lazy val getSuper = AbcQName('GetSuper, AsmNamespace)
	private lazy val greaterEquals = AbcQName('GreaterEquals, AsmNamespace)
	private lazy val greaterThan = AbcQName('GreaterThan, AsmNamespace)
	private lazy val hasNext = AbcQName('HasNext, AsmNamespace)
	private lazy val hasNext2 = AbcQName('HasNext2, AsmNamespace)
	private lazy val ifEqual = AbcQName('IfEqual, AsmNamespace)
	private lazy val ifFalse = AbcQName('IfFalse, AsmNamespace)
	private lazy val ifGreaterEqual = AbcQName('IfGreaterEqual, AsmNamespace)
	private lazy val ifGreaterThan = AbcQName('IfGreaterThan, AsmNamespace)
	private lazy val ifLessEqual = AbcQName('IfLessEqual, AsmNamespace)
	private lazy val ifLessThan = AbcQName('IfLessThan, AsmNamespace)
	private lazy val ifNotGreaterEqual = AbcQName('IfNotGreaterEqual, AsmNamespace)
	private lazy val ifNotGreaterThan = AbcQName('IfNotGreaterThan, AsmNamespace)
	private lazy val ifNotLessEqual = AbcQName('IfNotLessEqual, AsmNamespace)
	private lazy val ifNotLessThan = AbcQName('IfNotLessThan, AsmNamespace)
	private lazy val ifNotEqual = AbcQName('IfNotEqual, AsmNamespace)
	private lazy val ifStrictEqual = AbcQName('IfStrictEqual, AsmNamespace)
	private lazy val ifStrictNotEqual = AbcQName('IfStrictNotEqual, AsmNamespace)
	private lazy val ifTrue = AbcQName('IfTrue, AsmNamespace)
	private lazy val in = AbcQName('In, AsmNamespace)
	private lazy val incLocal = AbcQName('IncLocal, AsmNamespace)
	private lazy val incLocalInt = AbcQName('IncLocalInt, AsmNamespace)
	private lazy val increment = AbcQName('Increment, AsmNamespace)
	private lazy val incrementInt = AbcQName('IncrementInt, AsmNamespace)
	private lazy val initProperty = AbcQName('InitProperty, AsmNamespace)
	private lazy val instanceOf = AbcQName('InstanceOf, AsmNamespace)
	private lazy val isType = AbcQName('IsType, AsmNamespace)
	private lazy val isTypeLate = AbcQName('IsTypeLate, AsmNamespace)
	private lazy val jump = AbcQName('Jump, AsmNamespace)
	private lazy val kill = AbcQName('Kill, AsmNamespace)
	private lazy val label = AbcQName('Label, AsmNamespace)
	private lazy val lessEquals = AbcQName('LessEquals, AsmNamespace)
	private lazy val lessThan = AbcQName('LessThan, AsmNamespace)
	private lazy val lookupSwitch = AbcQName('LookupSwitch, AsmNamespace)
	private lazy val shiftLeft = AbcQName('ShiftLeft, AsmNamespace)
	private lazy val modulo = AbcQName('Modulo, AsmNamespace)
	private lazy val multiply = AbcQName('Multiply, AsmNamespace)
	private lazy val multiplyInt = AbcQName('MultiplyInt, AsmNamespace)
	private lazy val negate = AbcQName('Negate, AsmNamespace)
	private lazy val negateInt = AbcQName('NegateInt, AsmNamespace)
	private lazy val newActivation = AbcQName('NewActivation, AsmNamespace)
	private lazy val newArray = AbcQName('NewArray, AsmNamespace)
	private lazy val newCatch = AbcQName('NewCatch, AsmNamespace)
	private lazy val newClass = AbcQName('NewClass, AsmNamespace)
	private lazy val newFunction = AbcQName('NewFunction, AsmNamespace)
	private lazy val newObject = AbcQName('NewObject, AsmNamespace)
	private lazy val nextName = AbcQName('NextName, AsmNamespace)
	private lazy val nextValue = AbcQName('NextValue, AsmNamespace)
	private lazy val nop = AbcQName('Nop, AsmNamespace)
	private lazy val not = AbcQName('Not, AsmNamespace)
	private lazy val pop = AbcQName('Pop, AsmNamespace)
	private lazy val popScope = AbcQName('PopScope, AsmNamespace)
	private lazy val pushByte = AbcQName('PushByte, AsmNamespace)
	private lazy val pushDouble = AbcQName('PushDouble, AsmNamespace)
	private lazy val pushFalse = AbcQName('PushFalse, AsmNamespace)
	private lazy val pushInt = AbcQName('PushInt, AsmNamespace)
	private lazy val pushNamespace = AbcQName('PushNamespace, AsmNamespace)
	private lazy val pushNaN = AbcQName('PushNaN, AsmNamespace)
	private lazy val pushNull = AbcQName('PushNull, AsmNamespace)
	private lazy val pushScope = AbcQName('PushScope, AsmNamespace)
	private lazy val pushShort = AbcQName('PushShort, AsmNamespace)
	private lazy val pushString = AbcQName('PushString, AsmNamespace)
	private lazy val pushTrue = AbcQName('PushTrue, AsmNamespace)
	private lazy val pushUInt = AbcQName('PushUInt, AsmNamespace)
	private lazy val pushUndefined = AbcQName('PushUndefined, AsmNamespace)
	private lazy val pushWith = AbcQName('PushWith, AsmNamespace)
	private lazy val returnValue = AbcQName('ReturnValue, AsmNamespace)
	private lazy val returnVoid = AbcQName('ReturnVoid, AsmNamespace)
	private lazy val shiftRight = AbcQName('ShiftRight, AsmNamespace)
	private lazy val setLocal = AbcQName('SetLocal, AsmNamespace)
	private lazy val setLocal0 = AbcQName('SetLocal0, AsmNamespace)
	private lazy val setLocal1 = AbcQName('SetLocal1, AsmNamespace)
	private lazy val setLocal2 = AbcQName('SetLocal2, AsmNamespace)
	private lazy val setLocal3 = AbcQName('SetLocal3, AsmNamespace)
	private lazy val setGlobalSlot = AbcQName('SetGlobalSlot, AsmNamespace)
	private lazy val setProperty = AbcQName('SetProperty, AsmNamespace)
	private lazy val setSlot = AbcQName('SetSlot, AsmNamespace)
	private lazy val setSuper = AbcQName('SetSuper, AsmNamespace)
	private lazy val strictEquals = AbcQName('StrictEquals, AsmNamespace)
	private lazy val subtract = AbcQName('Subtract, AsmNamespace)
	private lazy val subtractInt = AbcQName('SubtractInt, AsmNamespace)
	private lazy val swap = AbcQName('Swap, AsmNamespace)
	private lazy val `throw` = AbcQName('Throw, AsmNamespace)
	private lazy val typeOf = AbcQName('TypeOf, AsmNamespace)
	private lazy val shiftRightUnsigned = AbcQName('ShiftRightUnsigned, AsmNamespace)
	private lazy val setByte = AbcQName('SetByte, AsmNamespace)
	private lazy val setShort = AbcQName('SetShort, AsmNamespace)
	private lazy val setInt = AbcQName('SetInt, AsmNamespace)
	private lazy val setFloat = AbcQName('SetFloat, AsmNamespace)
	private lazy val setDouble = AbcQName('SetDouble, AsmNamespace)
	private lazy val getByte = AbcQName('GetByte, AsmNamespace)
	private lazy val getShort = AbcQName('GetShort, AsmNamespace)
	private lazy val getInt = AbcQName('GetInt, AsmNamespace)
	private lazy val getFloat = AbcQName('GetFloat, AsmNamespace)
	private lazy val getDouble = AbcQName('GetDouble, AsmNamespace)
	private lazy val sign1 = AbcQName('Sign1, AsmNamespace)
	private lazy val sign8 = AbcQName('Sign8, AsmNamespace)
	private lazy val sign16 = AbcQName('Sign16, AsmNamespace)

	def apply(bytecode: Bytecode): Boolean = {
		var modified = false
		var removes = List.empty[AbstractOp]
		var replacements = Map.empty[AbstractOp, List[AbstractOp]]
		var stack = List.empty[AbstractOp]
		var balance = 0
		val optDebugFile: Option[DebugFile] = bytecode.ops.find(op => op.opCode == Op.debugfile).asInstanceOf[Option[DebugFile]]
		var lineNum = 0

		val markers = bytecode.markers
		var markerMap = Map.empty[Symbol, Marker]
		var unresolveMarkerMap = Map.empty[Symbol, Marker]
		var isBackwardMarker = Map.empty[Marker, Boolean]

		var maxStack = 0L
		var dumpAfterASM: Option[String] = None
		var naked = false

		def nextOp() = {
			val op = stack.head
			stack = stack.tail
			op
		}
		def expectNextOp(msg: String = "missing argument(s)") = {
			if(stack.isEmpty) {
				throwError(msg)
			}
			nextOp()
		}
		def throwError(msg: String) {
			optDebugFile match {
				case Some(aDebugFile) => {
					bytecode.dump()
					sys.error(aDebugFile.file + ":" + lineNum + " => " + msg)
				}
				case _ => {
					bytecode.dump()
					sys.error(msg)
				}
			}
		}

		@inline def readABCNamespaceSet(msg: String = ""): AbcNSSet = {
			var nsList = List.empty[AbcNamespace]
			@tailrec def loop() {
				if(stack.nonEmpty) {
					stack.head match {
						case cp@CallProperty(abcName, x) if abcName == abcNamespaceSet => {
							nextOp()
							removes = cp :: removes
						}
						case fp@FindPropStrict(aName) => {
							removes = fp :: removes
							aName match {
								case AbcQName('__as3, AsmNamespace) => {
									resolveABCName('AbcNamespaceSet) match {
										case Some(abcName) => abcName match {
											case qn: AbcQName => nsList = qn.namespace :: nsList
											case qn: AbcQNameA => nsList = qn.namespace :: nsList
											case _ => throwError("abcNamespaceSet is expecting abcNamespace arguments")
										}
										case _ =>
									}
								}
								case AbcQName('__cint, AsmNamespace) => {
									resolveABCName('AbcNamespaceSet) match {
										case Some(abcName) => abcName match {
											case qn: AbcQName => nsList = qn.namespace :: nsList
											case qn: AbcQNameA => nsList = qn.namespace :: nsList
											case _ => throwError("abcNamespaceSet is expecting abcNamespace arguments")
										}
										case _ =>
									}
								}
								case AbcQName('AbcNamespace, AsmNamespace) => {
									nextOp()
									readABCNamespace("abcNamespaceSet is expecting abcNamespace arguments") match {
										case Some(abcNs) => nsList = abcNs :: nsList
										case _ => throwError("abcNamespaceSet is expecting a list of abcNamespace arguments")
									}
								}
								case _ => throwError("invalid call to abcNamespaceSet, expecting a list of abcNamespace as arguments")
							}
							loop()
						}
						case _ => throwError("invalid call to abcNamespaceSet, expecting a list of abcNamespace as arguments")
					}
				}
			}
			loop()
			AbcNSSet(nsList.reverse.toArray)
		}

		@inline def readABCNamespace(msg: String = ""): Option[AbcNamespace] = {
			var ret: Option[AbcNamespace] = None
			expectNextOp(msg) match {
				case op1@GetLex(nsKind) if nsKind == namespaceKind =>
					expectNextOp() match {
						case op2@GetProperty(typeName) if typeName.kind == AbcNameKind.QName => {
							val qName = typeName.asInstanceOf[AbcQName]
							expectNextOp() match {
								case op3@PushString(name) => {
									qName.name match {
										case 'NAMESPACE => {
											removes = op1 :: op2 :: op3 :: removes
											ret = Some(AbcNamespace(AbcNamespaceKind.Namespace, name))
										}
										case 'PUBLIC => {
											removes = op1 :: op2 :: op3 :: removes
											ret = Some(AbcNamespace(AbcNamespaceKind.Package, name))
										}
										case 'PACKAGE => {
											removes = op1 :: op2 :: op3 :: removes
											ret = Some(AbcNamespace(AbcNamespaceKind.Package, name))
										}
										case 'PACKAGEINTERNAL => {
											removes = op1 :: op2 :: op3 :: removes
											ret = Some(AbcNamespace(AbcNamespaceKind.PackageInternal, name))
										}
										case 'PROTECTED => {
											removes = op1 :: op2 :: op3 :: removes
											ret = Some(AbcNamespace(AbcNamespaceKind.Namespace, name))
										}
										case 'EXPLICIT => {
											removes = op1 :: op2 :: op3 :: removes
											ret = Some(AbcNamespace(AbcNamespaceKind.Protected, name))
										}
										case 'STATICPROTECTED => {
											removes = op1 :: op2 :: op3 :: removes
											ret = Some(AbcNamespace(AbcNamespaceKind.StaticProtected, name))
										}
										case 'PRIVATE => {
											removes = op1 :: op2 :: op3 :: removes
											ret = Some(AbcNamespace(AbcNamespaceKind.Private, name))
										}
										case _@str => throwError("Namespace kind must be one of NAMESPACE,PACKAGE,PACKAGEINTERNAL,PROTECTED,EXPLICIT,STATICPROTECTED,PRIVATE. Not : " + str)
									}
								}
								case _ => throwError("missing arguments for abcNamespace")
							}
							expectNextOp("invalid call to abcNamespace") match {
								case cp@CallProperty(aName, 2) if aName == abcNamespace => removes = cp :: removes
								case _ => throwError("invalid call to abcNamespace")
							}
						}
						case _ => {throwError("missing arguments for abcNamespace"); None}
					}
				case _ => {throwError(msg); None}
			}
			ret
		}

		@inline def readUntil(abcName: AbcName) = {
			var ret = List.empty[AbstractOp]
			@tailrec def loop(): Unit = {
				if(stack.nonEmpty) {
					nextOp() match {
						case op@CallProperty(aName, count) if aName == abcName => ret = op :: ret
						case _@op => {
							ret = op :: ret
							loop()
						}
					}
				}
			}
			loop()
			ret
		}

		def resolveMarker(symbol: Symbol) = {
			val newSymbol = Symbol(symbol.toString().tail + ":")
			if(markerMap.contains(newSymbol)) {
				val marker = markerMap(newSymbol)
				isBackwardMarker = isBackwardMarker.updated(marker, true)
				marker
			} else if(unresolveMarkerMap.contains(newSymbol)) {
				val marker = unresolveMarkerMap(newSymbol)
				isBackwardMarker = isBackwardMarker.updated(marker, isBackwardMarker.getOrElse(marker, false))
				marker
			} else {
				val marker = markers.mark(Nop())
				unresolveMarkerMap = unresolveMarkerMap.updated(newSymbol, marker)
				isBackwardMarker = isBackwardMarker.updated(marker, isBackwardMarker.getOrElse(marker, false))
				marker
			}
		}

		@inline def readOp_Marker_Markers(opName: Symbol, abcName: AbcName, opFactory: (Marker, Array[Marker]) => AbstractOp) {
			expectNextOp(opName + " expect a string label as first parameter") match {
				case ps@PushString(symbol) => {
					if(symbol.toString().last == ':') {
						throwError("the target label " + symbol + " mustn't ended with :")
					}

					val defaultCase = resolveMarker(symbol)

					var cases = List.empty[Marker]

					@tailrec def loop() {
						if(stack.nonEmpty) {
							stack.head match {
								case cp@CallProperty(aName, count) if aName == abcName => {
									removes = cp :: removes
									nextOp()
								}
								case ps@PushString(aSymbol) => {
									if(aSymbol.toString().last == ':') {
										throwError("the target label " + aSymbol + " mustn't ended with :")
									}
									cases = resolveMarker(aSymbol) :: cases
									removes = ps :: removes
									nextOp()
									loop()
								}
								case _ => throwError("cases arguments have to been of String type into " + opName)
							}
						} else
							throwError("invalid call to " + opName)
					}
					loop()

					replacements = replacements.updated(ps, List(opFactory(defaultCase, cases.reverse.toArray)))
				}
				case _ => throwError(opName + " is expecting a label as first argument" + opName)
			}
		}

		@inline def readOp_Marker(opName: Symbol, abcName: AbcName, opFactory: (Marker) => AbstractOp) {
			expectNextOp(opName + " expect a string as parameter") match {
				case ps@PushString(symbol) => {
					if(symbol.toString().last == ':') {
						throwError("the target label " + symbol + " mustn't ended with :")
					}

					expectNextOp("invalid call to " + opName) match {
						case cp@CallProperty(aName, 1) if aName == abcName => {
							removes = ps :: removes
							val marker = resolveMarker(symbol)
							replacements = replacements.updated(cp, List(opFactory(marker)))
						}
						case _ => throwError("invalid call to " + opName)
					}
				}
				case _ => throwError("expecting " + opName)
			}
		}

		@inline def readOp_String(opName: Symbol, abcName: AbcName, opFactory: (Symbol) => AbstractOp) {
			expectNextOp(opName + " expect a string as parameter") match {
				case ps@PushString(str) => {
					expectNextOp("invalid call to " + opName) match {
						case cp@CallProperty(aName, 1) if aName == abcName => {
							removes = ps :: removes
							replacements = replacements.updated(cp, List(opFactory(str)))
						}
						case _ => throwError("invalid call to " + opName)
					}
				}
				case _ => throwError("expecting " + opName)
			}
		}
		@inline def readOp_Register(opName: Symbol, abcName: AbcName, opFactory: (Int) => AbstractOp) {
			var ops = readUntil(abcName)
			if(ops.isEmpty) {
				throwError(opName + " expect a variable name or a register number")
			} else ops.head match {
				case cp@CallProperty(aName, count) if aName == abcName => {
					removes = cp :: removes
					ops = ops.tail
					if(ops.isEmpty) {
						throwError(opName + " expect a variable name or a register number")
					} else ops.head match {
						case gs@GetSlot(register) => {
							opFactory(register) match {
								case op: GetLocal => replacements = replacements.updated(gs, List(GetSlot(register)))
								case op: SetLocal => replacements = replacements.updated(gs, List(SetSlot(register)))
								case _@op => replacements = replacements.updated(gs, List(op))
							}
						}
						case gl@GetLocal(register) => replacements = replacements.updated(gl, List(opFactory(register)))
						case pb@PushByte(register) => replacements = replacements.updated(pb, List(opFactory(register)))
						case ps@PushShort(register) => replacements = replacements.updated(ps, List(opFactory(register)))
						case pi@PushInt(register) => replacements = replacements.updated(pi, List(opFactory(register)))
						case _ => throwError(opName + " expect a variable name or a register number")
					}
				}
				case _ => throwError(opName + " expect a variable name or a register number")
			}
		}
		@inline def readOp_Int(opName: Symbol, abcName: AbcName, opFactory: (Int) => AbstractOp) {
			expectNextOp(opName + " expect an integer as parameter") match {
				case pb@PushByte(value) => {
					expectNextOp("invalid call to " + opName) match {
						case cp@CallProperty(aName, 1) if aName == abcName => {
							removes = pb :: removes
							replacements = replacements.updated(cp, List(opFactory(value)))
						}
						case _ => throwError("invalid call to " + opName)
					}
				}
				case ps@PushShort(value) => {
					expectNextOp("invalid call to " + opName) match {
						case cp@CallProperty(aName, 1) if aName == abcName => {
							removes = ps :: removes
							replacements = replacements.updated(cp, List(opFactory(value)))
						}
						case _ => throwError("invalid call to " + opName)
					}
				}
				case pi@PushInt(value) => {
					expectNextOp("invalid call to " + opName) match {
						case cp@CallProperty(aName, 1) if aName == abcName => {
							removes = pi :: removes
							replacements = replacements.updated(cp, List(opFactory(value)))
						}
						case _ => throwError("invalid call to " + opName)
					}
				}
				case _ => throwError(opName + " expect an integer as parameter")
			}
		}
		@inline def readOp_Int_Int(opName: Symbol, abcName: AbcName, opFactory: (Int, Int) => AbstractOp) {
			expectNextOp(opName + " expect an integer as first parameter") match {
				case pb@PushByte(value) => {
					expectNextOp(opName + " expect an integer as second parameter") match {
						case pb2@PushByte(value2) => {
							expectNextOp("invalid call to " + opName) match {
								case cp@CallProperty(aName, 2) if aName == abcName => {
									removes = pb :: pb2 :: removes
									replacements = replacements.updated(cp, List(opFactory(value, value2)))
								}
								case _ => throwError("invalid call to " + opName)
							}
						}
						case dup@Dup() => {
							expectNextOp("invalid call to " + opName) match {
								case cp@CallProperty(aName, 2) if aName == abcName => {
									removes = pb :: dup :: removes
									replacements = replacements.updated(cp, List(opFactory(value, value)))
								}
								case _ => throwError("invalid call to " + opName)
							}
						}
						case _ => throwError("invalid call to " + opName)
					}
				}
				// TODO other int push
				//				case ps@PushShort(value) => {
				//					expectNextOp("invalid call to " + opName) match {
				//						case cp@CallProperty(aName, 1) if (aName == abcName) => {
				//							removes = ps :: removes
				//							replacements = replacements.updated(cp, List(opFactory(value)))
				//						}
				//						case _ => throwError("invalid call to " + opName)
				//					}
				//				}
				//				case pi@PushInt(value) => {
				//					expectNextOp("invalid call to " + opName) match {
				//						case cp@CallProperty(aName, 1) if (aName == abcName) => {
				//							removes = pi :: removes
				//							replacements = replacements.updated(cp, List(opFactory(value)))
				//						}
				//						case _ => throwError("invalid call to " + opName)
				//					}
				//				}
				case _ => throwError(opName + " expect an integer as parameter")
			}
		}
		@inline def readOp_AbcName(opName: Symbol, abcName: AbcName, opFactory: (AbcName) => AbstractOp) {
			resolveABCName(opName) match {
				case Some(name) => {
					expectNextOp("invalid call to " + opName) match {
						case cp@CallProperty(aName, 1) if aName == abcName => replacements = replacements.updated(cp, List(opFactory(name)))
						case _ => throwError("invalid call to " + opName)
					}
				}
				case _ =>
			}
		}
		@inline def readOp_AbcName_Int(opName: Symbol, abcName: AbcName, opFactory: (AbcName, Int) => AbstractOp) {
			resolveABCName(opName) match {
				case Some(aName) => {
					expectNextOp(opName + " expect arguments count as second parameter") match {
						case pb@PushByte(count) => {
							expectNextOp("invalid call to " + opName) match {
								case cp@CallProperty(thatName, 2) if thatName == abcName => {
									removes = pb :: removes
									replacements = replacements.updated(cp, List(opFactory(aName, count)))
								}
								case x@_ => throwError("invalid call to " + opName)
							}
						}
						case pi@PushInt(count) => {
							expectNextOp("invalid call to " + opName) match {
								case cp@CallProperty(thatName, 2) if thatName == abcName => {
									removes = pi :: removes
									replacements = replacements.updated(cp, List(opFactory(aName, count)))
								}
								case _ => throwError("invalid call to " + opName)
							}
						}
						case _ => throwError(opName + " expect arguments count as second parameter")
					}
				}
				case _ =>
			}
		}
		def resolveABCName(asmOpName: Symbol): Option[AbcName] = {
			var ret: Option[AbcName] = None
			expectNextOp("invalid call to " + asmOpName) match {
				case gl@GetLex(abcName) => {
					abcName match {
						case AbcQName('AbcRTQNameL, AsmNamespace) => {
							removes = gl :: removes
							ret = Some(AbcRTQNameL)
						}
						case AbcQName('AbcRTQNameLA, AsmNamespace) => {
							removes = gl :: removes
							ret = Some(AbcRTQNameLA)
						}
						case _@e => throwError("unknown abcName type :" + e + " into " + asmOpName)
					}
				}
				case fp@FindPropStrict(abcName) => {
					abcName match {
						case AbcQName('AbcMultinameLA, AsmNamespace) => {
							expectNextOp("AbcMultinameLA is expecting an abcNamespaceSet as argument into " + asmOpName) match {
								case fp2@FindPropStrict(fpAbcNamespace) if fpAbcNamespace == abcNamespaceSet => {
									removes = fp2 :: removes
									val ns = readABCNamespaceSet("AbcMultinameLA is expecting an abcNamespaceSet as argument into " + asmOpName)
									ret = Some(AbcMultinameLA(ns))
									expectNextOp("invalid call to AbcMultinameLA into " + asmOpName) match {
										case cp@CallProperty(aName, 1) if aName == abcMultinameLA => removes = cp :: removes
										case _ => throwError("invalid call to AbcMultinameLA into " + asmOpName)
									}
								}
								case _ => throwError("AbcMultinameLA is expecting an AbcNamespaceSet as argument into " + asmOpName)
							}
						}
						case AbcQName('AbcMultinameL, AsmNamespace) => {
							expectNextOp("AbcMultinameL is expecting an AbcNamespaceSet as argument into " + asmOpName) match {
								case fp2@FindPropStrict(fpAbcNamespace) if fpAbcNamespace == abcNamespaceSet => {
									removes = fp2 :: removes
									val ns = readABCNamespaceSet("AbcMultinameL is expecting an AbcNamespaceSet as argument into " + asmOpName)
									ret = Some(AbcMultinameL(ns))
									expectNextOp("invalid call to AbcMultinameL into " + asmOpName) match {
										case cp@CallProperty(aName, 1) if aName == abcMultinameL => removes = cp :: removes
										case _ => throwError("invalid call to AbcMultinameL into " + asmOpName)
									}
								}
								case _ => throwError("AbcMultinameL is expecting an AbcNamespaceSet as argument into " + asmOpName)
							}
						}
						case AbcQName('AbcMultinameA, AsmNamespace) => {
							expectNextOp("AbcMultinameA is expecting a string as it's first argument into " + asmOpName) match {
								case ps@PushString(psValue) => {
									removes = ps :: removes
									expectNextOp("AbcMultinameA is expecting an AbcNamespaceSet as it's second argument into " + asmOpName) match {
										case fp2@FindPropStrict(fpAbcNamespace) if fpAbcNamespace == abcNamespaceSet => {
											removes = fp2 :: removes
											val ns = readABCNamespaceSet("AbcMultinameA is expecting an AbcNamespaceSet as it's second argument into " + asmOpName)
											ret = Some(AbcMultinameA(psValue, ns))
											expectNextOp("invalid call to abcMultinameA into " + asmOpName) match {
												case cp@CallProperty(aName, 2) if aName == abcMultinameA => removes = cp :: removes
												case _ => throwError("invalid call to abcMultinameA into " + asmOpName)
											}
										}
										case _ => throwError("AbcMultinameA is expecting an AbcNamespaceSet as it's second argument into " + asmOpName)
									}
								}
								case _ => throwError("AbcMultinameA is expecting a string as it's first argument into " + asmOpName)
							}
						}
						case AbcQName('AbcMultiname, AsmNamespace) => {
							expectNextOp("AbcMultiname is expecting a string as it's first argument into " + asmOpName) match {
								case ps@PushString(psValue) => {
									removes = ps :: removes
									expectNextOp("AbcMultiname is expecting an AbcNamespaceSet as it's second argument into " + asmOpName) match {
										case fp2@FindPropStrict(fpAbcNamespace) if fpAbcNamespace == abcNamespaceSet => {
											removes = fp2 :: removes
											val ns = readABCNamespaceSet("AbcMultiname is expecting an AbcNamespaceSet as it's second argument into " + asmOpName)
											ret = Some(AbcMultiname(psValue, ns))
											expectNextOp("invalid call to abcMultiname into " + asmOpName) match {
												case cp@CallProperty(aName, 2) if aName == abcMultiname => removes = cp :: removes
												case _ => throwError("invalid call to abcMultiname into " + asmOpName)
											}
										}
										case _ => throwError("AbcMultiname is expecting an AbcNamespaceSet as it's second argument into " + asmOpName)
									}
								}
								case _ => throwError("AbcMultiname is expecting a string as it's first argument into " + asmOpName)
							}
						}
						case AbcQName('AbcRTQNameA, AsmNamespace) => {
							expectNextOp("AbcRTQNameA is expecting a string as argument into " + asmOpName) match {
								case ps@PushString(psValue) => {
									removes = ps :: removes
									ret = Some(AbcRTQNameA(psValue))
									expectNextOp("invalid call to AbcRTQNameA into " + asmOpName) match {
										case cp@CallProperty(aName, 1) if aName == abcRTQNameA => removes = cp :: removes
										case _ => throwError("invalid call to AbcRTQNameA into " + asmOpName)
									}
								}
								case _ => throwError("AbcRTQNameA is expecting a string as argument into " + asmOpName)
							}
						}
						case AbcQName('AbcRTQName, AsmNamespace) => {
							expectNextOp("AbcRTQName is expecting a string as argument into " + asmOpName) match {
								case ps@PushString(psValue) => {
									removes = ps :: removes
									ret = Some(AbcRTQName(psValue))
									expectNextOp("invalid call to AbcRTQName into " + asmOpName) match {
										case cp@CallProperty(aName, 1) if aName == abcRTQName => removes = cp :: removes
										case _ => throwError("invalid call to AbcRTQName into " + asmOpName)
									}
								}
								case _ => throwError("AbcRTQName is expecting a string as argument into " + asmOpName)
							}
						}
						case AbcQName('AbcQNameA, AsmNamespace) => {
							expectNextOp("AbcQNameA is expecting a string as it's first argument into " + asmOpName) match {
								case ps@PushString(psValue) => {
									removes = ps :: removes
									expectNextOp("AbcQNameA is expecting an abcNamespace as it's second argument into " + asmOpName) match {
										case fp2@FindPropStrict(fpAbcNamespace) if fpAbcNamespace == abcNamespace => {
											removes = fp2 :: removes
											readABCNamespace("AbcQNameA is expecting an abcNamespace as it's second argument into " + asmOpName) match {
												case Some(ns) => {
													ret = Some(AbcQNameA(psValue, ns))
													expectNextOp("invalid call to AbcQNameA into " + asmOpName) match {
														case cp@CallProperty(aName, 2) if aName == abcQNameA => removes = cp :: removes
														case _ => throwError("invalid call to AbcQNameA into " + asmOpName)
													}
												}
												case _ =>
											}
										}
										case _ => throwError("AbcQNameA is expecting an abcNamespace as it's second argument into " + asmOpName)
									}
								}
								case _ => throwError("AbcQNameA is expecting a string as it's first argument into " + asmOpName)
							}
						}
						case AbcQName('AbcQName, AsmNamespace) => {
							expectNextOp("AbcQName is expecting a string as it's first argument into " + asmOpName) match {
								case ps@PushString(psValue) => {
									removes = ps :: removes
									expectNextOp("AbcQName is expecting an abcNamespace as it's second argument into " + asmOpName) match {
										case fp2@FindPropStrict(fpAbcNamespace) if fpAbcNamespace == abcNamespace => {
											removes = fp2 :: removes
											readABCNamespace("AbcQName is expecting an AbcNamespace as it's second argument into " + asmOpName) match {
												case Some(ns) => {
													ret = Some(AbcQName(psValue, ns))
													expectNextOp("invalid call to AbcQName into " + asmOpName) match {
														case cp@CallProperty(aName, 2) if aName == abcQName => removes = cp :: removes
														case _ => throwError("invalid call to AbcQName into " + asmOpName)
													}
												}
												case _ =>
											}
										}
										case _ => throwError("AbcQName is expecting an abcNamespace as it's second argument into " + asmOpName)
									}
								}
								case _ => throwError("AbcQName is expecting a string as it's first argument into " + asmOpName)
							}
						}
						case AbcQName('__as3, AsmNamespace) => {
							var ops = readUntil(__as3)
							ops.headOption match {
								case Some(op) => {
									op match {
										case CallProperty(aName, count) if aName == __as3 => {
											ops = ops.tail
											ops.headOption match {
												case Some(gl: GetLex) => {
													ret = Some(gl.typeName)
													ops = ops.tail
													removes = gl :: removes
												}
												case Some(gp: GetProperty) => {
													ret = Some(gp.property)
													ops = ops.tail
													removes = gp :: removes
												}
												case Some(cp: CallProperty) => {
													ret = Some(cp.property)
													ops = ops.tail
													removes = cp :: removes
												}
												case _ => throwError("expecting getProperty in __as3 call into " + asmOpName)
											}
										}
										case _ => throwError("invalid call to __as3 into " + asmOpName)
									}
									removes = op :: removes
								}
								case _ => throwError("missing arguments from __as3 into " + asmOpName)
							}
							ops.map(op => removes = op :: removes)
						}
						case AbcQName('__cint, AsmNamespace) => {
							var ops = readUntil(__cint)
							ops.headOption match {
								case Some(op) => {
									op match {
										case CallProperty(aName, count) if aName == __cint => {
											ops = ops.tail
											ops.headOption match {
												case Some(gl: GetLex) => {
													ret = Some(gl.typeName)
													ops = ops.tail
													removes = gl :: removes
												}
												case Some(gp: GetProperty) => {
													ret = Some(gp.property)
													ops = ops.tail
													removes = gp :: removes
												}
												case _ => throwError("expecting getProperty in __cint call into " + asmOpName)
											}
										}
										case _ => throwError("invalid call to __cint into " + asmOpName)
									}
									removes = op :: removes
								}
								case _ => throwError("missing arguments from __cint into " + asmOpName)
							}
							ops.map(op => removes = op :: removes)
						}
						case _ => throwError("Can't decode/find abcName into " + asmOpName)
					}
					removes = fp :: removes
				}
				case _ => throwError("invalid call to " + asmOpName)
			}
			ret
		}

		@inline def readOp_Long(opName: Symbol, abcName: AbcName, opFactory: (Long) => AbstractOp) {
			expectNextOp(opName + " expect an unsigned integer as parameter") match {
				case pb@PushByte(value) => {
					expectNextOp("invalid call to " + opName) match {
						case cp@CallProperty(aName, 1) if aName == abcName => {
							removes = pb :: removes
							replacements = replacements.updated(cp, List(opFactory(value.toLong)))
						}
						case _ => throwError("invalid call to " + opName)
					}
				}
				case ps@PushShort(value) => {
					expectNextOp("invalid call to " + opName) match {
						case cp@CallProperty(aName, 1) if aName == abcName => {
							removes = ps :: removes
							replacements = replacements.updated(cp, List(opFactory(value.toLong)))
						}
						case _ => throwError("invalid call to " + opName)
					}
				}
				case pi@PushInt(value) => {
					expectNextOp("invalid call to " + opName) match {
						case cp@CallProperty(aName, 1) if aName == abcName => {
							removes = pi :: removes
							replacements = replacements.updated(cp, List(opFactory(value.toLong)))
						}
						case _ => throwError("invalid call to " + opName)
					}
				}
				case pu@PushUInt(value) => {
					expectNextOp("invalid call to " + opName) match {
						case cp@CallProperty(aName, 1) if aName == abcName => {
							removes = pu :: removes
							replacements = replacements.updated(cp, List(opFactory(value)))
						}
						case _ => throwError("invalid call to " + opName)
					}
				}
				case pd@PushDouble(value) => {
					expectNextOp("invalid call to " + opName) match {
						case cp@CallProperty(aName, 1) if aName == abcName => {
							removes = pd :: removes
							replacements = replacements.updated(cp, List(opFactory(value.toLong)))
						}
						case _ => throwError("invalid call to " + opName)
					}
				}
				case _ => throwError(opName + " expect an unsigned integer as parameter")
			}
		}
		@inline def decode_String(opName: String): String = {
			expectNextOp(opName + " expect a string as parameter") match {
				case ps@PushString(str) => {
					removes = ps :: removes
					str.toString().tail.toString
				}
				case _ => throwError(opName + " expect a string as parameter"); ""
			}
		}
		@inline def readOp_Double(opName: Symbol, abcName: AbcName, opFactory: (Double) => AbstractOp) {
			expectNextOp(opName + " expect a number as parameter") match {
				case pb@PushByte(value) => {
					expectNextOp("invalid call to " + opName) match {
						case cp@CallProperty(aName, 1) if aName == abcName => {
							removes = pb :: removes
							replacements = replacements.updated(cp, List(opFactory(value.toDouble)))
						}
						case _ => throwError("invalid call to " + opName)
					}
				}
				case ps@PushShort(value) => {
					expectNextOp("invalid call to " + opName) match {
						case cp@CallProperty(aName, 1) if aName == abcName => {
							removes = ps :: removes
							replacements = replacements.updated(cp, List(opFactory(value.toDouble)))
						}
						case _ => throwError("invalid call to " + opName)
					}
				}
				case pi@PushInt(value) => {
					expectNextOp("invalid call to " + opName) match {
						case cp@CallProperty(aName, 1) if aName == abcName => {
							removes = pi :: removes
							replacements = replacements.updated(cp, List(opFactory(value.toDouble)))
						}
						case _ => throwError("invalid call to " + opName)
					}
				}
				case pu@PushUInt(value) => {
					expectNextOp("invalid call to " + opName) match {
						case cp@CallProperty(aName, 1) if aName == abcName => {
							removes = pu :: removes
							replacements = replacements.updated(cp, List(opFactory(value.toDouble)))
						}
						case _ => throwError("invalid call to " + opName)
					}
				}
				case pd@PushDouble(value) => {
					expectNextOp("invalid call to " + opName) match {
						case cp@CallProperty(aName, 1) if aName == abcName => {
							removes = pd :: removes
							replacements = replacements.updated(cp, List(opFactory(value)))
						}
						case _ => throwError("invalid call to " + opName)
					}
				}
				case _ => throwError(opName + " expect a number as parameter")
			}
		}

		@inline def decode_Long(opName: String): Long = {
			expectNextOp(opName + " expect an unsigned integer as parameter") match {
				case pb@PushByte(value) => {
					removes = pb :: removes
					value.toLong
				}
				case ps@PushShort(value) => {
					removes = ps :: removes
					value.toLong
				}
				case pi@PushInt(value) => {
					removes = pi :: removes
					value.toLong
				}
				case pu@PushUInt(value) => {
					removes = pu :: removes
					value
				}
				case pd@PushDouble(value) => {
					removes = pd :: removes
					value.toLong
				}
				case _ => throwError(opName + " expect an unsigned integer as parameter"); 0L
			}
		}

		def asm(callOp: AbstractOp, numArguments: Int) = {
			if(numArguments == 0 || stack.isEmpty)
				false
			else {
				def loop() {
					val currentOp = stack.head
					stack = stack.tail
					currentOp match {
						case GetLocal(0) =>
						case DebugLine(line) => lineNum = line
						case PushString(value) => {
							if(value.toString().last != ':') {
								throwError("label " + value + " have to ended with :")
							} else if(markerMap.contains(value))
								throwError("duplicate label " + value)
							else {
								val label = Label()
								if(unresolveMarkerMap.contains(value)) {
									val marker = unresolveMarkerMap(value)
									unresolveMarkerMap -= value
									markers.forwardMarker(marker.op.get, label)
								}
								markerMap = markerMap.updated(value, markers.mark(label))
								replacements = replacements.updated(currentOp, List(label))
							}
						}
						case gl@GetLex(typeName) => typeName match {
							case AbcQName(asmOpName, AsmNamespace) => {
								asmOpName match {
									case 'Add => {
										replacements = replacements.updated(gl, List(Add()))
									}
									case 'AddInt => {
										replacements = replacements.updated(gl, List(AddInt()))
									}
									case 'BitAnd => {
										replacements = replacements.updated(gl, List(BitAnd()))
									}
									case 'BitOr => {
										replacements = replacements.updated(gl, List(BitOr()))
									}
									case 'BitNot => {
										replacements = replacements.updated(gl, List(BitNot()))
									}
									case 'BitXor => {
										replacements = replacements.updated(gl, List(BitXor()))
									}
									case 'Breakpoint => {
										replacements = replacements.updated(gl, List(Breakpoint()))
									}
									case 'BreakpointLine => {
										replacements = replacements.updated(gl, List(BreakpointLine()))
									}
									case 'CheckFilter => {
										replacements = replacements.updated(gl, List(CheckFilter()))
									}
									case 'CoerceAny => {
										replacements = replacements.updated(gl, List(CoerceAny()))
									}
									case 'CoerceBoolean => {
										replacements = replacements.updated(gl, List(CoerceBoolean()))
									}
									case 'CoerceDouble => {
										replacements = replacements.updated(gl, List(CoerceDouble()))
									}
									case 'CoerceInt => {
										replacements = replacements.updated(gl, List(CoerceInt()))
									}
									case 'CoerceObject => {
										replacements = replacements.updated(gl, List(CoerceObject()))
									}
									case 'CoerceString => {
										replacements = replacements.updated(gl, List(CoerceString()))
									}
									case 'CoerceUInt => {
										replacements = replacements.updated(gl, List(CoerceUInt()))
									}
									case 'ConvertBoolean => {
										replacements = replacements.updated(gl, List(ConvertBoolean()))
									}
									case 'ConvertInt => {
										replacements = replacements.updated(gl, List(ConvertInt()))
									}
									case 'ConvertDouble => {
										replacements = replacements.updated(gl, List(ConvertDouble()))
									}
									case 'ConvertObject => {
										replacements = replacements.updated(gl, List(ConvertObject()))
									}
									case 'ConvertString => {
										replacements = replacements.updated(gl, List(ConvertString()))
									}
									case 'ConvertUInt => {
										replacements = replacements.updated(gl, List(ConvertUInt()))
									}
									case 'Decrement => {
										replacements = replacements.updated(gl, List(Decrement()))
									}
									case 'DecrementInt => {
										replacements = replacements.updated(gl, List(DecrementInt()))
									}
									case 'Divide => {
										replacements = replacements.updated(gl, List(Divide()))
									}
									case 'Dup => {
										replacements = replacements.updated(gl, List(Dup()))
									}
									case 'DefaultXMLNamespaceLate => {
										replacements = replacements.updated(gl, List(DefaultXMLNamespaceLate()))
									}
									case 'Equals => {
										replacements = replacements.updated(gl, List(Equals()))
									}
									case 'EscapeXMLAttribute => {
										replacements = replacements.updated(gl, List(EscapeXMLAttribute()))
									}
									case 'EscapeXMLElement => {
										replacements = replacements.updated(gl, List(EscapeXMLElement()))
									}
									case 'GetGlobalScope => {
										replacements = replacements.updated(gl, List(GetGlobalScope()))
									}
									case 'GreaterEquals => {
										replacements = replacements.updated(gl, List(GreaterEquals()))
									}
									case 'GreaterThan => {
										replacements = replacements.updated(gl, List(GreaterThan()))
									}
									case 'HasNext => {
										replacements = replacements.updated(gl, List(HasNext()))
									}
									case 'In => {
										replacements = replacements.updated(gl, List(In()))
									}
									case 'Increment => {
										replacements = replacements.updated(gl, List(Increment()))
									}
									case 'IncrementInt => {
										replacements = replacements.updated(gl, List(IncrementInt()))
									}
									case 'InstanceOf => {
										replacements = replacements.updated(gl, List(InstanceOf()))
									}
									case 'IsTypeLate => {
										replacements = replacements.updated(gl, List(IsTypeLate()))
									}
									case 'Label => {
										replacements = replacements.updated(gl, List(Label()))
									}
									case 'LessEquals => {
										replacements = replacements.updated(gl, List(LessEquals()))
									}
									case 'LessThan => {
										replacements = replacements.updated(gl, List(LessThan()))
									}
									case 'ShiftLeft => {
										replacements = replacements.updated(gl, List(ShiftLeft()))
									}
									case 'Modulo => {
										replacements = replacements.updated(gl, List(Modulo()))
									}
									case 'Multiply => {
										replacements = replacements.updated(gl, List(Multiply()))
									}
									case 'MultiplyInt => {
										replacements = replacements.updated(gl, List(MultiplyInt()))
									}
									case 'Negate => {
										replacements = replacements.updated(gl, List(Negate()))
									}
									case 'NegateInt => {
										replacements = replacements.updated(gl, List(NegateInt()))
									}
									case 'NewActivation => {
										replacements = replacements.updated(gl, List(NewActivation()))
									}
									case 'NextName => {
										replacements = replacements.updated(gl, List(NextName()))
									}
									case 'NextValue => {
										replacements = replacements.updated(gl, List(NextValue()))
									}
									case 'Nop => {
										replacements = replacements.updated(gl, List(Nop()))
									}
									case 'Not => {
										replacements = replacements.updated(gl, List(Not()))
									}
									case 'Pop => {
										replacements = replacements.updated(gl, List(Pop()))
									}
									case 'PopScope => {
										replacements = replacements.updated(gl, List(PopScope()))
									}
									case 'PushFalse => {
										replacements = replacements.updated(gl, List(PushFalse()))
									}
									case 'PushNaN => {
										replacements = replacements.updated(gl, List(PushNaN()))
									}
									case 'PushNull => {
										replacements = replacements.updated(gl, List(PushNull()))
									}
									case 'PushScope => {
										replacements = replacements.updated(gl, List(PushScope()))
									}
									case 'PushTrue => {
										replacements = replacements.updated(gl, List(PushTrue()))
									}
									case 'PushUndefined => {
										replacements = replacements.updated(gl, List(PushUndefined()))
									}
									case 'PushWith => {
										replacements = replacements.updated(gl, List(PushWith()))
									}
									case 'ReturnValue => {
										replacements = replacements.updated(gl, List(ReturnValue()))
									}
									case 'ReturnVoid => {
										replacements = replacements.updated(gl, List(ReturnVoid()))
									}
									case 'ShiftRight => {
										replacements = replacements.updated(gl, List(ShiftRight()))
									}
									case 'StrictEquals => {
										replacements = replacements.updated(gl, List(StrictEquals()))
									}
									case 'Subtract => {
										replacements = replacements.updated(gl, List(Subtract()))
									}
									case 'SubtractInt => {
										replacements = replacements.updated(gl, List(SubtractInt()))
									}
									case 'Swap => {
										replacements = replacements.updated(gl, List(Swap()))
									}
									case 'Throw => {
										replacements = replacements.updated(gl, List(Throw()))
									}
									case 'TypeOf => {
										replacements = replacements.updated(gl, List(TypeOf()))
									}
									case 'ShiftRightUnsigned => {
										replacements = replacements.updated(gl, List(ShiftRightUnsigned()))
									}
									case 'SetByte => {
										replacements = replacements.updated(gl, List(SetByte()))
									}
									case 'SetShort => {
										replacements = replacements.updated(gl, List(SetShort()))
									}
									case 'SetInt => {
										replacements = replacements.updated(gl, List(SetInt()))
									}
									case 'SetFloat => {
										replacements = replacements.updated(gl, List(SetFloat()))
									}
									case 'SetDouble => {
										replacements = replacements.updated(gl, List(SetDouble()))
									}
									case 'GetByte => {
										replacements = replacements.updated(gl, List(GetByte()))
									}
									case 'GetShort => {
										replacements = replacements.updated(gl, List(GetShort()))
									}
									case 'GetInt => {
										replacements = replacements.updated(gl, List(GetInt()))
									}
									case 'GetFloat => {
										replacements = replacements.updated(gl, List(GetFloat()))
									}
									case 'GetDouble => {
										replacements = replacements.updated(gl, List(GetDouble()))
									}
									case 'Sign1 => {
										replacements = replacements.updated(gl, List(Sign1()))
									}
									case 'Sign8 => {
										replacements = replacements.updated(gl, List(Sign8()))
									}
									case 'Sign16 => {
										replacements = replacements.updated(gl, List(Sign16()))
									}
									case 'GetLocal0 => {
										replacements = replacements.updated(gl, List(GetLocal(0)))
									}
									case 'GetLocal1 => {
										replacements = replacements.updated(gl, List(GetLocal(1)))
									}
									case 'GetLocal2 => {
										replacements = replacements.updated(gl, List(GetLocal(2)))
									}
									case 'GetLocal3 => {
										replacements = replacements.updated(gl, List(GetLocal(3)))
									}
									case 'SetLocal0 => {
										replacements = replacements.updated(gl, List(SetLocal(0)))
									}
									case 'SetLocal1 => {
										replacements = replacements.updated(gl, List(SetLocal(1)))
									}
									case 'SetLocal2 => {
										replacements = replacements.updated(gl, List(SetLocal(2)))
									}
									case 'SetLocal3 => {
										replacements = replacements.updated(gl, List(SetLocal(3)))
									}
									case _ => throwError("unknown op : " + asmOpName)
								}
							}
							case _ =>
						}
						case FindPropStrict(typeName) => typeName match {
							case AbcQName(asmOpName, AsmNamespace) => {
								asmOpName match {
									case 'CallProperty => {
										readOp_AbcName_Int(asmOpName, callProperty, (abcName: AbcName, args: Int) => CallProperty(abcName, args))
										removes = currentOp :: removes
									}
									//									case 'CallMethod => {
									//										readOp_Int_Int(asmOpName, callMethod, (index: Int, args: Int) => CallMethod(index, args))
									//										removes = currentOp :: removes
									//									}
									//									case 'CallStatic => {
									//										readOp_Int_Int(asmOpName, callStatic, (index: Int, args: Int) => CallStatic(index, args))
									//										removes = currentOp :: removes
									//									}
									case 'CallPropLex => {
										readOp_AbcName_Int(asmOpName, callPropLex, (abcName: AbcName, args: Int) => CallPropLex(abcName, args))
										removes = currentOp :: removes
									}
									case 'CallPropVoid => {
										readOp_AbcName_Int(asmOpName, callPropVoid, (abcName: AbcName, args: Int) => CallPropVoid(abcName, args))
										removes = currentOp :: removes
									}
									case 'CallSuperVoid => {
										readOp_AbcName_Int(asmOpName, callSuperVoid, (abcName: AbcName, args: Int) => CallSuperVoid(abcName, args))
										removes = currentOp :: removes
									}
									case 'ConstructProp => {
										readOp_AbcName_Int(asmOpName, constructProp, (abcName: AbcName, args: Int) => ConstructProp(abcName, args))
										removes = currentOp :: removes
									}
									case 'CallSuper => {
										readOp_AbcName_Int(asmOpName, callSuper, (abcName: AbcName, args: Int) => CallSuper(abcName, args))
										removes = currentOp :: removes
									}
									case 'AsType => {
										readOp_AbcName(asmOpName, asType, (abcName: AbcName) => AsType(abcName))
										removes = currentOp :: removes
									}
									case 'Coerce => {
										readOp_AbcName(asmOpName, coerce, (abcName: AbcName) => Coerce(abcName))
										removes = currentOp :: removes
									}
									case 'GetLex => {
										readOp_AbcName(asmOpName, getLex, (abcName: AbcName) => GetLex(abcName))
										removes = currentOp :: removes
									}
									case 'IsType => {
										readOp_AbcName(asmOpName, isType, (abcName: AbcName) => IsType(abcName))
										removes = currentOp :: removes
									}
									case 'DeleteProperty => {
										readOp_AbcName(asmOpName, deleteProperty, (abcName: AbcName) => DeleteProperty(abcName))
										removes = currentOp :: removes
									}
									case 'FindDef => {
										readOp_AbcName(asmOpName, findDef, (abcName: AbcName) => FindDef(abcName))
										removes = currentOp :: removes
									}
									case 'FindProperty => {
										readOp_AbcName(asmOpName, findProperty, (abcName: AbcName) => FindProperty(abcName))
										removes = currentOp :: removes
									}
									case 'FindPropStrict => {
										readOp_AbcName(asmOpName, findPropStrict, (abcName: AbcName) => FindPropStrict(abcName))
										removes = currentOp :: removes
									}
									case 'GetDescendants => {
										readOp_AbcName(asmOpName, getDescendants, (abcName: AbcName) => GetDescendants(abcName))
										removes = currentOp :: removes
									}
									case 'GetProperty => {
										readOp_AbcName(asmOpName, getProperty, (abcName: AbcName) => GetProperty(abcName))
										removes = currentOp :: removes
									}
									case 'GetSuper => {
										readOp_AbcName(asmOpName, getSuper, (abcName: AbcName) => GetSuper(abcName))
										removes = currentOp :: removes
									}
									case 'InitProperty => {
										readOp_AbcName(asmOpName, initProperty, (abcName: AbcName) => InitProperty(abcName))
										removes = currentOp :: removes
									}
									case 'SetProperty => {
										readOp_AbcName(asmOpName, setProperty, (abcName: AbcName) => SetProperty(abcName))
										removes = currentOp :: removes
									}
									case 'SetSuper => {
										readOp_AbcName(asmOpName, setSuper, (abcName: AbcName) => SetSuper(abcName))
										removes = currentOp :: removes
									}
									case 'GetLocal => {
										readOp_Register(asmOpName, getLocal, (register: Int) => GetLocal(register))
										removes = currentOp :: removes
									}
									case 'DebugFile => {
										readOp_String(asmOpName, debugFile, (file: Symbol) => DebugFile(file))
										removes = currentOp :: removes
									}
									case 'PushString => {
										readOp_String(asmOpName, pushString, (str: Symbol) => PushString(str))
										removes = currentOp :: removes
									}
									case 'DefaultXMLNamespace => {
										readOp_String(asmOpName, defaultXMLNamespace, (uri: Symbol) => DefaultXMLNamespace(uri))
										removes = currentOp :: removes
									}
									case 'SetLocal => {
										readOp_Register(asmOpName, setLocal, (register: Int) => SetLocal(register))
										removes = currentOp :: removes
									}
									case 'ApplyType => {
										readOp_Int(asmOpName, applyType, (i: Int) => ApplyType(i))
										removes = currentOp :: removes
									}
									case 'Call => {
										readOp_Int(asmOpName, call, (i: Int) => Call(i))
										removes = currentOp :: removes
									}
									case 'Construct => {
										readOp_Int(asmOpName, construct, (i: Int) => Construct(i))
										removes = currentOp :: removes
									}
									case 'ConstructSuper => {
										readOp_Int(asmOpName, constructSuper, (i: Int) => ConstructSuper(i))
										removes = currentOp :: removes
									}
									case 'DebugLine => {
										readOp_Int(asmOpName, debugLine, (i: Int) => DebugLine(i))
										removes = currentOp :: removes
									}
									case 'DecLocal => {
										readOp_Register(asmOpName, decLocal, (register: Int) => DecLocal(register))
										removes = currentOp :: removes
									}
									case 'DecLocalInt => {
										readOp_Register(asmOpName, decLocalInt, (register: Int) => DecLocalInt(register))
										removes = currentOp :: removes
									}
									case 'GetGlobalSlot => {
										readOp_Int(asmOpName, getGlobalSlot, (i: Int) => GetGlobalSlot(i))
										removes = currentOp :: removes
									}
									case 'GetScopeObject => {
										readOp_Int(asmOpName, getScopeObject, (i: Int) => GetScopeObject(i))
										removes = currentOp :: removes
									}
									case 'GetSlot => {
										readOp_Int(asmOpName, getSlot, (i: Int) => GetSlot(i))
										removes = currentOp :: removes
									}
									case 'IncLocal => {
										readOp_Register(asmOpName, incLocal, (register: Int) => IncLocal(register))
										removes = currentOp :: removes
									}
									case 'IncLocalInt => {
										readOp_Register(asmOpName, incLocalInt, (register: Int) => IncLocalInt(register))
										removes = currentOp :: removes
									}
									case 'Kill => {
										readOp_Register(asmOpName, kill, (register: Int) => Kill(register))
										removes = currentOp :: removes
									}
									case 'NewArray => {
										readOp_Int(asmOpName, newArray, (i: Int) => NewArray(i))
										removes = currentOp :: removes
									}
									case 'NewObject => {
										readOp_Int(asmOpName, newObject, (i: Int) => NewObject(i))
										removes = currentOp :: removes
									}
									case 'SetGlobalSlot => {
										readOp_Int(asmOpName, setGlobalSlot, (i: Int) => SetGlobalSlot(i))
										removes = currentOp :: removes
									}
									case 'SetSlot => {
										readOp_Int(asmOpName, setSlot, (i: Int) => SetSlot(i))
										removes = currentOp :: removes
									}
									case 'PushByte => {
										readOp_Int(asmOpName, pushByte, (i: Int) => PushByte(i))
										removes = currentOp :: removes
									}
									case 'PushInt => {
										readOp_Int(asmOpName, pushInt, (i: Int) => PushInt(i))
										removes = currentOp :: removes
									}
									case 'PushShort => {
										readOp_Int(asmOpName, pushShort, (i: Int) => PushShort(i))
										removes = currentOp :: removes
									}
									case 'PushUInt => {
										readOp_Long(asmOpName, pushUInt, (l: Long) => PushUInt(l))
										removes = currentOp :: removes
									}
									case 'PushDouble => {
										readOp_Double(asmOpName, pushDouble, (d: Double) => PushDouble(d))
										removes = currentOp :: removes
									}
									case 'IfEqual => {
										readOp_Marker(asmOpName, ifEqual, (marker: Marker) => IfEqual(marker))
										removes = currentOp :: removes
									}
									case 'IfFalse => {
										readOp_Marker(asmOpName, ifFalse, (marker: Marker) => IfFalse(marker))
										removes = currentOp :: removes
									}
									case 'IfGreaterEqual => {
										readOp_Marker(asmOpName, ifGreaterEqual, (marker: Marker) => IfGreaterEqual(marker))
										removes = currentOp :: removes
									}
									case 'IfGreaterThan => {
										readOp_Marker(asmOpName, ifGreaterThan, (marker: Marker) => IfGreaterThan(marker))
										removes = currentOp :: removes
									}
									case 'IfLessEqual => {
										readOp_Marker(asmOpName, ifLessEqual, (marker: Marker) => IfLessEqual(marker))
										removes = currentOp :: removes
									}
									case 'IfLessThan => {
										readOp_Marker(asmOpName, ifLessThan, (marker: Marker) => IfLessThan(marker))
										removes = currentOp :: removes
									}
									case 'IfNotGreaterEqual => {
										readOp_Marker(asmOpName, ifNotGreaterEqual, (marker: Marker) => IfNotGreaterEqual(marker))
										removes = currentOp :: removes
									}
									case 'IfNotGreaterThan => {
										readOp_Marker(asmOpName, ifNotGreaterThan, (marker: Marker) => IfNotGreaterThan(marker))
										removes = currentOp :: removes
									}
									case 'IfNotLessEqual => {
										readOp_Marker(asmOpName, ifNotLessEqual, (marker: Marker) => IfNotLessEqual(marker))
										removes = currentOp :: removes
									}
									case 'IfNotLessThan => {
										readOp_Marker(asmOpName, ifNotLessThan, (marker: Marker) => IfNotLessThan(marker))
										removes = currentOp :: removes
									}
									case 'IfNotEqual => {
										readOp_Marker(asmOpName, ifNotEqual, (marker: Marker) => IfNotEqual(marker))
										removes = currentOp :: removes
									}
									case 'IfStrictEqual => {
										readOp_Marker(asmOpName, ifStrictEqual, (marker: Marker) => IfStrictEqual(marker))
										removes = currentOp :: removes
									}
									case 'IfStrictNotEqual => {
										readOp_Marker(asmOpName, ifStrictNotEqual, (marker: Marker) => Jump(marker))
										removes = currentOp :: removes
									}
									case 'IfTrue => {
										readOp_Marker(asmOpName, ifTrue, (marker: Marker) => IfTrue(marker))
										removes = currentOp :: removes
									}
									case 'Jump => {
										readOp_Marker(asmOpName, jump, (marker: Marker) => Jump(marker))
										removes = currentOp :: removes
									}
									case 'LookupSwitch => {
										readOp_Marker_Markers(asmOpName, lookupSwitch, (defaultCase: Marker, cases: Array[Marker]) => LookupSwitch(defaultCase, cases))
										removes = currentOp :: removes
									}
									// TODO
									//									case 'HasNext2 => {
									//										readOp_Register_Register(asmOpName, hasNext2, (register1: Int, register2:Int) => HasNext2(register1, register2))
									//										removes = currentOp :: removes
									//									}
									case '__as3 => {
										val ops = readUntil(__as3)
										ops.headOption match {
											case Some(op) => {
												op match {
													case CallProperty(aName, count) if aName == __as3 => {
														removes = currentOp :: op :: removes
														//														replacements = replacements.updated(op, ops.reverse.dropRight(1))
													}
													case _ => throwError("invalid call to " + asmOpName)
												}
											}
											case _ => throwError("missing arguments to " + asmOpName)
										}
									}
									case '__cint => {
										val ops = readUntil(__cint)
										ops.headOption match {
											case Some(op) => {
												op match {
													case CallProperty(aName, count) if aName == __cint => {
														removes = currentOp :: op :: removes

														for($op <- ops) $op match {
															case Add() => replacements = replacements.updated($op, List(AddInt()))
															case DecLocal(register) => replacements = replacements.updated($op, List(DecLocalInt(register)))
															case Decrement() => replacements = replacements.updated($op, List(DecrementInt()))
															case IncLocal(register) => replacements = replacements.updated($op, List(IncLocalInt(register)))
															case Multiply() => replacements = replacements.updated($op, List(MultiplyInt()))
															case Negate() => replacements = replacements.updated($op, List(NegateInt()))
															case Subtract() => replacements = replacements.updated($op, List(SubtractInt()))
															case _ =>
														}

														//														replacements = replacements.updated(op, ops.reverse.dropRight(1))
													}
													case _ => throwError("invalid call to " + asmOpName)
												}
											}
											case _ => throwError("missing arguments to " + asmOpName)
										}
									}
									case _ => throwError("unknown op : " + asmOpName)
								}
							}
							case _ => throwError("unexpected : findPropStrict(" + typeName + ")")
						}
						case _ =>
					}
					if(stack.nonEmpty)
						loop()
				}
				loop()
				true
			}
		}

		def independentCall(callOp: AbstractOp, numArguments: Int) {
			if(numArguments != 0 && stack.nonEmpty) {
				val currentOp = callOp
				currentOp match {
					case CallProperty(aName, count) if aName == __cint => {
						@tailrec def loop() {
							if(stack.nonEmpty) {
								val $op = stack.head
								stack = stack.tail
								$op match {
									case Add() => replacements = replacements.updated($op, List(AddInt()))
									case DecLocal(register) => replacements = replacements.updated($op, List(DecLocalInt(register)))
									case Decrement() => replacements = replacements.updated($op, List(DecrementInt()))
									case IncLocal(register) => replacements = replacements.updated($op, List(IncLocalInt(register)))
									case Multiply() => replacements = replacements.updated($op, List(MultiplyInt()))
									case Negate() => replacements = replacements.updated($op, List(NegateInt()))
									case Subtract() => replacements = replacements.updated($op, List(SubtractInt()))
									case _ =>
								}
								loop()
							}
						}
						loop()
					}
					case CallPropVoid(aName, count) if aName == __cint => {
						@tailrec def loop() {
							if(stack.nonEmpty) {
								val $op = stack.head
								stack = stack.tail
								$op match {
									case Add() => replacements = replacements.updated($op, List(AddInt()))
									case DecLocal(register) => replacements = replacements.updated($op, List(DecLocalInt(register)))
									case Decrement() => replacements = replacements.updated($op, List(DecrementInt()))
									case IncLocal(register) => replacements = replacements.updated($op, List(IncLocalInt(register)))
									case Multiply() => replacements = replacements.updated($op, List(MultiplyInt()))
									case Negate() => replacements = replacements.updated($op, List(NegateInt()))
									case Subtract() => replacements = replacements.updated($op, List(SubtractInt()))
									case _ =>
								}
								loop()
							}
						}
						loop()
					}
					case _ => throwError("Unknown call " + callOp)
				}
			}
		}

		var removePop = false
		var removeConvert = false

		var opIndex = -1

		val ops = bytecode.ops

		def removeCastAt(castName: AbcQName, castIndex: Int) {
			if(castIndex < ops.size) {
				ops(castIndex) match {
					case cast@CallProperty(name, 1) if name == castName => {
						removes = cast :: removes
						@tailrec def loop(index: Int) {
							if(index >= 0) {
								ops(index) match {
									case fp@FindPropStrict(aName) if aName == castName => removes = fp :: removes
									case _ => loop(index - 1)
								}
							} else throwError("Malformed cast " + castName)
						}
						loop(castIndex - 1)
					}
					case _ =>
				}
			}
		}

		def queueOpToRemove(op: AbstractOp) {
			if(markers.hasMarkerFor(op)) {
				// forward op to a Nop op that will be removed by the peephole optimizer
				val nop = Nop()
				markers.forwardMarker(op, nop)
				replacements = replacements.updated(op, List(nop))
			} else
				removes = op :: removes
		}

		for(op <- ops) {
			opIndex += 1
			op match {
				case ConvertInt() | CoerceInt() => if(removeConvert) {
					queueOpToRemove(op)
					removePop = false
					removeConvert = false
				}
				case DebugLine(line) => {
					removePop = false
					removeConvert = false
					lineNum = line
				}
				case Pop() if removePop => {
					queueOpToRemove(op)
					removePop = false
					removeConvert = false
				}
				case FindPropStrict(typeName) if typeName == __asm => {
					removePop = false
					removeConvert = false
					balance += 1
					queueOpToRemove(op)
				}
				case FindPropStrict(typeName) if typeName == __cint => {
					removePop = false
					removeConvert = false
					balance += 1
					queueOpToRemove(op)
				}
				case FindPropStrict(typeName) if typeName == __dumpAfterASM => {
					removePop = false
					removeConvert = false
					if(balance > 0)
						throwError("can't call __dumpAfterASM inside __asm, __maxStack, or __dumpAfterASM")

					balance += 1
					queueOpToRemove(op)
				}
				case FindPropStrict(typeName) if typeName == __nakedName => {
					removePop = false
					removeConvert = false
					if(balance > 0)
						throwError("can't call __naked inside __asm, __maxStack, or __dumpAfterASM")

					balance += 1
					queueOpToRemove(op)
				}
				case FindPropStrict(typeName) if typeName == __maxStack => {
					removePop = false
					removeConvert = false
					if(balance > 0)
						throwError("can't call __dumpAfterASM inside __asm, __maxStack, or __dumpAfterASM")

					balance += 1
					queueOpToRemove(op)
				}
				case CallPropVoid(property, numArguments) if (property == __asm) && (balance > 0) => {
					removePop = false
					removeConvert = true
					asm(op, numArguments)
					queueOpToRemove(op)
					balance -= 1
					removeCastAt(intName, opIndex + 1)
				}
				case CallProperty(property, numArguments) if (property == __asm) && (balance > 0) => {
					asm(op, numArguments)
					removePop = true
					removeConvert = true
					queueOpToRemove(op)
					balance -= 1
					removeCastAt(intName, opIndex + 1)
				}
				case CallPropVoid(property, numArguments) if (property == __dumpAfterASM) && (balance > 0) => {
					removePop = false
					removeConvert = false
					dumpAfterASM = Some(decode_String("__dumpAfterASM"))
					queueOpToRemove(op)
					stack.foreach(queueOpToRemove)
					balance -= 1
				}
				case CallProperty(property, numArguments) if (property == __dumpAfterASM) && (balance > 0) => {
					dumpAfterASM = Some(decode_String("__dumpAfterASM"))
					removePop = true
					removeConvert = false
					queueOpToRemove(op)
					stack.foreach(queueOpToRemove)
					balance -= 1
				}
				case CallPropVoid(property, numArguments) if (property == __nakedName) && (balance > 0) => {
					removePop = false
					removeConvert = false
					naked = true
					queueOpToRemove(op)
					stack.foreach(queueOpToRemove)
					balance -= 1
				}
				case CallProperty(property, numArguments) if (property == __nakedName) && (balance > 0) => {
					naked = true
					removePop = true
					removeConvert = false
					queueOpToRemove(op)
					stack.foreach(queueOpToRemove)
					balance -= 1
				}
				case CallPropVoid(property, numArguments) if (property == __maxStack) && (balance > 0) => {
					removePop = false
					removeConvert = false
					maxStack = decode_Long("__asmStack")
					queueOpToRemove(op)
					stack.foreach(queueOpToRemove)
					balance -= 1
				}
				case CallProperty(property, numArguments) if (property == __maxStack) && (balance > 0) => {
					maxStack = decode_Long("__asmStack")
					removePop = true
					removeConvert = false
					queueOpToRemove(op)
					stack.foreach(queueOpToRemove)
					balance -= 1
				}
				case CallPropVoid(property, numArguments) if (property == __cint) && (balance > 0) => {
					removePop = false
					removeConvert = true
					independentCall(op, numArguments)
					queueOpToRemove(op)
					balance -= 1
					removeCastAt(intName, opIndex + 1)
				}
				case CallProperty(property, numArguments) if (property == __cint) && (balance > 0) => {
					removePop = true
					removeConvert = true
					independentCall(op, numArguments)
					queueOpToRemove(op)
					balance -= 1
					removeCastAt(intName, opIndex + 1)
				}
				case _ => {
					removePop = false
					removeConvert = false
					if(balance > 0) stack = stack ::: List(op)
				}
			}
		}

		modified = removes.nonEmpty || replacements.nonEmpty || (maxStack > 0)

		if(modified) {
			if(unresolveMarkerMap.nonEmpty) {
        bytecode.dump()
				sys.error("can't resolve label :" + unresolveMarkerMap.map(p => p._1).mkString(", "))
			}
			if(naked) {
				if(ops.size >= 2) {
					ops(0) match {
						case GetLocal(0) => removes = ops(0) :: removes
						case _ =>
					}
					ops(1) match {
						case PushScope() => removes = ops(1) :: removes
						case _ =>
					}
				}
			}

			removes foreach {bytecode.remove}
			replacements.iterator foreach {x => bytecode.replace(x._1, x._2)}

			val newOps = bytecode.ops
			@tailrec def getNextOp(index: Int): AbstractOp = {
				newOps(index) match {
					case DebugFile(x) => getNextOp(index + 1)
					case DebugLine(x) => getNextOp(index + 1)
					case op@_ => op
				}
			}

			removes = List.empty[AbstractOp]
			replacements = Map.empty[AbstractOp, List[AbstractOp]]
			for(marker <- isBackwardMarker.filter(mb => !mb._2).map(p=>p._1).toList.reverse) {
				val markedOp = marker.op.get
				val nextOp = getNextOp(newOps.indexOf(markedOp) + 1)
				if (markers.hasMarkerFor(nextOp)){
					val nop=Nop()
					markers.forwardMarker(markedOp, nop)
					replacements = replacements.updated(markedOp, List(nop))
				} else {
					markers.forwardMarker(markedOp, nextOp)
					removes = markedOp :: removes
				}
			}

			removes foreach {bytecode.remove}
			replacements.iterator foreach {x => bytecode.replace(x._1, x._2)}
		}

		removes = List.empty[AbstractOp]
		replacements = Map.empty[AbstractOp, List[AbstractOp]]
		stack = List.empty[AbstractOp]

		removePop = false
		opIndex = -1

		var repeatStack = List.empty[(Int, Int)]

		@tailrec def do_repeat(count: Int, toBeRepeated: SeqView[AbstractOp, List[AbstractOp]], repeatedOps: List[AbstractOp]): List[AbstractOp] = {
			if(count <= 0) repeatedOps
			else {
				var markerCopies = Map.empty[Marker, Marker]

				var newOps: List[AbstractOp] = toBeRepeated.map {
          case op: LookupSwitch => {
            val newDefaultMarker = markerCopies.getOrElse(op.defaultCase, markers.mark(Nop()))
            markerCopies = markerCopies.updated(op.defaultCase, newDefaultMarker)
            val newCases = op.cases.map(m => {
              val newMarker = markerCopies.getOrElse(m, markers.mark(Nop()))
              markerCopies = markerCopies.updated(m, newMarker)
              newMarker
            }
            ).toArray

            val newOp = LookupSwitch(newDefaultMarker, newCases)

            if (markers.hasMarkerFor(op)) {
              val newMarker = markerCopies.getOrElse(markers.mark(op), markers.mark(newOp))
              markers.forwardMarker(newMarker.op.get, newOp)
              markerCopies = markerCopies.updated(markers.mark(op), newMarker)
            }

            newOp
          }
          case op: Jump => {
            val newMarker = markerCopies.getOrElse(op.marker, markers.mark(Nop()))
            markerCopies = markerCopies.updated(op.marker, newMarker)

            val newOp = Jump(newMarker)

            if (markers.hasMarkerFor(op)) {
              val newMarker = markerCopies.getOrElse(markers.mark(op), markers.mark(newOp))
              markers.forwardMarker(newMarker.op.get, newOp)
              markerCopies = markerCopies.updated(markers.mark(op), newMarker)
            }

            newOp
          }
          case op: AbstractConditionalOp => {
            val newMarker = markerCopies.getOrElse(op.marker, markers.mark(Nop()))
            markerCopies = markerCopies.updated(op.marker, newMarker)

            val newOp = Op.copyConditionalOp(op, newMarker)

            if (markers.hasMarkerFor(op)) {
              val newMarker = markerCopies.getOrElse(markers.mark(op), markers.mark(newOp))
              markers.forwardMarker(newMarker.op.get, newOp)
              markerCopies = markerCopies.updated(markers.mark(op), newMarker)
            }

            newOp
          }
          case op:AbstractOp => {
            val newOp = op.opCopy()
            if (markers.hasMarkerFor(op)) {
              val newMarker = markerCopies.getOrElse(markers.mark(op), markers.mark(newOp))
              markers.forwardMarker(newMarker.op.get, newOp)
              markerCopies = markerCopies.updated(markers.mark(op), newMarker)
            }
            newOp
          }
        }.toList
				val nops = markerCopies.filter(_._2.op.get.opCode == Op.nop).map(x => x._2)
				if(nops.nonEmpty) {
					val nop = Nop()
					newOps = newOps ::: List(nop)
					nops.foreach(m => markers.forwardMarker(m.op.get, nop))
				}
				do_repeat(count - 1, toBeRepeated, repeatedOps ::: newOps)
			}
		}

		for(op <- ops) {
			opIndex += 1

			op match {
				case DebugLine(line) => {
					removePop = false
					lineNum = line
				}
				case Pop() if removePop => {
					queueOpToRemove(op)
					removePop = false
				}
				case FindPropStrict(typeName) if typeName == __beginRepeat => {
					stack = List.empty[AbstractOp]
					removePop = false
					balance += 1
					queueOpToRemove(op)
				}
				case FindPropStrict(typeName) if typeName == __endRepeat => {
					removePop = false
					balance += 1
					queueOpToRemove(op)
				}
				case CallPropVoid(property, 1) if (property == __beginRepeat) && (balance > 0) => {
					removePop = false
					repeatStack = (decode_Long("__beginRepeat").intValue, opIndex + 1) :: repeatStack
					queueOpToRemove(op)
				}
				case CallProperty(property, 1) if (property == __beginRepeat) && (balance > 0) => {
					repeatStack = (decode_Long("__beginRepeat").intValue, opIndex + 2) :: repeatStack
					removePop = true
					queueOpToRemove(op)
				}
				case CallPropVoid(property, 0) if (property == __endRepeat) && (balance > 0) => {
					removePop = false
					val (count, fromOp) = repeatStack.head
					repeatStack = repeatStack.tail
					replacements = replacements.updated(op, do_repeat(count - 1, ops.view(fromOp, opIndex - 1), Nil))
					balance -= 1
					stack = List.empty[AbstractOp]
				}
				case CallProperty(property, 0) if (property == __endRepeat) && (balance > 0) => {
					removePop = true
					val (count, fromOp) = repeatStack.head
					repeatStack = repeatStack.tail
					replacements = replacements.updated(op, do_repeat(count - 1, ops.view(fromOp, opIndex - 1), Nil))
					balance -= 1
					stack = List.empty[AbstractOp]
				}
				case _ => {
					removePop = false
					if(balance > 0) stack = stack ::: List(op)
				}
			}
		}

		modified ||= (removes.nonEmpty || replacements.nonEmpty)

		removes foreach {bytecode.remove}
		replacements.iterator foreach {x => bytecode.replace(x._1, x._2)}

		if(modified) {
			bytecode.body match {
				case Some(body) => {
					val (operandStack, scopeStack) = StackAnalysis(bytecode)
					//					body.localCount = localCount
					if(maxStack > 0) {
						if(maxStack < operandStack)
							throwError("__maxStack is too low for your method min expected : " + operandStack)
					} else {
						maxStack = operandStack
					}
					body.maxStack = operandStack
					body.maxScopeDepth = body.initScopeDepth + scopeStack

				}
				case None => ApparatLog warn "Bytecode body missing. Cannot adjust stack/locals."
			}

		}

		dumpAfterASM match {
			case Some(msg) => {
				ApparatLog.info(msg)
				bytecode.dump()
			}
			case _ =>
		}

		modified
	}
}
