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
package apparat.bytecode.optimization

import apparat.bytecode.combinator.BytecodeChains._
import apparat.bytecode.combinator._
import apparat.bytecode.operations._
import apparat.bytecode.Bytecode
import apparat.abc.AbcQName

object PeepholeOptimizations extends (Bytecode => Boolean) {
  /*def apply(bytecode: Bytecode) = {
    bytecode rewrite whileLoop
    bytecode rewrite getLex
    bytecode rewrite unnecessaryIntCast
    bytecode rewrite ifFalse
    bytecode rewrite ifTrue
    bytecode
  }*/

  @inline def replace_getlocal0(macroName: AbcQName, bytecode: Bytecode) {
    /**
     * @author Patrick Le Clec'h
     *         rewrite some call that are made by asc2
     *         Macro function are call with GetLocal(0) instead of GetLex(..)
     *
     */

    val tf_ungetlocal0 = new TransformTool(bytecode) {
      override def onNext() = {
        ops.head match {
          case call@CallPropVoid(aName, n) => {
            unwindParameterStack(-n)
            parameters match {
              case GetLocal(0) :: xs => {
                replace(parameters.head, GetLex(macroName))
                parameters = xs
              }
              case _ =>
            }
          }
          case call@CallProperty(aName, n) => {
            unwindParameterStack(-n)
            parameters match {
              case GetLocal(0) :: xs => {
                replace(parameters.head, GetLex(macroName))
                parameters = xs
                ops.tail match {
                  case Pop() :: xx => {
                    remove(ops.tail.head)
                    ops = ops.tail
                    replace(call, CallPropVoid(aName, n))
                  }
                  case _ => {
                    addToParameter(PushByte(1))
                  }
                }
              }
              case _ =>
            }
          }
          case _ => addToParameter()
        }
      }
    }
    tf_ungetlocal0()
  }

  def replace_getlex_call(bytecode: Bytecode): Boolean = {
    /**
     * @author Patrick Le Clec'h
     *         rewrite some call that are made by asc2 and that are really slow and mess the rest of the transformation step
     *         asc2 rewrite call as GetLex GetGlobalScope ... Call(n)
     *
     */
    val tf = new TransformTool(bytecode) {
      override def onNext() = {
        ops.head match {
          case call@Call(n) => {
            unwindParameterStack(-n)
            parameters match {
              case x :: xs => {
                skipUntil(parameters, Op.getglobalscope) match {
                  case (ggs:GetGlobalScope, rest) => {
                    skipUntil(rest, Op.getlex) match  {
                      case (getLex:GetLex, rest2) => {
                        replace(ggs, Nop())
                        replace(getLex, FindPropStrict(getLex.typeName))
                        parameters = rest2
                        ops.tail match {
                          case Pop() :: xx => {
                            remove(ops.tail.head)
                            ops = ops.tail
                            replace(call, CallPropVoid(getLex.typeName, n))
                          }
                          case _ => {
                            addToParameter(PushByte(1))
                            replace(call, CallProperty(getLex.typeName, n))
                          }
                        }
                      }
                      case _ =>
                    }
                  }
                  case _ =>
                }
              }
              case _ =>
            }
          }
          case _ => addToParameter()
        }
      }

      def skipUntil(_rest: List[AbstractOp], opCode: Int): (AbstractOp, List[AbstractOp]) = {
        var rest: List[AbstractOp] = _rest
        var aop: AbstractOp = null
        var continue = true
        while (continue && (rest != Nil)) {
          rest.head match {
            case op if op.opCode == opCode => {
              aop = op
              rest = rest.tail
              continue = false
            }
            case op if op.operandDelta == 0 => {
              rest = rest.tail
            }
            case _ => continue = false
          }
        }
        (aop, rest)
      }
    }
    tf()
  }

  def apply(bytecode: Bytecode): Boolean = {
    var source = bytecode.ops
    var target = List.empty[AbstractOp]
    val n = source.length
    var modified = false
    val markers = bytecode.markers
    var i = 0

    @inline def nextOp(): Unit = {
      i += 1
      source = source.tail
    }

    while (i < n) {
      val op = source.head
      val opCode = op.opCode
      if (Op.nop == opCode) {
        val tail = source.tail
        if (tail.nonEmpty) {
          if (markers.hasMarkerFor(tail.head)) {
            // TODO fold label
            // so for now we did not remove the Nop
            // if tail.head has already a marker
            // L0:Nop
            // L1:XXX
            target = op :: target
          } else {
            markers.forwardMarker(op, tail.head)
            modified = true
          }
        }
      } /*else if (Op.jump == opCode) {
				val tail = source.tail
				if (tail.nonEmpty){
					op.asInstanceOf[Jump].marker.op match {
						case Some(markedOp) if (markedOp == tail.head) => {
							modified = true
							target = markedOp :: target
						}
						case _ => target = op :: target
					}
				} else {
					target = op :: target
				}
			}*//* else if(Op.pushfalse == opCode) {
				if(source.tail.head.opCode == Op.iffalse) {
					val ifFalse = source.tail.head.asInstanceOf[IfFalse]
					target = Jump(ifFalse.marker) :: target
					markers.forwardMarker(ifFalse, target.head)
					markers.forwardMarker(op, target.head)
					modified = true
					nextOp()
				} else {
					target = op :: target
				}
			} else if(Op.pushtrue == opCode) {
				if(source.tail.head.opCode == Op.iftrue) {
					val ifTrue = source.tail.head.asInstanceOf[IfTrue]
					target = Jump(ifTrue.marker) :: target
					markers.forwardMarker(ifTrue, target.head)
					markers.forwardMarker(op, target.head)
					modified = true
					nextOp()
				} else {
					target = op :: target
				}
			} else if(Op.getglobalscope == opCode) {
				if(source.tail.head.opCode == Op.getslot) {
					val getSlot = source.tail.head.asInstanceOf[GetSlot]
					target = GetGlobalSlot(getSlot.slot) :: target
					markers.forwardMarker(getSlot, target.head)
					markers.forwardMarker(op, target.head)
					modified = true
					nextOp()
				} else {
					target = op :: target
				}
			}*/
      else if (Op.findpropstrict == opCode) {
        if (source.tail.head.opCode == Op.getproperty) {
          val getProperty = source.tail.head.asInstanceOf[GetProperty]

          if (getProperty.property == op.asInstanceOf[FindPropStrict].property) {
            target = GetLex(getProperty.property) :: target
            markers.forwardMarker(op, target.head)
            modified = true
          } else {
            target = getProperty :: op :: target
          }

          nextOp()
        } else {
          target = op :: target
        }
      } /* else if(Op.add_i == opCode ||
				Op.subtract_i == opCode ||
				Op.multiply_i == opCode) {
				if(source.tail.head.opCode == Op.convert_i) {
					modified = true
					nextOp()
				}

				target = op :: target
			} else if(Op.getlocal == opCode ||
				Op.getlocal0 == opCode ||
				Op.getlocal1 == opCode ||
				Op.getlocal2 == opCode ||
				Op.getlocal3 == opCode) {
				var tail = source.tail
				val op2 = tail.head
				if(op2.opCode == Op.increment_i || op2.opCode == Op.decrement_i) {
					nextOp()
					tail = tail.tail
					val op3 = tail.head
					if(op3.opCode == Op.dup) {
						nextOp()
						tail = tail.tail
						val op4 = tail.head
						if(op4.opCode == Op.convert_i) {
							nextOp()
							tail = tail.tail
							val op5 = tail.head
							val opCode5 = op5.opCode
							if(opCode5 == Op.setlocal ||
								opCode5 == Op.setlocal0 ||
								opCode5 == Op.setlocal1 ||
								opCode5 == Op.setlocal2 ||
								opCode5 == Op.setlocal3) {
								nextOp()
								val setLocal = op5.asInstanceOf[SetLocal]
								if(setLocal.register == op.asInstanceOf[GetLocal].register) {
									target = op :: (if(op2.opCode == Op.increment_i) IncLocalInt(setLocal.register) else DecLocalInt(setLocal.register)) :: target
								} else {
									target = op5 :: op4 :: op3 :: op2 :: op :: target
								}
							} else {
								target = op4 :: op3 :: op2 :: op :: target
							}
						} else {
							target = op3 :: op2 :: op :: target
						}
					} else {
						target = op2 :: op :: target
					}
				} else {
					target = op :: target
				}
			}*/
      else {
        target = op :: target
      }

      nextOp()
    }

    if (modified) {
      bytecode.ops = target.reverse
    }

    modified
  }

  lazy val ifFalse = (PushFalse() ~ partial {
    case ifFalse: IfFalse => ifFalse
  }) ^^ {
    case PushFalse() ~ IfFalse(marker) => Jump(marker) :: Nil
    case _ => error("Unreachable code.")
  }

  lazy val ifTrue = (PushTrue() ~ partial {
    case ifTrue: IfTrue => ifTrue
  }) ^^ {
    case PushTrue() ~ IfTrue(marker) => Jump(marker) :: Nil
    case _ => error("Unreachable code.")
  }

  lazy val whileLoop = (
    partial {
      case getLocal: GetLocal => getLocal
    } ~ (IncrementInt() | DecrementInt()) ~ Dup() ~ ConvertInt() ~
      partial {
        case setLocal: SetLocal => setLocal
      }
    ) ^^ {
    case GetLocal(x) ~ op ~ Dup() ~ ConvertInt() ~ SetLocal(y) if x == y => {
      (op match {
        case DecrementInt() => DecLocalInt(x)
        case IncrementInt() => IncLocalInt(x)
        case _ => error("Unreachable code.")
      }) :: GetLocal(x) :: Nil
    }
    case GetLocal(x) ~ op ~ Dup() ~ ConvertInt() ~ SetLocal(y) => {
      GetLocal(x) :: op :: Dup() :: ConvertInt() :: SetLocal(y) :: Nil
    }
    case _ => error("Unreachable code.")
  }

  lazy val getLex = (
    partial {
      case findPropStrict: FindPropStrict => findPropStrict
    } ~
      partial {
        case getProperty: GetProperty => getProperty
      }
    ) ^^ {
    case FindPropStrict(x) ~ GetProperty(y) if x == y => GetLex(x) :: Nil
    case FindPropStrict(x) ~ GetProperty(y) => FindPropStrict(x) :: GetProperty(y) :: Nil
    case _ => error("Unreachable code.")
  }

  lazy val unnecessaryIntCast = ((AddInt() | SubtractInt() | MultiplyInt()) ~ ConvertInt()) ^^ {
    case x ~ ConvertInt() => x :: Nil
    case _ => error("Unreachable code.")
  }
}
