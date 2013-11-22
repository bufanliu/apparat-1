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
 *
 * @author Patrick Le Clec'h
 */

package apparat.bytecode.optimization

import apparat.bytecode.operations.{OpWithMarker, Nop, AbstractOp}
import scala.annotation.tailrec
import apparat.bytecode.Bytecode

class TransformTool(bytecode: Bytecode) {
  val markers = bytecode.markers
  var parameters = List.empty[AbstractOp]
  var removes = List.empty[AbstractOp]
  var replacements = Map.empty[AbstractOp, List[AbstractOp]]

  @tailrec final def unwindAndSaveParameterStack(depth: Int, ret: AbstractOp = Nop(), saved: List[AbstractOp] = Nil): List[AbstractOp] = {
    if ((depth < 0) && parameters.nonEmpty) {
      val op = parameters.head
      parameters = parameters.tail
      val newSaved = if (markers.hasMarkerFor(op)) {
        val i = parameters.indexWhere {
          case m: OpWithMarker if m.marker.op.get == op => true
          case _ => false
        }
        if (i >= 0) {
          val newSaved = parameters.take(i + 1).reverse ::: saved
          parameters = parameters.drop(i + 1)
          newSaved
        } else saved
      } else saved
      unwindAndSaveParameterStack(depth + op.operandDelta, op, op :: newSaved)
    } else saved
  }

  @tailrec final def unwindParameterStack(depth: Int, ret: AbstractOp = Nop()): AbstractOp = {
    if ((depth < 0) && parameters.nonEmpty) {
      val op = parameters.head
      parameters = parameters.tail
      if (markers.hasMarkerFor(op)) {
        val i = parameters.indexWhere {
          case m: OpWithMarker if m.marker.op.get == op => true
          case _ => false
        }
        if (i >= 0)
          parameters = parameters.drop(i + 1)
      }
      unwindParameterStack(depth + op.operandDelta, op)
    } else ret
  }

  def modified = replacements.nonEmpty || removes.nonEmpty

  private def end() = {
    val m = modified
    if (m) {
      removes foreach {
        bytecode.remove
      }

      replacements.iterator foreach {
        x => bytecode.replace(x._1, x._2)
      }
    }
    m
  }

  @inline def addToParameter(op: AbstractOp = null) {
    if (op == null) parameters = ops.head :: parameters
    else parameters = op :: parameters
  }

  @inline def remove(op: AbstractOp) {
    removes = op :: removes
  }

  @inline def replace(op: AbstractOp, by: AbstractOp) {
    replace(op, List(by))
  }

  @inline def replace(op: AbstractOp, by: List[AbstractOp]) {
    replacements = replacements.updated(op, by)
  }

  def onNext() = ()

  var ops = List.empty[AbstractOp]

  def apply(): Boolean = {
    ops = bytecode.ops
    while (ops != Nil) {
      onNext()
      ops = ops.tail
    }
    end()
  }
}
