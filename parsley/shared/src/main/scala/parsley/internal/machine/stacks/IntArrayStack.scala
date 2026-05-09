/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.stacks

private [machine] final class IntArrayStack(initialSize: Int = IntArrayStack.DefaultSize) {
    private [this] var array: Array[Int] = new Array(initialSize)
    private [this] var sp = -1

    def push(x: Int): Unit = {
        sp += 1
        if (array.length == sp) {
            grow()
        }
        array(sp) = x
    }
    
    private def grow(): Unit = {
        val newArray: Array[Int] = new Array(sp * 2)
        java.lang.System.arraycopy(array, 0, newArray, 0, sp)
        array = newArray
    }

    def upush(x: Int): Unit = {
        sp += 1
        array(sp) = x
    }

    def exchange(x: Int): Unit = array(sp) = x
    def peekAndExchange(x: Int): Any = {
        val y = array(sp)
        array(sp) = x
        y
    }
    def pop_(): Unit = sp -= 1
    def pop(): Int = {
        val x = array(sp)
        sp -= 1
        x
    }
    def peek: Int = array(sp)

    def drop(x: Int): Unit = sp -= x

    // This is off by one, but that's fine, if everything is also off by one :P
    def usize: Int = sp
    // $COVERAGE-OFF$
    def size: Int = usize + 1
    def isEmpty: Boolean = sp == -1
    def nonEmpty: Boolean = sp != -1
    def mkString(sep: String): String = array.take(sp + 1).reverse.mkString(sep)
    // $COVERAGE-ON$
}
private [machine] object IntArrayStack {
    final val DefaultSize = 8
}
