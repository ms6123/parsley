/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley

import scala.util.Random

import parsley.quick.{digit, eof, optional}

class StackSafetyTests extends ParsleyTest {
    private val hugeNumber = {
        val builder = new StringBuilder
        for (_ <- 0 until 20000) {
            builder.append(Random.nextInt(10))
        }
        builder.toString
    }

    "parsers" should "never StackOverflow" in {
        lazy val p: Parsley[Unit] = digit ~> optional(p) ~> eof

        p.parse(hugeNumber) should be(Success(()))
    }

    "monadic parsers" should "never StackOverflow" in {
        lazy val p: Parsley[Unit] = digit.flatMap(_ => optional(p)) ~> eof

        p.parse(hugeNumber) should be(Success(()))
    }
}
