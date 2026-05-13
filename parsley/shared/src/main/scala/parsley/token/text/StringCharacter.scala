/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.text

import parsley.Parsley
import Parsley.{empty, some}
import parsley.character.{char, satisfy}
import parsley.token.descriptions.EscapeDesc
import parsley.token.errors.ErrorConfig
import parsley.token.{Basic, CharPred, NotRequired, Unicode}
import parsley.unicode.{satisfy as satisfyUtf16}

private [token] abstract class StringCharacter {
    def apply(isLetter: CharPred): Parsley[Option[Int]]
    def isRaw: Boolean

    protected def _checkBadChar(err: ErrorConfig) = err.verifiedStringBadCharsUsedInLiteral.checkBadChar
}

private [token] class RawCharacter(err: ErrorConfig) extends StringCharacter {
    override def isRaw: Boolean = true
    override def apply(isLetter: CharPred): Parsley[Option[Int]] = isLetter match {
        case Basic(isLetter) => err.labelStringCharacter(satisfy(isLetter).map(c => Some(c.toInt))) | _checkBadChar(err)
        case Unicode(isLetter) => err.labelStringCharacter(satisfyUtf16(isLetter).map(Some(_))) | _checkBadChar(err)
        case NotRequired => empty
    }
}

private [token] class EscapableCharacter(desc: EscapeDesc, escapes: Escape, space: Parsley[?], err: ErrorConfig) extends StringCharacter {
    override def isRaw: Boolean = false
    private lazy val escapeEmpty = desc.emptyEscape.fold[Parsley[Char]](empty)(c => err.labelStringEscapeEmpty(char(c)))
    private lazy val escapeGap = {
        if (desc.gapsSupported) some(err.labelStringEscapeGap(space)) ~> err.labelStringEscapeGapEnd(char(desc.escBegin))
        else empty
    }
    private lazy val stringEscape: Parsley[Option[Int]] =
        escapes.escapeBegin ~> (escapeGap.as(None)
                             | escapeEmpty.as(None)
                             | escapes.escapeCode.map(Some(_)))

    override def apply(isLetter: CharPred): Parsley[Option[Int]] = {
        val escBegin = desc.escBegin
        isLetter match {
            case Basic(isLetter) => err.labelStringCharacter(
                stringEscape | err.labelGraphicCharacter(satisfy(c => isLetter(c) && c != escBegin).map(c => Some(c.toInt)))
                             | _checkBadChar(err)
            )
            case Unicode(isLetter) => err.labelStringCharacter(
                stringEscape | err.labelGraphicCharacter(satisfyUtf16(c => isLetter(c) && c != escBegin).map(Some(_)))
                             | _checkBadChar(err)
            )
            case NotRequired => stringEscape
        }
    }
}
