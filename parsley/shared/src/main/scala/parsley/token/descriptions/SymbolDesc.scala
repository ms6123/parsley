/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.descriptions

import parsley.internal.collection.immutable.Trie

/** This class describes how symbols (textual literals in a BNF) should be
  * processed lexically.
  *
  * @param hardKeywords what keywords are ''always'' treated as keywords within the language.
  * @param hardOperators what operators are ''always'' treated as reserved operators within the language.
  * @param caseSensitive are the keywords case sensitive: when `false`, `IF == if`.
  * @since 4.0.0
  */
final case class SymbolDesc (hardKeywords: Set[String],
                             hardOperators: Set[String],
                             caseSensitive: Boolean) {
    require((hardKeywords & hardOperators).isEmpty, "there cannot be an intersection between keywords and operators")
    private [parsley] val hardOperatorsTrie: Trie[Unit] = Trie(hardOperators)

    private [parsley] val isReservedName: String => Boolean = {
        val theReservedNames = if (caseSensitive) hardKeywords else hardKeywords.map(_.toLowerCase)
        if (caseSensitive) theReservedNames.contains
        else name => theReservedNames.contains(name.toLowerCase)
    }

    private [parsley] val isReservedOp: String => Boolean = hardOperators.contains
}

/** This object contains any preconfigured symbol descriptions.
  * @since 4.0.0
  */
object SymbolDesc {
    /** Plain definition of symbols: case sensitive with no hard keywords or operators.
      *
      * {{{
      * hardKeywords = Set.empty
      * hardOperators = Set.empty
      * caseSensitive = true
      * }}}
      *
      * @since 4.0.0
      */
    val plain = SymbolDesc(hardKeywords = Set.empty, hardOperators = Set.empty, caseSensitive = true)
}
