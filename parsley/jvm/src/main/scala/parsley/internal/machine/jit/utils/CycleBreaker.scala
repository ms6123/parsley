/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.jit.utils

import scala.collection.mutable

object CycleBreaker {
    def chooseBreakNodes(graphIn: Map[Int, Set[Int]]): Set[Int] = {
        val sortedKeys = graphIn.keys.toList.sorted

        val graph: mutable.Map[Int, mutable.SortedSet[Int]] = mutable.Map.empty[Int, mutable.SortedSet[Int]] ++= graphIn.map { case (k, v) =>
            k -> (mutable.SortedSet.empty[Int] ++= v)
        }

        def findCycle(g: collection.Map[Int, collection.Iterable[Int]]): Option[List[Int]] = {
            val visited = mutable.Set.empty[Int]
            val visiting = mutable.Set.empty[Int]
            var cycleOpt: Option[List[Int]] = None

            def dfs(node: Int, path: List[Int]): Unit = {
                if (cycleOpt.isDefined) {
                    return
                }
                visiting.add(node)

                for (neighbor <- g.getOrElse(node, Nil)) {
                    if (cycleOpt.isEmpty) {
                        if (visiting.contains(neighbor)) {
                            // Back-edge found: extract the exact cycle from the DFS path stack
                            val cycle = neighbor :: (node :: path).takeWhile(_ != neighbor)
                            cycleOpt = Some(cycle)
                        } else if (!visited.contains(neighbor)) {
                            dfs(neighbor, node :: path)
                        }
                    }
                }

                visiting.remove(node)
                visited.add(node)
            }

            for (node <- sortedKeys) {
                if (!visited.contains(node) && cycleOpt.isEmpty) {
                    dfs(node, Nil)
                }
            }

            cycleOpt
        }

        val nodesToBreak = Set.newBuilder[Int]

        var cycle = findCycle(graph)
        while (cycle.isDefined) {
            val nodeToBreak = cycle.get.max
            nodesToBreak += nodeToBreak

            graph.remove(nodeToBreak)
            graph.values.foreach(_.remove(nodeToBreak))

            cycle = findCycle(graph)
        }

        nodesToBreak.result()
    }
}
