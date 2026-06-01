package parsley.internal.machine.jit.utils

import scala.collection.mutable

object Tarjan {
    def findCyclicNodes(graph: collection.Map[Int, Set[Int]]): Set[Int] = {
        var indexCounter = 0
        val index = mutable.Map.empty[Int, Int]
        val lowlink = mutable.Map.empty[Int, Int]
        val onStack = mutable.Set.empty[Int]

        var stack = List.empty[Int]
        val cyclicNodes = Set.newBuilder[Int]

        def strongconnect(v: Int): Unit = {
            index(v) = indexCounter
            lowlink(v) = indexCounter
            indexCounter += 1

            stack = v :: stack
            onStack.add(v)

            val neighbors = graph.getOrElse(v, Set.empty)
            for (w <- neighbors) {
                if (!index.contains(w)) {
                    strongconnect(w)
                    lowlink(v) = math.min(lowlink(v), lowlink(w))
                } else if (onStack.contains(w)) {
                    lowlink(v) = math.min(lowlink(v), index(w))
                }
            }

            if (lowlink(v) == index(v)) {
                val scc = mutable.Set.empty[Int]
                var w = -1
                while ({
                    w = stack.head
                    stack = stack.tail
                    onStack.remove(w)
                    scc.add(w)
                    w != v
                }) ()

                if (scc.size > 1 || neighbors.contains(v)) {
                    cyclicNodes ++= scc
                }
            }
        }

        for (v <- graph.keys) {
            if (!index.contains(v)) {
                strongconnect(v)
            }
        }

        cyclicNodes.result()
    }
}
