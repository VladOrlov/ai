package org.jvo.ai

case class PathNode(state: State,
                    previousPathNode: Option[PathNode],
                    //g(x)
                    step: Int,
                    //h(x)
                    heuristics: Int,
                    //f(x)
                    cost: Int)


object PathNode {

  implicit class ReachPathNode(pathNode: PathNode) extends Ordered[PathNode] {
    override def compare(that: PathNode): Int = {
      if (pathNode.cost == that.cost)
        0
      else if (pathNode.cost > that.cost)
        1
      else
        -1
    }
  }

}