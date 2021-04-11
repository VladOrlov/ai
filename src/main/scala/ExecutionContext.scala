package org.jvo.ai

import scala.collection.mutable.{Set => MutableSet}

case class ExecutionContext(endState: State,
                            passedStates: MutableSet[State] = MutableSet(),
                            nonVisitedNodes: MutableSet[PathNode] = MutableSet())

object ExecutionContext{
  implicit class ReachExecutionContext(executionContext: ExecutionContext){

    def addPassedState(implicit state: State): Boolean = {
      executionContext.passedStates.add(state)
    }

    def removeNonVisitedNode(implicit pathNode: PathNode): Boolean = {
      executionContext.nonVisitedNodes.remove(pathNode)
    }

    def updateContext(implicit pathNode: PathNode): Unit = {
      addPassedState(pathNode.state)
      removeNonVisitedNode(pathNode)
    }

    def addNonVisitedNodes(implicit availablePath: Seq[PathNode]): Unit = {
      availablePath.foreach(executionContext.nonVisitedNodes.add)
    }

    def getHighestPriorityNonVisitedNode: Option[PathNode] = {
      executionContext.nonVisitedNodes
        .toSeq
        .sortBy(_.cost)
        .headOption
    }
  }
}