package org.jvo.ai

import scala.annotation.tailrec

object AStar {

  def main(args: Array[String]): Unit = {
    printFastestSolution(findSolution(getInitialPathNode(getStartState))(ExecutionContext(getEndState)))
  }

  @tailrec
  private def findSolution(maybePathNode: Option[PathNode])(implicit executionContext: ExecutionContext): Option[PathNode] = {
    val maybe = maybePathNode
      .map(pathNode => evaluatePathAndUpdateExecutionContext(executionContext)(pathNode))
      .flatMap(_.headOption)
      .orElse(executionContext.getHighestPriorityNonVisitedNode)
      .map((pathNode: PathNode) => PathNodeHasEndState(pathNode, pathNode.state == executionContext.endState))
      // В этом случае @tailrec оптимизация не работает
      //      .flatMap {
      //        case PathNodeHasEndState(pathNode, true) => Option(pathNode)
      //        case PathNodeHasEndState(pathNode, false) => findSolution(Option(pathNode))
      //      }

    // А так работает
    maybe match {
      case Some(value) => value match {
        case PathNodeHasEndState(pathNode, true) => Option(pathNode)
        case PathNodeHasEndState(pathNode, false) => findSolution(Option(pathNode))
      }
      case _ => None
    }
  }

  private def getInitialPathNode(startState: State): Option[PathNode] = {
    Option(
      PathNode(
        state = startState,
        previousPathNode = None,
        step = 1,
        heuristics = Math.abs(startState.barrel1.currentVolume - startState.barrel3.currentVolume),
        cost = 1 + 2)
    )
  }

  private def evaluatePathAndUpdateExecutionContext(executionContext: ExecutionContext)(implicit pathNode: PathNode): Seq[PathNode] = {
    executionContext.updateContext
    implicit val availablePath: Seq[PathNode] = getAvailablePath(pathNode, executionContext).sortBy(_.cost)
    executionContext.addNonVisitedNodes

    println(s"step: ${executionContext.getCurrentStep}, pathNode: $pathNode")
//    availablePath.foreach(println)

    availablePath
  }

  private def printFastestSolution(maybePathNode: Option[PathNode]): Unit = {
    restorePathRecursively(maybePathNode)
      .zip(Stream from 1)
      .foreach {
        case (state, step) => println(s" step: $step, state: ${state.getBarrelsState}")
      }
  }

  private def restorePathRecursively(maybePathNode: Option[PathNode]): Seq[State] = {
    maybePathNode match {
      case Some(value) =>
        restorePathRecursively(value.previousPathNode) ++ Seq(value.state)
      case None => Nil
    }
  }

  private def getEndState = {
    State(
      barrel1 = Barrel(maxVolume = 6, currentVolume = 5),
      barrel2 = Barrel(maxVolume = 3, currentVolume = 0),
      barrel3 = Barrel(maxVolume = 7, currentVolume = 5),
    )
  }

  private def getStartState = {
    State(
      barrel1 = Barrel(maxVolume = 6, currentVolume = 4),
      barrel2 = Barrel(maxVolume = 3, currentVolume = 0),
      barrel3 = Barrel(maxVolume = 7, currentVolume = 6),
    )
  }

  private def getAvailablePath(pathNode: PathNode, executionContext: ExecutionContext): Seq[PathNode] = {
    getPossibleNewStates(pathNode)
      .filterNot(executionContext.passedStates.contains)
      .map(state => createNewPathNode(pathNode, state))
  }

  private def getPossibleNewStates(pathNode: PathNode): Seq[State] = {

    val State(barrel1, barrel2, barrel3) = pathNode.state

    List(
      barrel1.pourOverToBarrel(barrel2)
        .map(result => State(barrel1 = result.sourceBarrel, barrel2 = result.targetBarrel, barrel3 = barrel3)),
      barrel1.pourOverToBarrel(barrel3)
        .map(result => State(barrel1 = result.sourceBarrel, barrel2 = barrel2, barrel3 = result.targetBarrel)),
      barrel2.pourOverToBarrel(barrel1)
        .map(result => State(barrel1 = result.targetBarrel, barrel2 = result.sourceBarrel, barrel3 = barrel3)),
      barrel2.pourOverToBarrel(barrel3)
        .map(result => State(barrel1 = barrel1, barrel2 = result.sourceBarrel, barrel3 = result.targetBarrel)),
      barrel3.pourOverToBarrel(barrel1)
        .map(result => State(barrel1 = result.targetBarrel, barrel2 = barrel2, barrel3 = result.sourceBarrel)),
      barrel3.pourOverToBarrel(barrel2)
        .map(result => State(barrel1 = barrel1, barrel2 = result.targetBarrel, barrel3 = result.sourceBarrel))
    ).flatten
  }

  private def createNewPathNode(pathNode: PathNode, state: State) = {
    val heuristics = Math.abs(state.barrel1.currentVolume - state.barrel3.currentVolume)
    PathNode(
      state = state,
      previousPathNode = Some(pathNode),
      step = pathNode.step + 1,
      heuristics = heuristics,
      cost = pathNode.step + 1 + heuristics)
  }

  private def pourOverLiquid(sourceBarrel: Barrel, targetBarrel: Barrel): Option[PourOverResult] = {

    isTransfusionPossible(sourceBarrel, targetBarrel) match {
      case true =>
        val newTargetBarrelVolume = sourceBarrel.currentVolume + targetBarrel.currentVolume

        Option(if (newTargetBarrelVolume > targetBarrel.maxVolume) {
          PourOverResult(
            sourceBarrel = sourceBarrel.copy(currentVolume = newTargetBarrelVolume - targetBarrel.maxVolume),
            targetBarrel = targetBarrel.copy(currentVolume = targetBarrel.maxVolume)
          )
        } else {
          PourOverResult(
            sourceBarrel = sourceBarrel.copy(currentVolume = 0),
            targetBarrel = targetBarrel.copy(currentVolume = newTargetBarrelVolume)
          )
        })
      case false =>
        None
    }
  }

  private def isTransfusionPossible(sourceBarrel: Barrel, targetBarrel: Barrel): Boolean = {
    sourceBarrel.currentVolume > 0 && targetBarrel.currentVolume != targetBarrel.maxVolume
  }
}
