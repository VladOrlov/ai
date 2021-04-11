package org.jvo.ai

case class State(barrel1: Barrel, barrel2: Barrel, barrel3: Barrel)

object State {

  def getStateAfterPourOver(result: PourOverResult)(nonChangedBarrel: Barrel): State = {
    result.pourOverDirection match {
      case FromBarrelOneToBarrelTwo =>
        State(barrel1 = result.sourceBarrel, barrel2 = result.targetBarrel, barrel3 = nonChangedBarrel)
      case FromBarrelOneToBarrelThree =>
        State(barrel1 = result.sourceBarrel, barrel2 = nonChangedBarrel, barrel3 = result.targetBarrel)
      case FromBarrelTwoToBarrelOne =>
        State(barrel1 = result.targetBarrel, barrel2 = result.sourceBarrel, barrel3 = nonChangedBarrel)
      case FromBarrelTwoToBarrelThree =>
        State(barrel1 = nonChangedBarrel, barrel2 = result.sourceBarrel, barrel3 = result.targetBarrel)
      case FromBarrelThreeToBarrelOne =>
        State(barrel1 = result.targetBarrel, barrel2 = nonChangedBarrel, barrel3 = result.sourceBarrel)
      case FromBarrelThreeToBarrelTwo =>
        State(barrel1 = nonChangedBarrel, barrel2 = result.targetBarrel, barrel3 = result.sourceBarrel)
    }
  }

  implicit class RichState(state: State) {

    def getBarrelsState: String = {
      s"barrel 1 - ${state.barrel1.currentVolume}L, barrel 2 - ${state.barrel2.currentVolume}L, barrel 3 - ${state.barrel3.currentVolume}L"
    }
  }

}