package org.jvo.ai

case class State(barrel1: Barrel, barrel2: Barrel, barrel3: Barrel)

object State {

  implicit class RichState(state: State) {

    def getBarrelsState: String = {
      s"barrel 1 - ${state.barrel1.currentVolume}L, barrel 2 - ${state.barrel2.currentVolume}L, barrel 3 - ${state.barrel3.currentVolume}L"
    }
  }

}