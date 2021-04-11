package org.jvo.ai

case class Barrel(maxVolume: Int, currentVolume: Int)

object Barrel {

  implicit class RichBarrel(barrel: Barrel) {

    def pourOverToBarrel(targetBarrel: Barrel): Option[PourOverResult] = {
      isTransfusionPossible(barrel, targetBarrel) match {
        case true =>
          val newTargetBarrelVolume = barrel.currentVolume + targetBarrel.currentVolume

          Option(if (newTargetBarrelVolume > targetBarrel.maxVolume) {
            PourOverResult(
              sourceBarrel = barrel.copy(currentVolume = newTargetBarrelVolume - targetBarrel.maxVolume),
              targetBarrel = targetBarrel.copy(currentVolume = targetBarrel.maxVolume)
            )
          } else {
            PourOverResult(
              sourceBarrel = barrel.copy(currentVolume = 0),
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

}
