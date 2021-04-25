package scala2021.ayafimau.task04

import scala.annotation.tailrec

object Main extends App {
  val coins = List(2, 4, 6)

  val k = 8

  println(canChange(k))

  def canChange(sum: Int): Boolean = {

    @tailrec def canChangeInternal(possibleSums: List[Int]): Boolean = {
      val targetSums = possibleSums.distinct.filter(x => x <= sum)
      if (targetSums.isEmpty)
        false
      else if (targetSums.contains(sum))
        true
      else {
        val spawnedSums = targetSums.flatMap(x =>
          coins.map(coin => x + coin)
        )
        canChangeInternal(spawnedSums)
      }
    }

    canChangeInternal(coins)
  }
}
