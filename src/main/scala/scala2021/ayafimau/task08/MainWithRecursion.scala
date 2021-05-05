package scala2021.ayafimau.task08

object MainWithRecursion extends App{

  List(
    "X|X|X|X|X|X|X|X|X|X||XX",
    "9-|9-|9-|9-|9-|9-|9-|9-|9-|9-||",
    "5/|5/|5/|5/|5/|5/|5/|5/|5/|5/||5",
    "X|7/|9-|X|-8|8/|-6|X|X|X||81"
  )
    .map(x => (x, tenPinBowlingScore(x)))
    .foreach(println)

  def tenPinBowlingScore(input: String): Int = {
    val splitByBonus = "(^.*)\\|\\|(.*)$".r
    val splitByBonus(mainGame, bonusGame) = input
    val mainBalls = mainGame.replace("|", "").toList
    val bonusBalls = bonusGame.toList
    val intermediateScore = tenPinBowlingInternal(1, 1, 0, mainBalls ++ bonusBalls)

    // let's extract redundant score that we summed up from bonus balls (when we were processing them like main balls)
    val bonusSum = bonusBalls.map(extractScore).fold(0)(_ + _)
    val overScore =
      if (bonusBalls.length == 2 && bonusBalls.head == 'X')  // case when 2nd bonus ball was counted again by bonus-rules
        bonusSum + extractScore(bonusBalls(1))
      else bonusSum
    intermediateScore - overScore
  }

  // CAUTION: this func will also count up bonus balls likewise they are balls of the main game
  // multiplier1 - how many times we should count score of the 1st ball (head)
  // multiplier2 - how many times we should count score of the 2nd ball (just after head)
  def tenPinBowlingInternal(multiplier1: Int, multiplier2: Int, prevBall: Int, balls: List[Char]) : Int = {
    balls match {
      case Nil => 0
      case head :: rest =>
        head match {
          case 'X' => 10 * multiplier1 + tenPinBowlingInternal(multiplier2 + 1, 2, 10, rest)
          case '/' => (10 - prevBall) * multiplier1 + tenPinBowlingInternal(multiplier2 + 1, 1, 10 - prevBall, rest)
          case a => extractScore(a) * multiplier1 + tenPinBowlingInternal(multiplier2, 1, extractScore(a), rest)
        }
    }
  }

  def extractScore(rawScore: Char): Int = {
    rawScore match {
      case 'X' => 10
      case ch if ch >= '0' && ch <= '9' => ch - '0'
      case _ => 0
    }
  }
}
