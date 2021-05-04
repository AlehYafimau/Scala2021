package scala2021.ayafimau.task08

object Main extends App {

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
    val frames = mainGame split '|'

    val bonusBallsRaw = bonusGame.sliding(1).toList
    val bonusBalls = bonusBallsRaw match {
      case first :: second :: Nil => first :: second :: Nil
      case first :: Nil => first :: "" :: Nil
      case _ => "" :: "" :: Nil
    }

    val framesWithBonus = frames ++ bonusBalls
    framesWithBonus.sliding(3).foldLeft(0) { (accum, triple) =>
      val currentFrame = triple(0)
      val currentFrameBalls = currentFrame.toList
      currentFrameBalls match {
        case 'X' :: Nil =>
          val nextFrame = triple(1)
          val nextFrameBalls = nextFrame.toList
          nextFrameBalls match {
            case 'X' :: Nil => accum + 20 + extractFirstBallScore(triple(2))
            case _ :: '/' :: Nil => accum + 20
            case a :: b :: Nil => accum + 10 + extractScore(a) + extractScore(b)
            case a :: Nil => accum + 10 + extractScore(a) + extractFirstBallScore(triple(2)) // last frame + bonus situation
            case _ => accum + 10
          }
        case _ :: '/' :: Nil => accum + 10 + extractFirstBallScore(triple(1))
        case a :: b :: Nil => accum + extractScore(a) + extractScore(b)
      }
    }
  }

  def extractFirstBallScore(str: String): Int = {
    str.length match {
      case 0 => 0
      case _ => extractScore(str(0))
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
