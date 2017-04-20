import scala.annotation.tailrec

/**
  * A game of tenpins bowling lasts ten frames, in each of which the bowler makes one or two attempts to knock down
  * ten pins arranged in a triangle. If the bowler knocks down all ten pins on the first attempt
  * (that’s called a “strike”), he scores ten pins plus the number of pins knocked down on his next two rolls.
  * If the bowler knocks down all ten pins after two attempts (that’s called a “spare”), he scores ten pins plus the
  * number of pins knocked down on his next roll. If the bowler fails to knock down all ten pins
  * (that’s called an “open frame”), he scores the number of pins he knocked down. T
  * he scores accumulate through all ten frames. At the last frame, if necessary, the pins are reset for one or two
  * additional rolls to count the final bonus.
  */

case class Roll(attempt1:Int,
           attempt2:Option[Int]=None,
           bonusAttempt:Option[Int]=None)

object BowlingGame extends BowlingGame

class BowlingGame {

  def rolling(rolls: List[Roll]): List[Any] = {

    val rollingScore = rolls.map {
      _
      match {
        case Roll(10, None, None) => "strike"
        case Roll(a, Some(b), None) if (a + b == 10) => "spare"
        case Roll(a, b, Some(c)) => "bonus"
        case Roll(a, Some(b), None) => a + b
        case Roll(a, None, None) => a
      }
    }

    @tailrec
    def acc(list: List[Any], i: Int): List[Any] = {
      if (i == rollingScore.size) {
        rollingScore.size ==10 match {
          case true => getFinalScore(rolls, list)
          case _=> list
        }
      }
      else {

        val element =  list(i) match {

          // consecutive strike .. 10 + next two attempts(strike included) + last score
          case currentScore : String if(list.lift(i+1).isDefined && list.lift(i+1).get.isInstanceOf[String]
                             && list.lift(i+1).get.asInstanceOf[String]== "strike" && currentScore=="strike") =>

            list.updated(i, updateBonusScore(rolls, list, i) +
                            (if(rolls.lift(i+2).isDefined){rolls(i+2).attempt1} else {rolls(i+1).attempt2.getOrElse(0)}))

          // strike or spare case .. if strike, 10 + next two attempts(if strike no second) + last score, if spare 10 + next one attempt + last score
          case currentScore : String if(list.lift(i+1).isDefined) =>

            list.updated(i, updateBonusScore(rolls, list, i) +
                            (if(currentScore=="strike") { rolls(i+1).attempt2.getOrElse(0)} else {0}))

          case currentScore : String => list.updated(i,currentScore)

          // normal open frame .. doing addition of two attempts + last core
          case currentScore : Int =>
            list.updated(i, updateScore(list, i, currentScore))
        }

        acc(element, i+1)

      }
    }

    acc(rollingScore, 0)

  }

  def getFinalScore(rolls: List[Roll], list: List[Any]): List[Any] ={
    val i = rolls.size-1
    list.updated(i, rolls(i).attempt1 +
                    rolls(i).attempt2.getOrElse(0) +
                    rolls(i).bonusAttempt.getOrElse(0) +
                    list.lift(i-1).fold(0)(x=> if(x.isInstanceOf[Int]){x.asInstanceOf[Int]} else{0}))
  }


  private def updateBonusScore(rolls: List[Roll], list: List[Any], i: Int): Int ={
    updateScore(list, i, 10) + rolls(i+1).attempt1
  }

  private def updateScore(list: List[Any], i: Int, currentScore: Int): Int ={
    currentScore +
      list.lift(i-1).fold(0)(x=> if(x.isInstanceOf[Int]){x.asInstanceOf[Int]} else{0})
  }

}
