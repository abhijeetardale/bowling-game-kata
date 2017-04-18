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
           attempt2:Int,
           bonusAttempt:Option[Int]=None)

object BowlingGame extends BowlingGame

class BowlingGame {

  def rolling(rolls: List[Roll]): List[Any] = {

//    val rollingScore = rolls.map { x =>
//     if(x._1 + x._2 == 10) "spare" else x._1 + x._2
//    }

    val rollingScore = rolls.map { roll =>
      roll match{
        case Roll(10,0, None) => "strike"
        case Roll(a, b, None) if(a+ b == 10) => "spare"
        case Roll(a, b, Some(x)) => "bonus"
        case Roll(x, y, None) => x+y
      }
    }

    @tailrec
    def acc(list: List[Any], i: Int): List[Any] = {
      if (i == rollingScore.size) {
        list
      }
      else {
        //println(list(i) + s" index : $i => $list")
        val element =  list(i) match {

          case currentScore : String if(list.lift(i+1).isDefined && list.lift(i+1).get.isInstanceOf[String]
                             && list.lift(i+1).get.asInstanceOf[String]== "strike" && currentScore=="strike") =>
            list.updated(i, 10 +
                            rolls(i+1).attempt1 +
                            (if(rolls.lift(i+2).isDefined){rolls(i+2).attempt1} else {rolls(i+1).attempt2}) +
                            list.lift(i-1).fold(0)(x=> if(x.isInstanceOf[Int]){x.asInstanceOf[Int]} else{0}))

          case currentScore : String if(list.lift(i+1).isDefined) =>
            list.updated(i, 10 +
                            rolls(i+1).attempt1 +
                            (if(currentScore=="strike") { rolls(i+1).attempt2 } else {0}) +
                            list.lift(i-1).fold(0)(x=> if(x.isInstanceOf[Int]){x.asInstanceOf[Int]} else{0}))

          case currentScore : String => list.updated(i,currentScore)

          case currentScore : Int =>
            list.updated(i, currentScore +
                            list.lift(i-1).fold(0)(x=> if(x.isInstanceOf[Int]){x.asInstanceOf[Int]} else{0}))
        }

        acc(element, i+1)

      }
    }

    acc(rollingScore, 0)
  }

  def getFinalScore(rolls: List[Roll]): List[Any] ={
    val i = rolls.size-1
    val list = rolling(rolls)
    list.updated(i, rolls(i).attempt1 +
                    rolls(i).attempt2 +
                    rolls(i).bonusAttempt.getOrElse(0) +
                    list.lift(i-1).fold(0)(x=> if(x.isInstanceOf[Int]){x.asInstanceOf[Int]} else{0}))
  }


    /*@tailrec
    def acc(list: List[Any], i: Int): List[Any] = {
      if (i==rollingScore.size) list
      else {
        val element =  list(i) match {

          case s : String if(list.lift(i+1).isDefined && list.lift(i+1).get.isInstanceOf[String] && s=="spare") =>
            list.updated(i, getSpareBonus(rolls, list, i))

          case s : String if(list.lift(i+1).isDefined && list.lift(i+1).get.isInstanceOf[String] && s=="strike") =>
            list.updated(i, getStrikeBonus(rolls, list, i))

          case s : String if(list.lift(i+1).isDefined && list.lift(i+1).get.isInstanceOf[Int]) =>
            list.updated(i, getStrikeBonus(rolls, list, i))

          case s : Int =>
            list.updated(i, s + getLastScore(list, i))

          case s : String => list.updated(i,s)
        }

        acc(element, i+1)

      }
    }

    acc(rollingScore, 0)
  }

  private def getLastScore(list: List[Any], i: Int): Int = {
    list.lift(i-1).fold(0)(x=> if(x.isInstanceOf[Int]){x.asInstanceOf[Int]} else{0})
  }

  private def getStrikeBonus(rolls: List[Roll], list: List[Any], i: Int): Int = {
    getSpareBonus(rolls, list, i)+ rolls(i+1).attempt2
  }

  private def getSpareBonus(rolls: List[Roll], list: List[Any], i: Int): Int = {
    10 + rolls(i+1).attempt1 + getLastScore(list, i)
  }*/
}
