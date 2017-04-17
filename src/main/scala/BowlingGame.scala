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

case class roll(attempt1:Int,
           attempt2:Option[Int]=None,
           bonusAttempt:Option[Int]=None)

object BowlingGame extends BowlingGame

class BowlingGame {

  def rolling(rolls: List[(Int, Int)]): List[Any] = {

//    val rollingScore = rolls.map { x =>
//     if(x._1 + x._2 == 10) "spare" else x._1 + x._2
//    }

    val rollingScore = rolls.map { x =>
      (x._1, x._2) match{
        case (10, _) => "strike"
        case (a, b) if(a+ b == 10) => "spare"
        case (x, y) => x+y
      }
    }

    @tailrec
    def acc(list: List[Any], i: Int): List[Any] = {
      if (i==rollingScore.size) list
      else {
        val element =  list(i) match {

          case s : String if(list.lift(i+1).isDefined && list.lift(i+1).get.isInstanceOf[String] && s=="spare") =>
            list.updated(i,10 + rolls(i+1)._1 + list.lift(i-1).fold(0)(x=> if(x.isInstanceOf[Int]){x.asInstanceOf[Int]} else{0}))

          case s : String if(list.lift(i+1).isDefined && list.lift(i+1).get.isInstanceOf[String] && s=="strike") =>
            list.updated(i,10 + rolls(i+1)._1+ rolls(i+1)._2 + list.lift(i-1).fold(0)(x=> if(x.isInstanceOf[Int]){x.asInstanceOf[Int]} else{0}))

          case s : String if(list.lift(i+1).isDefined && list.lift(i+1).get.isInstanceOf[Int]) =>
            list.updated(i,10 + rolls(i+1)._1 + rolls(i+1)._2 + list.lift(i-1).fold(0)(x=> if(x.isInstanceOf[Int]){x.asInstanceOf[Int]} else{0}))

          case s : Int =>
            list.updated(i,s + list.lift(i-1).fold(0)(x=> if(x.isInstanceOf[Int]){x.asInstanceOf[Int]} else{0}))

          case s : String => list.updated(i,s)
        }

        acc(element, i+1)

      }
    }

    acc(rollingScore, 0)
  }
}
