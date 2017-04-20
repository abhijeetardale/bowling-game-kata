import org.scalatest.{Matchers, WordSpec}


class BowlingGameTest extends WordSpec with Matchers {

  val game = new  BowlingGame()

  val rolls = List(Roll(1,Some(4)),Roll(4,Some(5)),Roll(6,Some(4)),Roll(5,Some(5)),Roll(10),Roll(0,Some(1)),Roll(7,Some(3)),Roll(6,Some(4)),Roll(10),Roll(2,Some(8),Some(6)))

  "bowler made an attempt" should {

    "return the score after 1st attempt" in {
      //List(Roll(1,4))
      game.rolling(rolls.take(1)) shouldBe List(5)

    }

    "return the score after 2st attempt" in {
      //List(Roll(1,4),Roll(4,5))
      game.rolling(rolls.take(2)) shouldBe List(5,14)

    }

    "return the score after 3rd attempt with spare" in {
      //List(Roll(1,4),Roll(4,5),Roll(6,4))
      game.rolling(rolls.take(3)) shouldBe List(5,14,"spare")

    }

    "return the score after 4th attempt with spare" in {
      //List(Roll(1,4),Roll(4,5),Roll(6,4),Roll(5,5))
      game.rolling(rolls.take(4)) shouldBe List(5,14,29,"spare")

    }

    "return the score after 5th attempt with strike" in {
      //List(Roll(1,4),Roll(4,5),Roll(6,4),Roll(5,5),Roll(10))
      game.rolling(rolls.take(5)) shouldBe List(5,14,29,49,"strike")

    }

    "return the score after 6th attempt" in {
      //List(Roll(1,4),Roll(4,5),Roll(6,4),Roll(5,5),Roll(10),Roll(0,1))
      game.rolling(rolls.take(6)) shouldBe List(5,14,29,49,60,61)

    }

    "return the score after 7th attempt with spare" in {
      //List(Roll(1,4),Roll(4,5),Roll(6,4),Roll(5,5),Roll(10),Roll(0,1),Roll(7,3))
      game.rolling(rolls.take(7)) shouldBe List(5,14,29,49,60,61,"spare")

    }

    "return the score after 8th attempt with spare" in {
      //List(Roll(1,4),Roll(4,5),Roll(6,4),Roll(5,5),Roll(10),Roll(0,1),Roll(7,3),Roll(6,4))
      game.rolling(rolls.take(8)) shouldBe List(5,14,29,49,60,61,77,"spare")

    }


    "return the score after 9th attempt with strike" in {
      //List(Roll(1,4),Roll(4,5),Roll(6,4),Roll(5,5),Roll(10),Roll(0,1),Roll(7,3),Roll(6,4),Roll(10))
      game.rolling(rolls.take(9)) shouldBe List(5,14,29,49,60,61,77,97, "strike")

    }

    "return the score after 10th attempt with spare" in {
      //List(Roll(1,4),Roll(4,5),Roll(6,4),Roll(5,5),Roll(10),Roll(0,1),Roll(7,3),Roll(6,4),Roll(10),Roll(2,8,Some(6)))
      game.rolling(rolls) shouldBe List(5,14,29,49,60,61,77,97, 117, 133)//List(5,14,29,49,60,61,77,97, 117,"bonus")

    }

    "return the score after 10th attempt with all zeros" in {

      val rollsAllZero = List(Roll(0),Roll(0),Roll(0),Roll(0),Roll(0),Roll(0),Roll(0),Roll(0),Roll(0),Roll(0))
      game.rolling(rollsAllZero) shouldBe List(0,0,0,0,0,0,0,0,0,0)
    }

    "return the score after 10th attempt with all ones" in {

      val rollsAllOne = List(Roll(1,Some(1)),Roll(1,Some(1)),Roll(1,Some(1)),Roll(1,Some(1)),Roll(1,Some(1)),Roll(1,Some(1)),Roll(1,Some(1)),Roll(1,Some(1)),Roll(1,Some(1)),Roll(1,Some(1)))
      game.rolling(rollsAllOne) shouldBe List(2,4,6,8,10,12,14,16,18,20)

    }

    "return the score after 10th attempt with all spare" in {

      val rollsAllSpare = List(Roll(9,Some(1)),Roll(9,Some(1)),Roll(9,Some(1)),Roll(9,Some(1)),Roll(9,Some(1)),Roll(9,Some(1)),Roll(9,Some(1)),Roll(9,Some(1)),Roll(9,Some(1)),Roll(9,Some(1)))
      game.rolling(rollsAllSpare) shouldBe List(19, 38, 57, 76, 95, 114, 133, 152, 171, 181)//List(19, 38, 57, 76, 95, 114, 133, 152, 171, "spare")

    }

    "return the score after 10th attempt with all spare and last strike" in {

      val rollsAllSpareWithLastStrike = List(Roll(9,Some(1)),Roll(9,Some(1)),Roll(9,Some(1)),Roll(9,Some(1)),Roll(9,Some(1)),Roll(9,Some(1)),Roll(9,Some(1)),Roll(9,Some(1)),Roll(9,Some(1)),Roll(9,Some(1),Some(10)))
      game.rolling(rollsAllSpareWithLastStrike) shouldBe List(19, 38, 57, 76, 95, 114, 133, 152, 171, 191)//List(19, 38, 57, 76, 95, 114, 133, 152, 171, "bonus")

    }

    "return the score after 10th attempt with alternate spare and strike" in {

      val rollsAlternateSpareAndStrike = List(Roll(9,Some(1)),Roll(10),Roll(9,Some(1)),Roll(10),Roll(9,Some(1)),Roll(10),Roll(9,Some(1)),Roll(10),Roll(9,Some(1)),Roll(10,Some(9),Some(10)))
      game.rolling(rollsAlternateSpareAndStrike) shouldBe List(20, 40, 60, 80, 100, 120, 140, 160, 180, 209)//List(20, 40, 60, 80, 100, 120, 140, 160, 180, "bonus")

    }
    "return the score after 10th attempt with alternate strike and spare" in {

      val rollsAlternateSpareAndStrike = List(Roll(10),Roll(9,Some(1)),Roll(10),Roll(9,Some(1)),Roll(10),Roll(9,Some(1)),Roll(10),Roll(9,Some(1)),Roll(10),Roll(9,Some(1),Some(10)))
      game.rolling(rollsAlternateSpareAndStrike) shouldBe List(20, 40, 60, 80, 100, 120, 140, 160, 180, 200)//List(20, 40, 60, 80, 100, 120, 140, 160, 180, "bonus")

    }

    "return the score after 10th attempt with all strike" in {

      val rollsAllStrike = List(Roll(10),Roll(10),Roll(10),Roll(10),Roll(10),Roll(10),Roll(10),Roll(10),Roll(10),Roll(10,Some(10),Some(10)))
      game.rolling(rollsAllStrike) shouldBe List(30, 60, 90, 120, 150, 180, 210, 240, 270, 300)//List(30, 60, 90, 120, 150, 180, 210, 240, 270, "bonus")

    }

    "return the score after 10th attempt with all strike except last" in {

      val rollsAllStrike = List(Roll(10),Roll(10),Roll(10),Roll(10),Roll(10),Roll(10),Roll(10),Roll(10),Roll(10),Roll(9,Some(1),Some(10)))
      game.rolling(rollsAllStrike) shouldBe List(30, 60, 90, 120, 150, 180, 210, 239, 259, 279)//List(30, 60, 90, 120, 150, 180, 210, 239, 259, "bonus")

    }

    "return the score after 10th attempt with all strike except last with different combination" in {

      val rollsAllStrike = List(Roll(10),Roll(10),Roll(10),Roll(10),Roll(10),Roll(10),Roll(10),Roll(10),Roll(10),Roll(10,Some(9),Some(10)))
      game.rolling(rollsAllStrike) shouldBe List(30, 60, 90, 120, 150, 180, 210, 240, 269, 298)//List(30, 60, 90, 120, 150, 180, 210, 240, 269, "bonus")

    }

    "return the score after 10th attempt with random1 combinations" in {

      val rollsAllRandom = List(Roll(5,Some(1)),Roll(3,Some(7)),Roll(7,Some(1)),Roll(9),Roll(7,Some(1)),Roll(9),Roll(4,Some(5)),Roll(8,Some(1)),Roll(10),Roll(10,Some(8)))
      game.rolling(rollsAllRandom) shouldBe List(6, 23, 31, 40, 48, 57, 66, 75, 103, 121)

    }

    "return the score after 10th attempt with random2 combinations" in {

      val rollsAllRandom = List(Roll(0),Roll(7,Some(2)),Roll(0,Some(2)),Roll(3,Some(5)),Roll(0),Roll(0),Roll(4,Some(1)),Roll(3,Some(3)),Roll(0,Some(6)),Roll(0,Some(9)))
      game.rolling(rollsAllRandom) shouldBe List(0, 9, 11, 19, 19, 19, 24, 30, 36, 45)

    }

    "return the score after 10th attempt with random3 combinations" in {

      val rollsAllRandom = List(Roll(9),Roll(9),Roll(7,Some(3)),Roll(7,Some(2)),Roll(1),Roll(3),Roll(7),Roll(0,Some(7)),Roll(8,Some(2)),Roll(9,Some(1)))
      game.rolling(rollsAllRandom) shouldBe List(9, 18, 35, 44, 45, 48, 55, 62, 81, 91) //List(9, 18, 35, 44, 45, 48, 55, 62, 81, "spare")

    }

  }

}
