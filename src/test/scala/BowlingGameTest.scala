import org.scalatest.{Matchers, WordSpec}


class BowlingGameTest extends WordSpec with Matchers {

  val game = new  BowlingGame()

  "bowler made an attempt" should {

    "return the score after 1st attempt" in {

      game.rolling(List((1,4))) shouldBe List(5)

    }

    "return the score after 2st attempt" in {

      game.rolling(List((1,4),(4,5))) shouldBe List(5,14)

    }

    "return the score after 3rd attempt with spare" in {

      game.rolling(List((1,4),(4,5),(6,4))) shouldBe List(5,14,"spare")

    }

    "return the score after 4th attempt with spare" in {

      game.rolling(List((1,4),(4,5),(6,4),(5,5))) shouldBe List(5,14,29,"spare")

    }

    "return the score after 5th attempt with strike" in {

      game.rolling(List((1,4),(4,5),(6,4),(5,5),(10,0))) shouldBe List(5,14,29,49,"strike")

    }

    "return the score after 6th attempt" in {

      game.rolling(List((1,4),(4,5),(6,4),(5,5),(10,0),(0,1))) shouldBe List(5,14,29,49,60,61)

    }

    "return the score after 7th attempt with spare" in {

      game.rolling(List((1,4),(4,5),(6,4),(5,5),(10,0),(0,1),(7,3))) shouldBe List(5,14,29,49,60,61,"spare")

    }

    "return the score after 8th attempt with spare" in {

      game.rolling(List((1,4),(4,5),(6,4),(5,5),(10,0),(0,1),(7,3),(6,4))) shouldBe List(5,14,29,49,60,61,77,"spare")

    }


    "return the score after 9th attempt with strike" in {

      game.rolling(List((1,4),(4,5),(6,4),(5,5),(10,0),(0,1),(7,3),(6,4),(10,0))) shouldBe List(5,14,29,49,60,61,77,97, "strike")

    }

    "return the score after 10th attempt with spare" in {

      game.rolling(List((1,4),(4,5),(6,4),(5,5),(10,0),(0,1),(7,3),(6,4),(10,0),(2,8))) shouldBe List(5,14,29,49,60,61,77,97, 117,"spare")

    }

  }

}
