package bowling

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GameLogicTest extends AnyWordSpec with Matchers {

  "updateBonusPoints method" should {
    "update nothing if there is nothing to update #1" in {
      val frames = List(FinishedFrame(1, 2), FinishedFrame(2, 3), FinishedFrame(4, 2))
      Game.updateBonusPoints(frames, 5) shouldEqual frames
    }
    "update nothing if there is nothing to update #2" in {
      val frames = List(FinishedFrame(1, 2), FinishedStrike(1, 2), FinishedSpare(1, 2, 3))
      Game.updateBonusPoints(frames, 5) shouldEqual frames
    }
    "update strike and spare" in {
      val frames = List(FinishedFrame(1, 2), UnfinishedOneBonusStrike(2), UnfinishedBonusSpare(4, 2))
      Game.updateBonusPoints(frames, 5) shouldEqual List(FinishedFrame(1, 2), FinishedStrike(2, 5), FinishedSpare(4, 2, 5))
    }
    "update 2 strikes" in {
      val frames = List(FinishedFrame(1, 2), UnfinishedOneBonusStrike(10), UnfinishedTwoBonusStrike())
      Game.updateBonusPoints(frames, 5) shouldEqual List(FinishedFrame(1, 2), FinishedStrike(10, 5), UnfinishedOneBonusStrike(5))
    }
  }

  "addNewPins method" should {
    "add a new strike" in {
      val frames = List(FinishedFrame(4, 2))
      Game.addNewPins(frames, 10) shouldEqual List(FinishedFrame(4, 2), UnfinishedTwoBonusStrike())
    }
    "add a first frame" in {
      val frames = List()
      Game.addNewPins(frames, 3) shouldEqual List(UnfinishedFrame(3))
    }
    "add a second try to a first frame" in {
      val frames = List(UnfinishedFrame(3))
      Game.addNewPins(frames, 3) shouldEqual List(FinishedFrame(3, 3))
    }
    "add a spare" in {
      val frames = List(UnfinishedFrame(3))
      Game.addNewPins(frames, 7) shouldEqual List(UnfinishedBonusSpare(3, 7))
    }
    "add a second try to last frame" in {
      val frames = List(UnfinishedLastFrame(3))
      Game.addNewPins(frames, 3) shouldEqual List(FinishedLastFrame(3, 3))
    }
    "add a spare to last frame" in {
      val frames = List(UnfinishedLastFrame(3))
      Game.addNewPins(frames, 7) shouldEqual List(UnfinishedBonusLastFrame(3, 7))
    }
    "add a last try to last frame" in {
      val frames = List(UnfinishedBonusLastFrame(3, 7))
      Game.addNewPins(frames, 7) shouldEqual List(FinishedBonusLastFrame(3, 7, 7))
    }
    "add a last frame" in {
      val frames = List.fill(9)(FinishedFrame(3, 3))
      Game.addNewPins(frames, 7) shouldEqual frames :+ UnfinishedLastFrame(7)
    }
    "don't add a last frame" in {
      val frames = List.fill(8)(FinishedFrame(3, 3))
      Game.addNewPins(frames, 7) shouldEqual frames :+ UnfinishedFrame(7)
    }
  }

  "scores method" should {
    "give a correct score - simple games" in {
      checkScores(List(1, 2, 3), List(1, 3, 6))
      checkScores(List(5, 3, 2, 5), List(5, 8, 10, 15))
    }
    "give a correct score - strikes" in {
      checkScores(List(1, 2, 10, 2, 4), List(1, 3, 13, 17, 25))
      checkScores(List(10, 2, 4, 10, 1, 2), List(10, 14, 22, 32, 34, 38))
    }
    "give a correct score - spares" in {
      checkScores(List(1, 9, 1, 2), List(1, 10, 12, 14))
      checkScores(List(10, 1, 9, 8, 2, 2, 3), List(10, 12, 30, 46, 48, 52, 55))
    }
    "give a correct score - last game with bonus strike" in {
      checkLastScore(List(3, 0, 1, 9, 8, 2, 2, 3, 8, 2, 3, 2, 4, 5, 4, 3, 5, 5, 10, 6, 3), 111)
    }
    "give a correct score - last game with bonus spare and final strike" in {
      checkLastScore(List(3, 0, 1, 9, 8, 2, 2, 3, 8, 2, 3, 2, 4, 5, 4, 3, 5, 5, 1, 9, 10), 103)
    }
    "give a correct score - last game without bonuses" in {
      checkLastScore(List(3, 0, 1, 9, 8, 2, 2, 3, 8, 2, 3, 2, 4, 5, 4, 3, 5, 5, 3, 5), 93)
    }
    "give a correct score - perfect game" in {
      checkLastScore(List(10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10), 300)
    }
    "throw error - too much moves" in {
      the [IllegalStateException] thrownBy playGame(List(10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 1))
      the [IllegalStateException] thrownBy playGame(List(3, 0, 1, 9, 8, 2, 2, 3, 8, 2, 3, 2, 4, 5, 4, 3, 5, 5, 3, 5, 1))
      the [IllegalStateException] thrownBy playGame(List(3, 0, 1, 9, 8, 2, 2, 3, 8, 2, 3, 2, 4, 5, 4, 3, 5, 5, 1, 9, 10, 1))
      the [IllegalStateException] thrownBy playGame(List(3, 0, 1, 9, 8, 2, 2, 3, 8, 2, 3, 2, 4, 5, 4, 3, 5, 5, 10, 6, 3, 1))
    }
    "throw error - illigal number of pins" in {
      the[IllegalArgumentException] thrownBy playGame(List(9, 2))
      the[IllegalArgumentException] thrownBy playGame(List(3, 2, 11))
      the[IllegalArgumentException] thrownBy playGame(List(4, 2, 1, 10))
    }
  }

  def checkScores(pins: List[Int], expectedScores: List[Int]): Unit = {
    pins.length shouldEqual expectedScores.length
    var game = Game()

    pins.zip(expectedScores).foreach{case (pin, expectedScore) =>
      game = game.recordThrow(pin)
      game.score shouldEqual(expectedScore)
    }
  }

  def checkLastScore(pins: List[Int], expectedScore: Int): Unit = {
    var game = Game()

    pins.foreach{pin =>
      game = game.recordThrow(pin)
    }
    game.score shouldEqual(expectedScore)
  }

  def playGame(pins: List[Int]): Unit = {
    var game = Game()

    pins.foreach{pin =>
      game = game.recordThrow(pin)
    }
  }
}
