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

}
