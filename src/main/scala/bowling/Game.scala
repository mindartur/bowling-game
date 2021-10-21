package bowling

import bowling.validation.Validators


class Game (private var frames: List[Frame]) {

  def recordThrow(pins: Int): Game = {
    Validators.validatePins(pins)
    val updatedBonusFrames = Game.updateBonusPoints(frames, pins)
    val newFrames = Game.addNewPins(updatedBonusFrames, pins)
    new Game(newFrames)
  }

  def score: Int = {
    frames.map(_.score).sum
  }

}


object Game {
  def apply(): Game = {
    new Game(List())
  }


  private[bowling] def updateBonusPoints(frames: List[Frame], pins: Int): List[Frame] = {
    frames match {
      case List() => frames
      case List(frame) => List(updateFrame(frame, pins))
      case List(preLastFrame, lastFrame) => List(updateFrame(preLastFrame, pins), updateFrame(lastFrame, pins))
      case prefixList :+ preLastFrame  :+ lastFrame => prefixList :+ updateFrame(preLastFrame, pins) :+ updateFrame(lastFrame, pins)
    }
  }

  private[bowling] def addNewPins(frames: List[Frame], pins: Int): List[Frame] = {
    frames match {
      case List() => List(Frame(pins))
      case firstFrames :+ lastFrame if frames.length == 9 => lastFrame match {
        case UnfinishedFrame(firstScore) => firstFrames :+ Frame(firstScore, pins)
        case _ => frames :+ LastFrame(pins)
      }
      case firstFrames :+ lastFrame => lastFrame match {
        case FinishedLastFrame(_, _) => throw new IllegalStateException("The game has ended")
        case FinishedBonusLastFrame(_, _, _) => throw new IllegalStateException("The game has ended")
        case UnfinishedLastFrame(firstScore) => firstFrames :+ LastFrame(firstScore, pins)
        case UnfinishedBonusLastFrame(firstScore, secondScore) => firstFrames :+ FinishedBonusLastFrame(firstScore, secondScore, pins)
        case UnfinishedFrame(firstScore) => firstFrames :+ Frame(firstScore, pins)
        case _ => frames :+ Frame(pins)
      }
    }
  }

  private[bowling] def updateFrame(frame: Frame, pins: Int): Frame = {
    frame match {
      case UnfinishedOneBonusStrike(bonus) => FinishedStrike(bonus, pins)
      case UnfinishedTwoBonusStrike() => UnfinishedOneBonusStrike(pins)
      case UnfinishedBonusSpare(s1, s2) => FinishedSpare(s1, s2, pins)
      case other => other
    }
  }

}
