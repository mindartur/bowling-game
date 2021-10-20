package bowling

import bowling.validation.Validators

sealed trait Frame {
  def score: Int
  def tries: Int = 2
}
trait Strike extends Frame {
  override def tries: Int = 1
}
case class UnfinishedTwoBonusStrike() extends Strike {
  override def score: Int = 10
}
case class UnfinishedOneBonusStrike(bonus: Int) extends Strike {
  override def score: Int = 10 + bonus
}
case class FinishedStrike(bonus1: Int, bonus2: Int) extends Strike {
  override def score: Int = 10 + bonus1 + bonus2
}
case class UnfinishedFrame(firstScore: Int) extends Frame {
  override def score: Int = firstScore
  override def tries: Int = 1
}
case class FinishedFrame(firstScore: Int, secondScore: Int) extends Frame {
  override def score: Int = firstScore + secondScore
}
trait Spare extends Frame {
  override def tries: Int = 2
}
case class UnfinishedBonusSpare(firstScore: Int, secondScore: Int) extends Spare {
  override def score: Int = 10
  override def tries: Int = 2
}
case class FinishedSpare(firstScore: Int, secondScore: Int, bonus: Int) extends Spare {
  override def score: Int = 10 + bonus
}
trait LastFrame extends Frame
case class UnfinishedLastFrame(firstScore: Int) extends LastFrame {
  override def score: Int = firstScore
}
case class FinishedLastFrame(firstScore: Int, secondScore: Int) extends LastFrame {
  override def score: Int = firstScore + secondScore
}
case class UnfinishedBonusLastFrame(firstScore: Int, secondScore: Int) extends LastFrame {
  override def score: Int = firstScore  + secondScore
}
case class FinishedBonusLastFrame(firstScore: Int, secondScore: Int, thirdScore: Int) extends LastFrame {
  override def score: Int = firstScore + secondScore + thirdScore
  override def tries: Int = 3
}


object Frame {
  def apply(firstScore: Int): Frame = {
    if (firstScore == 10) UnfinishedTwoBonusStrike()
    else UnfinishedFrame(firstScore)
  }

  def apply(firstScore: Int, secondScore: Int): Frame = {
    if (firstScore + secondScore == 10) UnfinishedBonusSpare(firstScore, secondScore)
    else if (firstScore + secondScore > 10) throw new IllegalArgumentException("sum of pins in 2 rounds should be less than 10")
    else FinishedFrame(firstScore, secondScore)
  }
}

object LastFrame {
  def apply(firstScore: Int): LastFrame = {
    UnfinishedLastFrame(firstScore)
  }

  def apply(firstScore: Int, secondScore: Int): LastFrame = {
    if (firstScore < 10 && firstScore + secondScore > 10) throw new IllegalArgumentException("sum of pins in 2 rounds should be less than 10")
    if (firstScore == 10 || firstScore + secondScore == 10) UnfinishedBonusLastFrame(firstScore, secondScore)
    else FinishedLastFrame(firstScore, secondScore)
  }

  def apply(firstScore: Int, secondScore: Int, thirdScore: Int): LastFrame = {
    FinishedBonusLastFrame(firstScore, secondScore, thirdScore)
  }

}