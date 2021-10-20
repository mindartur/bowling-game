package bowling

sealed trait Frame {
  def score: Int
}
object Strike extends Frame {
  override def score: Int = 10
}
case class UnfinishedFrame(firstScore: Int) extends Frame {
  override def score: Int = firstScore
}
case class FinishedFrame(firstScore: Int, secondScore: Int) extends Frame {
  override def score: Int = firstScore + secondScore
}
case class Spare(firstScore: Int, secondScore: Int) extends Frame {
  override def score: Int = 10
}


object Frame {
  def apply(firstScore: Int): Frame = {
    if (firstScore == 10) Strike
    else UnfinishedFrame(firstScore)
  }

  def apply(firstScore: Int, secondScore: Int): Frame = {
    if (firstScore + secondScore == 10) Spare(firstScore, secondScore)
    else FinishedFrame(firstScore, secondScore)
  }
}