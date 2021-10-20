package bowling


class Game (private var frames: List[Frame]) {

  def recordThrow(pins: Int): Game = {
    frames match {
      case List() => new Game(frames :+ Frame(pins))
      case firstFrames :+ lastFrame => lastFrame match {
        case UnfinishedFrame(firstScore) => new Game(firstFrames :+ Frame(firstScore, pins))
        case _ => new Game(frames :+ Frame(pins))
      }
    }
  }

  def score: Int = {
    frames.map(_.score).sum
  }


}


object Game {
  def apply(): Game = {
    new Game(List())
  }
}
