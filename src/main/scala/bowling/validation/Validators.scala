package bowling.validation

object Validators {
  def validatePins(pins: Int): Unit = {
    if (pins < 0 || pins > 10) throw new IllegalArgumentException("pins should be between 0 and 10")
  }
}
