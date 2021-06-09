package com.fd.modelling.day1.session3

object SumTypes {
  sealed trait ColorModel

  case class RedComponent(value: Int) extends AnyVal
  object RedComponent {
    val max: RedComponent = RedComponent(256)
    val min: RedComponent = RedComponent(0)
  }
  case class GreenComponent(value: Int) extends AnyVal
  object GreenComponent {
    val max: GreenComponent = GreenComponent(256)
    val min: GreenComponent = GreenComponent(0)
  }
  case class BlueComponent(value: Int) extends AnyVal
  object BlueComponent {
    val max: BlueComponent = BlueComponent(256)
    val min: BlueComponent = BlueComponent(0)
  }

  case class RGB(red: RedComponent, green: GreenComponent, blue: BlueComponent)
      extends ColorModel {
    override def toString: String =
      s"rgb(${red.value}, ${green.value}, ${blue.value})"
  }

  case class CyanComponent(value: Int) extends AnyVal
  case class MagentaComponent(value: Int) extends AnyVal
  case class YellowComponent(value: Int) extends AnyVal
  case class KeyComponent(value: Int) extends AnyVal

  case class CMYK(
      cyan: CyanComponent,
      magenta: MagentaComponent,
      yellow: YellowComponent,
      key: KeyComponent
  ) extends ColorModel {
    override def toString: String =
      s"cmyk(${cyan.value}, ${magenta.value}, $yellow, $key)"
  }

  case class Hue(value: Int) extends AnyVal
  case class Saturation(value: Int) extends AnyVal
  case class Brightness(value: Int) extends AnyVal

  case class HSB(
      hue: Hue,
      saturation: Saturation,
      brightness: Brightness
  ) extends ColorModel {
    override def toString: String =
      s"hsb(${hue.value}, ${saturation.value}, ${brightness.value})"
  }
}
