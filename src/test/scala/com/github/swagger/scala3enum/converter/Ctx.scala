package com.github.swagger.scala3enum.converter

object Ctx {
  enum ColorEnum { case Red, Green, Blue }
}

case class CtxCar(make: String, color: Ctx.ColorEnum)
