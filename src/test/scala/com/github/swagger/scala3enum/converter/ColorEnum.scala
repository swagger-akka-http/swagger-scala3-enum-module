package com.github.swagger.scala3enum.converter

enum ColorEnum { case Red, Green, Blue }

case class Colors(set: Set[ColorEnum])
