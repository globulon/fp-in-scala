package com.promindis.fp

case class Box(height: Double, width: Double)

object Box {
  private def greaterBy(a: Box, b: Box, f: Box => Double) = if (f(a) > f(b)) a else b

  val wider : (Box, Box) => Box = greaterBy(_, _, p => p.width)

  val taller : (Box, Box) => Box = greaterBy(_, _, p => p.height)
}
