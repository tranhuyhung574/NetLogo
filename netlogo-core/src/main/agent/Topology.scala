// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.agent

import org.nlogo.api, api.AgentException
import java.util.ArrayList

@annotation.strictfp
object Topology {

  // factory method
  def get(world: World2D, xWraps: Boolean, yWraps: Boolean): Topology =
    (xWraps, yWraps) match {
      case (true , true ) => new Torus(world)
      case (true , false) => new VertCylinder(world)
      case (false, true ) => new HorizCylinder(world)
      case (false, false) => new Box(world)
    }

  // General wrapping function.
  def wrap(pos: Double, min: Double, max: Double): Double =
    if (pos >= max)
      min + ((pos - max) % (max - min))
    else if (pos < min) {
      val result = max - ((min - pos) % (max - min))
      // careful, if d is infinitesimal, then (max - d) might actually equal max!
      // but we must return an answer which is strictly less than max - ST 7/20/10
      if (result < max)
        result else min
    }
    else pos

  // for when you have a pcor and/or an offset and want to
  // wrap without converting to a double.
  def wrapPcor(pos: Int, min: Int, max: Int): Int =
    if (pos > max)
      min + ((pos - min) % (1 + max - min))
    else if (pos < min) // NOTE: we assume that min <= 0 here, since the world must always contain (0, 0)
      ((pos - max) % (1 + max - min)) + max
    else
      pos

  // assume no wrapping for now.
  def getRegion(x: Int, y: Int, r: Int, w: Int, h: Int, xWraps: Boolean, yWraps: Boolean): ArrayList[(Int, Int)] = {
    val ans: ArrayList[(Int, Int)] = new ArrayList()
    var (start, end, xx, yy) = (-1, -1, -1, -1)

    for (i <- 0 until h * w) {
      xx = i % w
      yy = i / w

      if (!(xx == x && yy == y) &&
        (Math.abs(x - xx) <= r || (xWraps && ((xx <= (x + r) % w) || (xx >= w - r + x)))) &&
        (Math.abs(y - yy) <= r || (yWraps && ((yy <= (y + r) % h) || (yy >= h - r + y))))) {
        if (start == -1) {
          start = i
          end = i + 1
        } else {
          end += 1
        }

      } else if (start != -1) {
        ans.add((start, end))

        start = -1
        end = -1
      }
    }

    if (start != -1) {
      ans.add((start, end))
    }

    ans
  }
}

abstract class Topology(val world: World, val xWraps: Boolean, val yWraps: Boolean)
extends Neighbors {

  @throws(classOf[AgentException])
  def wrapX(x: Double): Double
  @throws(classOf[AgentException])
  def wrapY(y: Double): Double

  def distanceWrap(dx: Double, dy: Double, x1: Double, y1: Double, x2: Double, y2: Double): Double
  def towardsWrap(headingX: Double, headingY: Double): Double

  def shortestPathX(x1: Double, x2: Double): Double
  def shortestPathY(y1: Double, y2: Double): Double

  ///

  def followOffsetX: Double = world.observer.followOffsetX
  def followOffsetY: Double = world.observer.followOffsetY

  @throws(classOf[AgentException])
  @throws(classOf[PatchException])
  def diffuse(amount: Double, vn: Int)

  @throws(classOf[AgentException])
  @throws(classOf[PatchException])
  def diffuse4(amount: Double, vn: Int)

  // getPatch methods.  These are here so they can be called by subclasses in their implementations
  // of getPN, getPS, etc.  They provide the usual torus-style behavior.  It's a little odd that
  // they're here rather than in Torus, but doing it that way would have involved other
  // awkwardnesses -- not clear to me right now (ST) what the best way to setup this up would be.
  // One suboptimal thing about how it's set up right now is that e.g. in subclass methods like
  // Box.getPN, the source.pycor gets tested once, and then if Box.getPN calls
  // Topology.getPatchNorth, then source.pycor gets redundantly tested again.
  // - JD, ST 6/3/04

}
