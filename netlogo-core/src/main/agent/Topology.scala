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
  @scala.inline
  private final def getRegionRow(x: Int, r: Int, offset: Int, arr: ArrayList[(Int, Int)]): Unit = {

    val low_within = x-r >= 0
    val high_within = x+r <= world.worldWidth-1

    if (low_within && high_within) {
      mergeAdd((offset + x-r, offset + x+r+1), arr)

    } else if (!low_within && !high_within) {
      mergeAdd((offset + 0, offset + world.worldWidth), arr)

    } else if (!low_within) {
      mergeAdd((offset + 0, offset + x + r + 1), arr)
      if (xWraps) {
        mergeAdd((offset + world.worldWidth + x - r, offset + world.worldWidth), arr)
      }

    } else { // !high_within
      if (xWraps) {
        mergeAdd((offset + 0, offset + x + r - world.worldWidth + 1), arr)
      }
      mergeAdd((offset + x - r, offset + world.worldWidth), arr)
    }

  }

  @scala.inline
  private final def mergeAdd(value: (Int, Int), arr: ArrayList[(Int, Int)]): Unit = {
    val s = arr.size()
    if (s == 0 || arr.get(s - 1)._2 < value._1) {
      arr.add(value)
    } else {
      val last = arr.get(s - 1)
      arr.set(s - 1, (last._1.min(value._1), value._2.max(last._2)))
    }
  }

  // 4 cases:
  // 1. x-r >=0 and x + r <= w-1
  // 2. x-r < 0 && x+r >= w
  // 3. x-r < 0
  // 4. x+r >= w
  
  def getRegion(X: Double, Y: Double, R: Double): ArrayList[(Int, Int)] = {

    val x: Int = X.toInt - world.minPxcor
    val y: Int = world.worldHeight - 1 - (Y.toInt - world.minPycor)
    val r: Int = if (X.toInt != X || Y.toInt != Y) R.toInt + 1 else R.toInt

    val ans: ArrayList[(Int, Int)] = new ArrayList()
    val a = Array((-1, -1), (-1, -1)) // at most 2 possible disjoint y ranges

    val low_within = y-r >= 0
    val high_within = y+r <= world.worldHeight-1

    if (low_within && high_within) { // y range is completely within world
      a(0) = (y-r, y+r+1)

    } else if (!low_within && !high_within) {
      a(0) = (0, world.worldHeight)

    } else if (low_within) { // low_within && !high_within
      val b = (y-r, world.worldHeight)
      if (yWraps) {
        a(0) = (0, y + r - world.worldHeight + 1)
        a(1) = b
      } else {
        a(0) = b
      }
    } else { // !low_within && high_within
      a(0) = (0, y+r+1)
      if (yWraps) {
        a(1) = (world.worldHeight + y - r, world.worldHeight)
      }
    }

    var i = a(0)._1
    while (i < a(0)._2) {
      getRegionRow(x, r, i * world.worldWidth, ans)
      i += 1
    }

    if (a(1)._1 > -1) {
      i = a(1)._1
      while (i < a(1)._2) {
        getRegionRow(x, r, i * world.worldWidth, ans)
        i += 1
      }
    }

    ans
  }

}
