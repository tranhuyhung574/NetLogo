// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.agent

import java.util.ArrayList

import org.scalatest.FunSuite
import Topology.getRegion

class GetRegionTests extends FunSuite {

  def toArrayList(l: List[(Int, Int)]): ArrayList[(Int, Int)] = {
    val res = new ArrayList[(Int, Int)]
    l.foreach { res.add }
    res
  }

  // ticket #1038
  test("getRegion no wrap") {
    assertResult(toArrayList(List((1, 3), (4, 5), (7, 9)))) (getRegion(2, 1, 1, 3, 3, false, false))
    assertResult(toArrayList(List((0, 5), (6, 9)))) (getRegion(2, 1, 2, 3, 3, false, false))
    assertResult(toArrayList(List(
      (2, 6), (8, 12), (14, 16), (17, 18), (20, 24), (26, 30)
    ))) (getRegion(4, 2, 2, 6, 6, false, false))

    assertResult(toArrayList(List((1, 3), (4, 5)))) (getRegion(2, 1, 1, 3, 2, false, false))
  }

  test("getRegion with xWrap") {
    assertResult(toArrayList(List(
      (0,1), (2, 7), (8, 13), (14, 16), (17, 19), (20, 25), (26, 31)
    ))) (getRegion(4, 2, 2, 6, 6, true, false))

    assertResult(toArrayList(List(
      (0,1), (3,5), (6,7)
    ))) (getRegion(5, 0, 2, 7, 1, true, false))

    assertResult(toArrayList(List(
      (3,5), (6,7)
    ))) (getRegion(0, 5, 2, 1, 7, true, false))

    assertResult(toArrayList(List(
      (0,1), (2,4), (6,7)
    ))) (getRegion(1, 0, 2, 7, 1, true, false))
  }

  test("getRegion with yWrap") {
    assertResult(toArrayList(List(
      (0,1), (3,5), (6,7)
    ))) (getRegion(0, 5, 2, 1, 7, false, true))

  }

  test("getRegion with all wrap") {
    assertResult(toArrayList(List((0,5), (6,9)))) (getRegion(2, 1, 1, 3, 3, true, true))
    assertResult(toArrayList(List((0,5), (6,9)))) (getRegion(2, 1, 2, 3, 3, true, true))

    assertResult(toArrayList(List(
      (0,1), (2, 7), (8, 13), (14, 16), (17, 19), (20, 25), (26, 31)
    ))) (getRegion(4, 2, 2, 6, 6, true, true))

    assertResult(toArrayList(List((0, 5)))) (getRegion(2, 1, 1, 3, 2, true, true))
  }

}
