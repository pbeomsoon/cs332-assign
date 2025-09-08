package recfun

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CountChangeSuite extends FunSuite {
  import Main.countChange
  test("countChange: example given in instructions") {
    assert(countChange(4,List(1,2)) === 3)
  }

  test("countChange: sorted CHF") {
    assert(countChange(300,List(5,10,20,50,100,200,500)) === 1022)
  }

  test("countChange: no pennies") {
    assert(countChange(301,List(5,10,20,50,100,200,500)) === 0)
  }

  test("countChange: unsorted CHF") {
    assert(countChange(300,List(500,5,50,100,20,200,10)) === 1022)
  }

  // 테스트 추가
  test("countChange: zero money") {
    assert(countChange(0, List(1,2,5)) === 1)
  }

  test("countChange: empty coins list") {
    assert(countChange(10, List()) === 0)
  }

  test("countChange: single coin multiple ways") {
    assert(countChange(5, List(1)) === 1)
  }

  test("countChange: case with multiple coins") {
    assert(countChange(25, List(1, 2, 5, 10, 20)) === 68)
  }
}
