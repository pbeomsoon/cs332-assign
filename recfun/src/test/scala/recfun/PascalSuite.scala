package recfun

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PascalSuite extends FunSuite {
  import Main.pascal
  test("pascal: col=0,row=2") {
    assert(pascal(0,2) === 1)
  }

  test("pascal: col=1,row=2") {
    assert(pascal(1,2) === 2)
  }

  test("pascal: col=1,row=3") {
    assert(pascal(1,3) === 3)
  }

  // 테스트 추가
  test("pascal: col=0,row=0") {
    assert(pascal(0,0) === 1)
  }

  test("pascal: col=row,row=4") {
    assert(pascal(4,4) === 1)
  }

  test("pascal: col=2,row=4") {
    assert(pascal(2,4) === 6)
  }

  test("pascal: col=1,row=4") {
    assert(pascal(1,4) === 4)
  }

  test("pascal: col=3,row=5") {
    assert(pascal(3,5) === 10)
  }

  test("pascal: col=9,row=15") {
    assert(pascal(9,15) === 5005)
  }
}
