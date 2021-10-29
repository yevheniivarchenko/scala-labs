package recfun

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CiSuite extends AnyFunSuite {
  import Main.ci

  test("c: a=4,x=2,k=13 (x < a)") {
    assert(ci(4,2,13) === 65528)
  }

  test("c: a=7,x=3,k=13 (x < a)") {
    assert(ci(7,3,13) === 16740381)
  }

  test("c: a=2,x=3,k=13 (x > a)") {
    assert(ci(2,3,13) === 2)
  }

  test("c: a=1,x=1,k=13 (x = a)") {
    assert(ci(1,1,13) === 0)
  }
}

