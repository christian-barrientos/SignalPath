import org.scalatest.FunSuite

class SpecialMathTest extends FunSuite{
  test("Should return 0 for specialMathIterative(0)") {
    assert(SpecialMath.specialMathIterative(0) === 0)
  }

  test("Should return 1 for specialMathIterative(0)") {
    assert(SpecialMath.specialMathIterative(1) === 1)
  }

  test("Should return 1293530146158671458 for specialMathIterative(90)") {
    assert(SpecialMath.specialMathIterative(90) === 1293530146158671458L)
  }

  test("Should successfully validate input") {
    assert(SpecialMath.validateInput(Array("23")) === true)
  }

  test("Should fail to validate empty input") {
    assert(SpecialMath.validateInput(Array()) === false)
  }

  test("Should fail to validate input of negative number") {
    assert(SpecialMath.validateInput(Array("-9")) === false)
  }

  test("Should fail to validate input of non int number") {
    assert(SpecialMath.validateInput(Array("9.0")) === false)
  }

  test("Should fail to validate input higher than 90") {
    assert(SpecialMath.validateInput(Array("1000")) === false)
  }
}
