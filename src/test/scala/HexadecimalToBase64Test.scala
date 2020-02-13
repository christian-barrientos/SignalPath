import org.scalatest.FunSuite

class HexadecimalToBase64Test extends FunSuite {

  test("Should return RXZpZGludA== for hexadecimalToBase64(45766964696e74)") {
    assert(HexadecimalToBase64.hexadecimalToBase64("45766964696e74") === "RXZpZGludA==")
  }

  test("Should return 6 for binaryToDecimal(110)") {
    assert(HexadecimalToBase64.binaryToDecimal("110") === "6")
  }

  test("Should return dA== for binaryToBase64(01110100)") {
    assert(HexadecimalToBase64.binaryToBase64("01110100") === "dA==")
  }

}
