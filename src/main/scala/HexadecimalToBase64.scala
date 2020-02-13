import scala.collection.mutable

object  HexadecimalToBase64 extends App {
  val ENTER_VALID_INPUT_MESSAGE = "Please enter a valid hexadecimal number as a parameter"
  val MULTIPLE_PARAMETERS_IN_MESSAGE = "Only 1 parameter is accepted, the first parameter provided will be used and the rest will be ignored"
  val EXIT_WITH_ERROR_MESSAGE = "Program failed to run successfully, please verify your inputs"

  /** Given a hexadecimal number, this will convert it to base 64 and return it
   */
  def hexadecimalToBase64(hex: String): String = {
    val hexToBinaryRepresentation = mutable.HashMap("0"->"0000", "1"->"0001", "2"->"0010", "3"->"0011", "4"->"0100", "5"->"0101", "6"->"0110", "7"->"0111",
      "8"->"1000", "9"->"1001", "a"->"1010", "b"->"1011", "c"->"1100", "d"->"1101", "e"->"1110", "f"->"1111")
    var binaryRepresentation = ""
    //Get binary representation of hexadecimal number
    for(a <- 0 to hex.length - 1) {
      binaryRepresentation = binaryRepresentation + hexToBinaryRepresentation(hex(a).toString)
    }
    binaryToBase64(binaryRepresentation)
  }

  /** Given a binary number, this will convert it to decimal and return it
   */
  def binaryToDecimal(binary: String):String = {
    var decimalResult = 0
    for(i <- 0 to binary.length-1) {
      decimalResult = (decimalResult + scala.math.pow(2,i) * binary(binary.length - 1 - i).asDigit).toInt
    }
    decimalResult.toString
  }

  /** Given a binary number, this will convert it to base 64 and return it
   */
  def binaryToBase64(binaryRepresentation: String):String = {
    val decimalToBase64Representation  = mutable.HashMap("0"->"A", "1"->"B", "2"->"C", "3"->"D", "4"->"E", "5"->"F", "6"->"G", "7"->"H",
      "8"->"I", "9"->"J", "10"->"K", "11"->"L", "12"->"M", "13"->"N", "14"->"O", "15"->"P", "16"->"Q", "17"->"R", "18"->"S", "19"->"T",
      "20"->"U", "21"->"V", "22"->"W", "23"->"X", "24"->"Y", "25"->"Z", "26"->"a", "27"->"b", "28"->"c", "29"->"d", "30"->"e", "31"->"f",
      "32"->"g", "33"->"h", "34"->"i", "35"->"j", "36"->"k", "37"->"l", "38"->"m", "39"->"n", "40"->"o", "41"->"p", "42"->"q", "43"->"r",
      "44"->"s", "45"->"t", "46"->"u", "47"->"v", "48"->"w", "49"->"x", "50"->"y", "51"->"z", "52"->"0", "53"->"1", "54"->"2", "55"->"3",
      "56"->"4", "57"->"5", "58"->"6", "59"->"7", "60"->"8", "61"->"9", "62"->"+", "63"->"/")
    var base64Representation = ""
    //Split binary representation in 6 character and start adding to base 64 representation
    var counter = 0;
    while(counter < binaryRepresentation.length-1) {
      //If we have a binary number not divisible by 6, we add 0 until it is, and add the = special character
      if(counter + 6 > binaryRepresentation.length-1){
        var binaryRepresentationInLoop = binaryRepresentation.substring(counter, binaryRepresentation.length)
        var charactersAdded = 0;
        while(binaryRepresentationInLoop.length !=6) {
          binaryRepresentationInLoop = binaryRepresentationInLoop + "0"
          charactersAdded+=1
        }
        val decimalRepresentationInLoop = binaryToDecimal(binaryRepresentationInLoop)
        base64Representation = base64Representation + decimalToBase64Representation(decimalRepresentationInLoop)
        if(charactersAdded == 2) {
          base64Representation = base64Representation + "="
        }
        else {
          base64Representation = base64Representation + "=="
        }
      }
      else {
        val binaryRepresentationInLoop = binaryRepresentation.substring(counter, counter + 6)
        val decimalRepresentationInLoop = binaryToDecimal(binaryRepresentationInLoop)
        base64Representation = base64Representation + decimalToBase64Representation(decimalRepresentationInLoop)
      }
      counter+= 6
    }
    base64Representation
  }

  def validateInput(arguments: Array[String]): Boolean = {
    var validInput = true
    //Verify that an argument was passed in and that it is a valid int between 0 and 90
    if (arguments.isEmpty) {
      println(ENTER_VALID_INPUT_MESSAGE)
      validInput = false
    }
    else if (arguments.length > 1) {
      println(MULTIPLE_PARAMETERS_IN_MESSAGE)
    }
    validInput
  }
  //Run our method if our input is valid
  if (validateInput(args)) {
    println(hexadecimalToBase64(args(0).trim()))
  }
  //Exit gracefully with an error message in the case of invalid input
  else {
    println(EXIT_WITH_ERROR_MESSAGE)
  }
}
