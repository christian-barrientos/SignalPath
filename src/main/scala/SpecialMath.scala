import scala.collection.mutable.ListBuffer

object SpecialMath extends App {
  var ENTER_VALID_INPUT_MESSAGE = "Please enter a valid positive int number, from 0 to 90, as a parameter"
  var MULTIPLE_PARAMETERS_IN_MESSAGE = "Only 1 parameter is accepted, the first parameter provided will be used and the rest will be ignored"
  var EXIT_WITH_ERROR_MESSAGE = "Program failed to run successfully, please verify error messages"

  /** Returns the result (long) of the following function:
   * f(n) = n + f(n-1) + f(n-2)
   *
   * Iterative approach, since a recursive approach would cause
   * a stack overflow when a big (~90) number is passed in
   */
  def specialMathIterative(n: Int): Long = {
    //Create a list to store all f(n), starting from the 2 base cases
    var specialMathResults = new ListBuffer[Long]()
    specialMathResults += 0
    specialMathResults += 1
    for (a <- 2 to n) {
      //Add to the list that holds all solutions, using the ones already there in the previous 2 indexes
      specialMathResults += a + specialMathResults(a - 1) + specialMathResults(a - 2)
    }
    //Return the solution stored at n
    specialMathResults(n)
  }

  /** Does some simple validation on the input, limiting the input to be
   * a positive number from 0 to 90
   *
   * Decided to limit the input to 90, since anything over that will cause an overflow
   */
  def validateInput(arguments: Array[String]): Boolean = {
    var validInput = true
    //Verify that an argument was passed in and that it is a valid int between 0 and 90
    if (arguments.isEmpty || arguments(0).toIntOption.isEmpty || arguments(0).toInt < 0 || arguments(0).toInt > 90) {
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
    println(specialMathIterative(args(0).toInt))
  }
  //Exit gracefully with an error message in the case of invalid input
  else {
    println(EXIT_WITH_ERROR_MESSAGE)
  }
}

