//<<<<<<<<<<<<<-------------- Purpose of this program "Calculator" ---------------------->>>>>>>>>>>>>>>>>//
/*
-> works with rational and integers both
-> retains output of last operation so that output can be used for further calculation
*/

/*
What does the code of calculator contain??? ----------->>>>>>>>>>>>>>>

-> Rational class is defined where
1. one primary constructor with two parameters is defined (in declaration of Rational class itself)
2. one auxiliary constructor with one parameter is defined
3. two data members are defined "numer" (i.e. numerator) and "denom" (i.e. denominator)
4. inbuilt "require" method is used to check whether value entered by user for denominator is 0, if it is zero then exception is generated with message
5. gcd method is defined which receives two parameters of int type and returns one parameter of int type
6. method overloading is used to overload +,-,*,/ operators to work with both rational and integer numbers

-> object class "Calculator" is defined where
1. NumberChoice() method is defined which gives two options to user i.e.
   whether user wants to work with rational number or with integer (whole) number
   [NOTE -> user can enter + or - values both for rational and whole numbers]
2. makeRational() method is defined which creates a rational number if user chooses to work with rational number
2. Options() method is defined which provides options of operations to user i.e. add, subtract, multiply, divide
3. Compute() method is defined where
   input(s) from user is taken either for rational or for integer (whole) number
   call to overloaded =,-,*,/ method is made depending upon option of operations chosen by the user
4. inside main() method, do while loop is run until user presses 'y' to exit,
   otherwise user can continue to perform operation while retaining last value of operation
 */


import scala.io.StdIn._       //this import statement is required to use readInt(),readChar() methods

class Rational(n: Int, d: Int) {
    require(d!=0,"Denominator can't be zero")
    private val g = gcd(n.abs, d.abs)
    val numer = n / g
    val denom = d / g

    //auxiliary constructor
    def this(n: Int) = this(n, 1)

  //gcd method
  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

  //overloaded methods
    def + (that: Rational): Rational =
      new Rational(
        numer * that.denom + that.numer * denom,
        denom * that.denom
      )
    def + (i: Int): Rational =
      new Rational(numer + i * denom, denom)

    def - (that: Rational): Rational =
      new Rational(
        numer * that.denom - that.numer * denom,
        denom * that.denom
      )
    def - (i: Int): Rational =
      new Rational(numer - i * denom, denom)

    def * (that: Rational): Rational =
      new Rational(numer * that.numer, denom * that.denom)
    def * (i: Int): Rational =
      new Rational(numer * i, denom)

    def / (that: Rational): Rational =
      new Rational(numer * that.denom, denom * that.numer)
    def / (i: Int): Rational =
      new Rational(numer, denom * i)
}

object Calculator {

    //Accepts option from user to work either with rational or with whole (integer) number
    def NumberChoice() = {
      println("Please enter your choice....Which number you want to do calculations with....")
      println("1. Rational")
      println("2. Whole")
    }

    //creates a rational number
    def makeRational(rational:Rational):Rational = {
       println("Enter numerator and denominator : ")
          val p = readInt()
          val q = readInt()
          rational.+(new Rational(p, q))
    }

    //provides option of operations to user
    def Options() = {
      println("******** Calculating machine provides below operations *********")
      println(" ")
      println("1. Add a rational")
      println("2. Subtract a rational")
      println("3. Multiply a rational")
      println("4. Divide a rational")
      println("5. Add an integer")
      println("6. Subtract an integer")
      println("7. Multiply an integer")
      println("8. Divide an integer")
    }

    //depending upon option selected by user calls the overloaded method
    def Compute(rational: Rational, input: Int): Rational = {
      input match {
        case 1 =>
          println("Enter numerator and denominator of a rational number for addition : ")
          val p = readInt()
          val q = readInt()
          rational.+(new Rational(p, q))
        case 2 =>
          println("Enter numerator and denominator of a rational number for subtraction : ")
          val p = readInt()
          val q = readInt()
          rational.-(new Rational(p, q))
        case 3 =>
          //val p = scala.io.StdIn.readInt()
          //val q = scala.io.StdIn.readInt()
          println("Enter numerator and denominator of a rational number for multiplication")
          val p = readInt()
          val q = readInt()
          rational.*(new Rational(p, q))
        case 4 =>
          //val p = scala.io.StdIn.readInt()
          println("Enter numerator and denominator of a rational number for division")
          val p = readInt()
          val q = readInt()
          rational./(new Rational(p,q))
        case 5 =>
          //val p = scala.io.StdIn.readInt()
          println("Enter an integer for addition: ")
          val p = readInt()
          rational.+(new Rational(p))
        case 6 =>
          //val p = scala.io.StdIn.readInt()
          println("Enter an integer for subtraction : ")
          val p = readInt()
          rational.-(new Rational(p))
        case 7 =>
          //val p = scala.io.StdIn.readInt()
          println("Enter an integer for multiplication: ")
          val p = readInt()
          rational.*(new Rational(p))
        case 8 =>
          //val p = scala.io.StdIn.readInt()
          println("Enter a non-zero integer for division: ")
          val p = readInt()
          rational./(new Rational(p))
        case _ =>
          rational
      }
    }

    //runs loop until user types 'y' to exit
    def main(args: Array[String]): Unit = {

      var rationalNumber1:Rational = new Rational(0)
      var rationalNumber2:Rational = new Rational(0)
      var num = 0
      var choice1 = 0
      var choice2 = 0
      var ch = 'y'

      println("<<<<<<----- Calculating Machine ------>>>>>>")
      NumberChoice()
      choice1 = readInt()
      choice1 match {   //matching choice
        case 1 =>
          rationalNumber1 = makeRational(rationalNumber1)
          println("Rational Number is : "+rationalNumber1.toString)
          rationalNumber2 = rationalNumber1
        do {
          Options()
          println("Please enter any of the above options : ")
          choice2 = readInt()
          //rationalNumber2 = Compute(rationalNumber2, choice2)
          //rationalNumber2 = rationalNumber1
          rationalNumber2 = Compute(rationalNumber2, choice2)
          if(rationalNumber2.denom == 1)
            println("Output is : " +rationalNumber2.numer)
          else if(rationalNumber2.denom < 0)
            println("Output is : " + "-"+ rationalNumber2.numer+"/"+rationalNumber2.denom.abs)
          else
            //println("Output is : " + rationalNumber2.toString)
            println("Output is : " + rationalNumber2.numer+"/"+rationalNumber2.denom)
          println("Press y to exit..... else press any character to continue...")
          ch = readChar()
        }while (ch != 'y')

        case 2 =>
          println("Enter a number : ")
          num = readInt()
          rationalNumber2 = new Rational(num)
          do {
            Options()
            println("Please enter any of the above options : ")
            choice2 = readInt()
            rationalNumber2 = Compute(rationalNumber2, choice2)
            if(rationalNumber2.denom == 1)
              println("Output is : "+rationalNumber2.numer)
            else if(rationalNumber2.denom < 0)
              println("Output is : " + "-"+ rationalNumber2.numer+"/"+rationalNumber2.denom.abs)
            else
              //println("Output is : " + rationalNumber2.toString)
              println("Output is : " + rationalNumber2.numer+"/"+rationalNumber2.denom)
            println("Press y to exit..... else press any character to continue...")
            ch = readChar()
          }while (ch != 'y')

      }

    }

}
