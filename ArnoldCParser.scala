import scala.util.parsing.combinator._

class ArnoldCParser extends JavaTokenParsers {

  def program = "IT'S SHOWTIME"~>
                rep(statement)<~
                "YOU HAVE BEEN TERMINATED" ^^ { case x => x }

  def statement: Parser[Generic] = (assigning | printing | declaration | ifElseStatement | ifStatement | whileStatement)

  def printing: Parser[Generic] = "TALK TO THE HAND" ~> (stringLiteral | variable) ^^ {
    case v: Variable => Printing(v)
    case v: String => PrintingString(v.toString.slice(1, v.toString.length-1))
  }
  def declaration: Parser[Generic] = "HEY CHRISTMAS TREE" ~ variable ~ "YOU SET US UP"  ~ value ^^ {
    case _ ~ variable ~ _ ~ value => Declaration(variable, value)
  }

  def assigning: Parser[Generic] = ("GET TO THE CHOPPER" ~ variable ~
                  "HERE IS MY INVITATION" ~ operand ~
                  rep(operations) ~ "ENOUGH TALK") ^^ { case _ ~ v ~ _ ~ firstOperand ~ opList ~ _ => Assign(v, firstOperand, opList) }

  def operations = ( plus | minus | mult | div | equalTo | greaterThen | or | and )

  def plus = "GET UP" ~> operand ^^ {case op=> Plus(op)}
  def minus = "GET DOWN" ~> operand ^^ {case op=> Minus(op)}
  def mult = "YOU'RE FIRED" ~> operand ^^ {case op=> Mult(op)}
  def div = "HE HAD TO SPLIT" ~> operand ^^ {case op=> Div(op)}

  def equalTo = "YOU ARE NOT YOU YOU ARE ME" ~> operand ^^ {case op=> EqualTo(op)}
  def greaterThen = "LET OFF SOME STEAM BENNET" ~> operand ^^ {case op => GreaterThen(op)}
  def or = "CONSIDER THAT A DIVORCE" ~> operand ^^ {case op=> Or(op)}
  def and = "KNOCK KNOCK" ~> operand ^^ {case op => And(op)}

  def ifStatement = ("BECAUSE I'M GOING TO SAY PLEASE" ~ variable ~ rep(statement) ~ "YOU HAVE NO RESPECT FOR LOGIC") ^^ {
      case _ ~ variable ~ statement_list ~ _ => IfStatement(variable, statement_list)
    }

  def ifElseStatement: Parser[Generic] = "BECAUSE I'M GOING TO SAY PLEASE" ~ variable ~ rep(statement)~ "BULLSHIT"~ rep(statement)~
                        "YOU HAVE NO RESPECT FOR LOGIC" ^^ {
   case _ ~ variable ~ ifStatements ~ _ ~ elseStatements ~ _ => IfElseStatement(variable, ifStatements, elseStatements)
 }

  def whileStatement: Parser[Generic] = "STICK AROUND" ~ variable ~ rep(statement) ~ "CHILL" ^^ {
    case _ ~ v ~ statements ~ _ => While(v, statements)
  }

  def operand = value | variable
  def variable = """[A-Za-z0-9]+""".r ^^ { case v => Variable(v)}
  def value = ("-".? ~ decimalNumber)  ^^ {
    case Some("-") ~ i => Integer(i.toInt * (-1))
    case _ ~ i => Integer(i.toInt)
  }

  def eval(toEval:String) = {
    parseAll(program, toEval) match{
      case Success(result, _) => ArnoldCBrain.evalProgram(result)
      case x => println(x)
    }
  }

}
