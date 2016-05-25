object ArnoldCBrain{
  var variables = collection.mutable.Map[String, Int]()

  def evalProgram(instructions: List[Generic]): Unit = {
    instructions.foreach { instruction =>
      instruction match {
        case Printing(v) => println(variables(v.v))
        case PrintingString(v) => println(v)
        case Declaration(variable, value) => variables(variable.v) = evalOperand(value)
        case IfElseStatement(v, ifStat, elseStat) => if(evalOperand(v) == 1) evalProgram(ifStat) else evalProgram(elseStat)
        case IfStatement(v, statements) => if(evalOperand(v) == 1) evalProgram(statements)
        case While(v, statements) =>  while(evalOperand(v) != 0){ evalProgram(statements) }
        case Assign(v, firstOperand, opList) => evalAssign(v, firstOperand, opList)
      }
    }
  }

  def evalOperand(op: Operand) = {
    op match {
      case Variable(v) => variables(v)
      case Integer(v) => v
    }
  }

  /*def evalOperation(a: Int, op: Operation) = {
    op match {
      case Plus(o) => a + evalOperand(o)
      case Minus(o) => a - evalOperand(o)
      case Mult(o) => a * evalOperand(o)
      case Div(o) => a / evalOperand(o)
      case EqualTo(o) => if(a == evalOperand(o)) 1 else 0
      case GreaterThen(o) => if(a > evalOperand(o)) 1 else 0
      case Or(o) => if(a || evalOperand(o)) 1 else 0
      case And(o) => if(a && evalOperand(o)) 1 else 0
    }
  } */

  def evalAssign(v: Variable, firstOperand: Operand, opList: List[Operation]) = {
    var acc = evalOperand(firstOperand)
    opList.foreach { op =>
      op match {
        case Plus(o) => acc += evalOperand(o)
        case Minus(o) => acc -= evalOperand(o)
        case Mult(o) => acc *= evalOperand(o)
        case Div(o) => acc /= evalOperand(o)
        case EqualTo(o) => if(acc == evalOperand(o)) acc = 1 else acc = 0
        case GreaterThen(o) => if(acc > evalOperand(o)) acc = 1 else acc = 0
        case Or(o) => if(toBool(acc) || toBool(evalOperand(o))) acc = 1 else acc = 0
        case And(o) => if(toBool(acc) && toBool(evalOperand(o))) acc = 1 else acc = 0
      }
    }
    variables(v.v) = acc
  }

  def toBool(x: Int): Boolean = x != 0
}
