trait Generic
case class Program(prg: List[Generic]) extends Generic

abstract class Operand
case class Variable(v: String) extends Operand with Generic
case class Integer(v: Int) extends Operand with Generic

abstract class Operation
case class Plus(operand: Operand) extends Operation with Generic
case class Minus(operand: Operand) extends Operation with Generic
case class Mult(operand: Operand) extends Operation with Generic
case class Div(operand: Operand) extends Operation with Generic
case class EqualTo(operand: Operand) extends Operation with Generic
case class GreaterThen(operand: Operand) extends Operation with Generic
case class Or(operand: Operand) extends Operation with Generic
case class And(operand: Operand) extends Operation with Generic

abstract class Statement
case class Printing(v: Variable) extends Statement with Generic
case class PrintingString(v: String) extends Statement with Generic
case class Declaration(k: Variable, v:Integer) extends Statement with Generic
case class IfStatement(v: Variable, statements: List[Generic]) extends Statement with Generic
case class IfElseStatement(v:Variable, ifStat: List[Generic], elseStat: List[Generic]) extends Statement with Generic
case class Assign(v: Variable, firstOperand: Operand, opList: List[Operation]) extends Statement with Generic
case class While(v: Variable, statements: List[Generic]) extends Statement with Generic
