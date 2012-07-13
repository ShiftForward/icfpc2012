sealed trait Opcode
case class Up() extends Opcode
case class Down() extends Opcode
case class Left() extends Opcode
case class Right() extends Opcode
case class Wait() extends Opcode
case class Abort() extends Opcode

object VM {
  def eval(o: Opcode, b: Board): Board = {
    b
  }

}
