sealed trait Opcode
case class MoveUp() extends Opcode
case class MoveDown() extends Opcode
case class MoveLeft() extends Opcode
case class MoveRight() extends Opcode
case class Wait() extends Opcode
case class Abort() extends Opcode
