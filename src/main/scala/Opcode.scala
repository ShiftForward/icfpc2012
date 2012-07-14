sealed case class Opcode()
case class MoveUp() extends Opcode {
  override def toString = "U"
}
case class MoveDown() extends Opcode {
  override def toString = "D"
}
case class MoveLeft() extends Opcode {
  override def toString = "L"
}
case class MoveRight() extends Opcode {
  override def toString = "R"
}
case class Wait() extends Opcode {
  override def toString = "W"
}
case class Abort() extends Opcode {
  override def toString = "A"
}

sealed trait Status
case class Playing() extends Status
case class Win() extends Status
case class Lost() extends Status
