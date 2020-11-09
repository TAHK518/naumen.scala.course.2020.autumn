trait Cell{
  def toString: String
}

class EmptyCell extends Cell {
  override def toString: String = "empty"
}

class NumberCell (number: Int) extends Cell {
  override def toString: String = number.toString
}

class StringCell (str: String) extends Cell {
  override def toString: String = str
}

class ReferenceCell (ix: Int, iy: Int, val table: Table) extends Cell {
  override def toString: String = getCellStringValue

  def getValue: Option[Cell] = table.getCell(ix, iy)

  private def getCellStringValue: String = {
    val referenceCell = table.getCell(ix, iy)

    if (referenceCell.isEmpty)
      return "outOfRange"

    referenceCell match {
      case Some(refCell: ReferenceCell) =>
        if (refCell.getValue.get == this) "cyclic"
        else refCell.toString
      case _ => referenceCell.get.toString
    }
  }
}
