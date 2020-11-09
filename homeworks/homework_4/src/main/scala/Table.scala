import scala.collection.mutable

class Table (height:Int , width:Int) {
  private val table = mutable.Map[Int, Cell]().withDefaultValue(new EmptyCell)

  def getCell (ix: Int, iy: Int): Option[Cell] = {
    if(isOutOfRange(ix, iy)) return None
    Option(table(toLinearIndex(ix, iy)))
  }

  def setCell (ix: Int, iy: Int, cell: Cell): Unit = {
    if (!isOutOfRange(ix, iy))
      table(toLinearIndex(ix, iy)) = cell
  }

  private def isOutOfRange(ix: Int, iy: Int) = ix < 0 || ix >= width || iy < 0 || iy >= height
  private def toLinearIndex(ix: Int, iy: Int) = ix + iy * width
}
