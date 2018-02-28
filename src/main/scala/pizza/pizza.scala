package pizza

import pizza.Ingredient.Ingredient

object Ingredient extends Enumeration {
  type Ingredient = Value
  val Tomato, Mushroom = Value
}

case class Cell(x: Int, y: Int, ingredient: Ingredient, sliced: Boolean = false)

case class Pizza(cells: Array[Cell], nRows: Int, nCols: Int){
  val nbTomato = cells.count(_.ingredient == Ingredient.Tomato)
  val nbMushroom = cells.count(_.ingredient == Ingredient.Mushroom)
  val nbRows = nRows
  val nbCols = nCols
}

case class Slice(row1: Int, col1: Int, row2: Int, col2: Int){

  val canExpandRow: Boolean = true
  val canExpandCol: Boolean = false
  def println = print(row1 + " => " + col1 + ";" + row2 + "=> " + col2)
  override def toString = row1.toString + " " + col1.toString + " " + row2.toString + " " + col2.toString
}

object Slice {
  def apply(cell: Cell) = new Slice(cell.x, cell.y, cell.x, cell.y)
}