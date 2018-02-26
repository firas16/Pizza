package pizza

import pizza.Ingredient.Ingredient

object Ingredient extends Enumeration {
  type Ingredient = Value
  val Tomato, Mushroom = Value
}

case class Cell(x: Int, y: Int, ingredient: Ingredient, sliced: Boolean = false)

case class Pizza(cells: Array[Cell]){
  val nbTomato = cells.count(_.ingredient == Ingredient.Tomato)
  val nbMushroom = cells.count(_.ingredient == Ingredient.Mushroom)
}

case class Slice(row1: Int, col1: Int, row2: Int, col2: Int){

  val canExpandRow: Boolean = true
  val canExpandCol: Boolean = false
  def println = print(row1 + " => " + col1 + ";" + row2 + "=> " + col2)
}

object Slice {
  def apply(cell: Cell) = new Slice(cell.x, cell.y, cell.x, cell.y)
}