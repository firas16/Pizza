package pizza

import pizza.Ingredient.Ingredient

object Ingredient extends Enumeration {
  type Ingredient = Value
  val Tomato, Mushroom = Value
}
case class Cell(x: Int, y: Int, ingredient: Ingredient, sliced: Boolean = false)
case class Pizza(nbRows: Int, nbCols: Int, cells: List[Cell])
case class Slice(row1: Int, col1: Int, row2: Int, col2: Int){

  val canExpandRow: Boolean = true
  val canExpandCol: Boolean = false
}
