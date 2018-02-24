package pizza

object Strategy {

  val l = 250
  val h = 250
  def run(pizza: Pizza, l: Int, h: Int): List[Slice] = {

      val nbMushroom = pizza.cells.map(_.ingredient).count(ingredient => ingredient == Ingredient.Mushroom)
      val nbTomate = pizza.cells.map(_.ingredient).count(ingredient => ingredient == Ingredient.Tomato)
      val maxSlices = Math.min(nbTomate, nbMushroom)/l
      val matrix = Array.ofDim[Cell](3,3)
    ???
  }
  def isSliceCorrect(slice: Slice, l: Int, h: Int): Boolean = ???

  def findNotSlicedCell(pizza: Pizza): Cell = ???

  def expandSlice(pizza: Pizza, slice: Slice): Slice = {
    val lenghtRow = slice.row2 - slice.row1
    val lengthCol = slice.col2 - slice.col1
    val maxToAdd = h - lenghtRow * lengthCol
    if(maxToAdd <= 0)
      slice
    else if (maxToAdd >= lenghtRow){

    }
    ???
  }

  def findCorrectSlice(pizza: Pizza, nbTomato: Int, nbMushroom: Int, slice: Slice = null): Slice = {
    if(nbTomato == nbMushroom == 0)
      slice
    else if(nbTomato == l == nbMushroom) {
      val firstCell = findNotSlicedCell(pizza)
      firstCell.ingredient match {
        case Ingredient.Mushroom =>
          findCorrectSlice(pizza, nbTomato, nbMushroom-1, Slice(firstCell.x, firstCell.y, firstCell.x, firstCell.y))
        case Ingredient.Tomato =>
          findCorrectSlice(pizza, nbTomato-1, nbMushroom, Slice(firstCell.x, firstCell.y, firstCell.x, firstCell.y))
      }
    }
    else {

      ???
    }

    ???
  }

}
