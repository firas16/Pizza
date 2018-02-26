package pizza

object Strategy {

  val l = 1
  val h = 6
  def run(pizza: Pizza, l: Int, h: Int): List[Slice] = {

    val nbMushroom = pizza.cells.map(_.ingredient).count(ingredient => ingredient == Ingredient.Mushroom)
    val nbTomate = pizza.cells.map(_.ingredient).count(ingredient => ingredient == Ingredient.Tomato)
    val maxSlices = Math.min(nbTomate, nbMushroom) / l

    val cell = findNotSlicedCell(pizza)
    val slice = findCorrectSlice(pizza, l, l, Slice(cell))
    slice match {
      case Some(sl) =>
        val pizzaUpdated = updatePizza(pizza, Some(sl).get)
        return slice.get :: run(pizzaUpdated, l, h)
      case None => List.empty[Slice]
    }
  }

  def isSliceCorrect(slice: Slice, pizza: Pizza): Boolean = {

    val cellsMorceau = getSliceCells(pizza, slice)
    val pizzaMorceau = Pizza(cellsMorceau.toArray)
    val nbTomato = pizzaMorceau.nbTomato
    val nbMushroom = pizzaMorceau.nbMushroom
    return (nbTomato >= l && nbMushroom >= l && nbTomato+nbMushroom <= h)

  }

  def findNotSlicedCell(pizza: Pizza): Cell = pizza.cells.find(!_.sliced).get

  def expandSlice(pizza: Pizza, slice: Slice): Slice = {
    val lenghtRow = slice.row2 - slice.row1
    val lengthCol = slice.col2 - slice.col1
    val maxToAdd = h - lenghtRow * lengthCol
    if(maxToAdd <= 0)
      return slice

    if(isSliceCorrect(Slice(slice.row1-1, slice.col1, slice.row2, slice.col2), pizza))
      Slice(slice.row1-1, slice.col1, slice.row2, slice.col2)
    else  if(isSliceCorrect(Slice(slice.row1, slice.col1-1, slice.row2, slice.col2), pizza))
      Slice(slice.row1, slice.col1-1, slice.row2, slice.col2)
    else  if(isSliceCorrect(Slice(slice.row1, slice.col1, slice.row2+1, slice.col2), pizza))
      Slice(slice.row1, slice.col1, slice.row2+1, slice.col2)
    else  if(isSliceCorrect(Slice(slice.row1, slice.col1, slice.row2, slice.col2+1), pizza))
      Slice(slice.row1, slice.col1, slice.row2, slice.col2+1)
    else
      slice
    }

  def updatePizza(pizza: Pizza, slice: Slice): Pizza = ???

  def getSliceCells(pizza: Pizza, slice: Slice): List[Cell] = {
    pizza.cells.filter(cell =>
      cell.x >= slice.row1 &&
        cell.x <= slice.row2 &&
        cell.y >= slice.col1 &&
        cell.y <= slice.col2).toList
  }

  def findCorrectSlice(pizza: Pizza, nbTomato: Int, nbMushroom: Int, slice: Slice = null): Option[Slice] = {
    if(nbTomato == 0 && nbMushroom == 0)
      Some(slice)
    else if(nbTomato == l && l == nbMushroom) {
      val firstCell = findNotSlicedCell(pizza)
      firstCell.ingredient match {
        case Ingredient.Mushroom =>
          findCorrectSlice(pizza, nbTomato, nbMushroom-1, Slice(firstCell.x, firstCell.y, firstCell.x, firstCell.y))
        case Ingredient.Tomato =>
          findCorrectSlice(pizza, nbTomato-1, nbMushroom, Slice(firstCell.x, firstCell.y, firstCell.x, firstCell.y))
      }
    }
    else {
      val biggerSlice = expandSlice(pizza, slice)
      if(biggerSlice == slice)
        None
      else {
        val sliceCells = getSliceCells(pizza, slice)
        val biggerSliceCells = getSliceCells(pizza, biggerSlice)
        val newMushroom = biggerSliceCells.filter(_.ingredient == Ingredient.Mushroom).size - sliceCells.filter(_.ingredient == Ingredient.Mushroom).size
        val newTomato = biggerSliceCells.filter(_.ingredient == Ingredient.Tomato).size - sliceCells.filter(_.ingredient == Ingredient.Tomato).size
        findCorrectSlice(pizza, nbTomato - newTomato, nbMushroom-newMushroom, biggerSlice)
      }
    }
  }

}
