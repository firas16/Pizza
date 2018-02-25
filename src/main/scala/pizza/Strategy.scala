package pizza

object Strategy {

  val l = 250
  val h = 250
  def run(pizza: Pizza, l: Int, h: Int): List[Slice] = {

    val nbMushroom = pizza.cells.map(_.ingredient).count(ingredient => ingredient == Ingredient.Mushroom)
    val nbTomate = pizza.cells.map(_.ingredient).count(ingredient => ingredient == Ingredient.Tomato)
    val maxSlices = Math.min(nbTomate, nbMushroom)/l
    var result: List[Slice] = List()
    var update: Boolean = true
    while(pizza.cells.exists(!_.sliced) && update){
      val cell = findNotSlicedCell(pizza)
      val slice = findCorrectSlice(pizza, l, l, slice(cell))
      result = slice:: result
      val pizzaUpdated = updatePizza(pizza, slice)
      update = pizzaUpdated == pizza
    }
    return result
  }
  def isSliceCorrect(slice: Slice, pizza: Pizza): Boolean = {

    val cellsMorceau = pizza.cells.filter(cell =>
        cell.x >= slice.row1 &&
        cell.x <= slice.row2 &&
        cell.y >= slice.col1 &&
        cell.y <= slice.col2)
    val pizzaMorceau = Pizza(cellsMorceau)
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

  def updatePizza(pizza: Pizza, slice: Slice) = ???

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
      val biggerSlice = expandSlice(pizza, slice)
      if(biggerSlice == slice)
        slice
      else
        findCorrectSlice(pizza, nbTomato, nbMushroom, biggerSlice)
    }
  }

}
