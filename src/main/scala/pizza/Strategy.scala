package pizza

import pizza.Ingredient.Ingredient

object Strategy {

  val l = 6
  val h = 14

  def run(pizza: Pizza, l: Int, h: Int): List[Slice] = {

    val nbMushroom = pizza.cells.map(_.ingredient).count(ingredient => ingredient == Ingredient.Mushroom)
    val nbTomate = pizza.cells.map(_.ingredient).count(ingredient => ingredient == Ingredient.Tomato)
    val maxSlices = Math.min(nbTomate, nbMushroom) / l

    val cell = findNotSlicedCell(pizza)
    val slice = findCorrectSlice(pizza, l, l, Slice(cell))
    slice match {
      case Some(sl) =>
        val pizzaUpdated = updatePizza(pizza, Some(sl).get)
        if(isSliceCompleted(sl, pizza))
          return slice.get :: run(pizzaUpdated, l, h)
        else run(pizzaUpdated, l, h)
      case None => List.empty[Slice]
    }
  }


  def expandPizzaSlice(pizza: Pizza, slice: Slice): Slice = {
    val newSlice = enhanceSlice(pizza, slice)
    if(newSlice == slice)
      return slice
    else
      expandPizzaSlice(pizza, newSlice)
  }

  def expandPizzaSlices(pizza: Pizza, slices: List[Slice]): List[Slice] = {

    slices match {
      case slice:: tail =>
        val newSlice = expandPizzaSlice(pizza, slice)
        val updatedPizza = updatePizza(pizza, newSlice)
        return newSlice :: expandPizzaSlices(updatedPizza, tail)
      case Nil => Nil
    }
  }

  def isSliceCompleted(slice: Slice, pizza: Pizza): Boolean = {
    val nbTomato = countIngredient(pizza, slice, Ingredient.Tomato)
    val nbMushroom = countIngredient(pizza, slice, Ingredient.Mushroom)
    isSliceAdmissible(slice, pizza) && nbTomato >= l && nbMushroom >= l
  }

  def isSliceAdmissible(slice: Slice, pizza: Pizza): Boolean = {

    if(0 > slice.row1 || 0 > slice.col1 || slice.row2 > pizza.nbRows-1 || slice.col2 > pizza.nbCols-1)
      return false
    val cellsMorceau = getSliceCells(pizza, slice)
    if(cellsMorceau.exists(_.sliced))
      return false
    val pizzaMorceau = Pizza(cellsMorceau.toArray, pizza.nbRows, pizza.nbCols)
    val nbTomato = pizzaMorceau.nbTomato
    val nbMushroom = pizzaMorceau.nbMushroom
    return (nbTomato+nbMushroom <= h)

  }

  def isNewSliceAdmissible(newSlice: Slice, slice: Slice, pizza: Pizza): Boolean = {

    if(0 > newSlice.row1 || 0 > newSlice.col1 || newSlice.row2 > pizza.nbRows-1 || newSlice.col2 > pizza.nbCols-1)
      return false
    val cellsMorceau = getSliceCells(pizza, slice)
    val newCellsMorceau = getSliceCells(pizza, newSlice)
    if((newCellsMorceau diff cellsMorceau).exists(_.sliced))
      return false
    val pizzaMorceau = Pizza(newCellsMorceau.toArray, pizza.nbRows, pizza.nbCols)
    val nbTomato = pizzaMorceau.nbTomato
    val nbMushroom = pizzaMorceau.nbMushroom
    return (nbTomato+nbMushroom <= h)

  }

  def findNotSlicedCell(pizza: Pizza): Cell = pizza.cells.find(!_.sliced).get

  def countIngredient(pizza: Pizza, slice: Slice, ingredient: Ingredient): Int = {
    val cells = getSliceCells(pizza, slice)
    ingredient match{
      case Ingredient.Mushroom => cells.filter(_.ingredient == Ingredient.Mushroom).size
      case Ingredient.Tomato => cells.filter(_.ingredient == Ingredient.Tomato).size
    }

  }

  def score(pizza: Pizza, slice: Slice): Int = {
    val nbMushroom = countIngredient(pizza, slice, Ingredient.Mushroom)
    val nbTomato = countIngredient(pizza, slice, Ingredient.Tomato)
    val scoreSlice = Math.max(l - nbMushroom,  0) + Math.max(l - nbTomato,  0)
    return scoreSlice
  }

  def expandSlice(pizza: Pizza, slice: Slice): Slice = {
    val lenghtRow = slice.row2 - slice.row1
    val lengthCol = slice.col2 - slice.col1
    val maxToAdd = h - lenghtRow * lengthCol
    if(maxToAdd <= 0)
      return slice

    val sliceGauche = Slice(slice.row1, slice.col1-1, slice.row2, slice.col2)
    val sliceDroite = Slice(slice.row1, slice.col1, slice.row2, slice.col2+1)
    val sliceUp = Slice(slice.row1-1, slice.col1, slice.row2, slice.col2)
    val sliceDown = Slice(slice.row1, slice.col1, slice.row2+1, slice.col2)
    val slices = List(sliceGauche, sliceDroite, sliceDown, sliceUp)
    val res = slices.find(slic => isSliceCompleted(slic, pizza))
    res match{
      case Some(slic) => slic
      case None =>
        val admissibleSlices = slices.filter(isSliceAdmissible(_, pizza))
        if(admissibleSlices.size>0)
          return admissibleSlices.sortBy(slice => score(pizza, slice)).reverse.head
        else
          return slice
      }
    }

  def enhanceSlice(pizza: Pizza, slice: Slice): Slice = {
    val lenghtRow = slice.row2 - slice.row1
    val lengthCol = slice.col2 - slice.col1
    val maxToAdd = h - lenghtRow * lengthCol
    if(maxToAdd <= 0)
      return slice

    val sliceGauche = Slice(slice.row1, slice.col1-1, slice.row2, slice.col2)
    val sliceDroite = Slice(slice.row1, slice.col1, slice.row2, slice.col2+1)
    val sliceUp = Slice(slice.row1-1, slice.col1, slice.row2, slice.col2)
    val sliceDown = Slice(slice.row1, slice.col1, slice.row2+1, slice.col2)
    val slices = List(sliceGauche, sliceDroite, sliceDown, sliceUp)
    val admissibleSlices = slices.filter(isNewSliceAdmissible(_, slice, pizza))
    if(admissibleSlices.size>0)
       return admissibleSlices.sortBy(slice => score(pizza, slice)).reverse.head
    else
       return slice

  }

  def updatePizza(pizza: Pizza, slice: Slice): Pizza = {
    val cells = getSliceCells(pizza, slice)
    val updatedCells = pizza.cells.map(cell =>
      Cell(cell.x, cell.y, cell.ingredient, cells.contains(cell) || cell.sliced))
    Pizza(updatedCells, pizza.nbRows, pizza.nbCols)
  }

  def getSliceCells(pizza: Pizza, slice: Slice): List[Cell] = {
    pizza.cells.filter(cell =>
      cell.x >= slice.row1 &&
        cell.x <= slice.row2 &&
        cell.y >= slice.col1 &&
        cell.y <= slice.col2).toList
  }

  def findCorrectSlice(pizza: Pizza, nbTomato: Int, nbMushroom: Int, slice: Slice = null): Option[Slice] = {
    if(nbTomato <= 0 && nbMushroom <= 0)
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
        val newMushroom = countIngredient(pizza, biggerSlice, Ingredient.Mushroom) - countIngredient(pizza, slice, Ingredient.Mushroom)
        val newTomato = countIngredient(pizza, biggerSlice, Ingredient.Tomato) - countIngredient(pizza, slice, Ingredient.Tomato)
        findCorrectSlice(pizza, nbTomato - newTomato, nbMushroom-newMushroom, biggerSlice)
      }
    }
  }

}
