package pizza


class StrategySuite extends UnitSpec {

  "findCorrectSlice" should "get a correct slice of pizza" in {

    //Given
    val Tomato = Ingredient.Tomato
    val Mushroom = Ingredient.Mushroom
    val pizza = Pizza(List(
     Cell(1,1,Tomato,false), Cell(1,2,Tomato,false), Cell(1,3,Tomato,false), Cell(1,4,Tomato,false), Cell(1,5,Tomato,false),
     Cell(2,1,Tomato,false), Cell(2,2,Mushroom,false), Cell(2,3,Mushroom,false), Cell(2,4,Mushroom,false), Cell(2,5,Tomato,false),
     Cell(3,1,Tomato,false), Cell(3,2,Tomato,false), Cell(3,3,Tomato,false), Cell(3,4,Tomato,false), Cell(3,5,Tomato,false)).toArray)

    val slices = Strategy.run(pizza, 1, 6)
    print(slices)
    val usedCells = slices.flatMap(slice => Strategy.getSliceCells(pizza, slice))
    val updatedPizzaCells = pizza.cells.map(cell =>
      if(usedCells.exists(ce => ce.x == cell.x && ce.y == cell.y))
        Cell(cell.x, cell.y, cell.ingredient, true)
      else
      cell)
    val res = Strategy.expandPizzaSlices(Pizza(updatedPizzaCells), slices)
    print(res)
    //When
    //Then
  }

}
