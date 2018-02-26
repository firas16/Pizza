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

    val slice = Strategy.run(pizza, 1, 6)
    print(slice)
    //When
    //Then
  }

}
