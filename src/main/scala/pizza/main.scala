package pizza

import java.io.PrintWriter

import scala.io.Source

object main extends App {

  val filePath = "src/main/scala/source/big.in"
  for (line <- Source.fromFile(filePath).getLines){
  }
  val lines = Source.fromFile(filePath).getLines.toList
  val params = lines(0).split(' ')
  val cells = lines.zipWithIndex.drop(1).map(line => line._1.toCharArray()
    .toList
    .zipWithIndex
    .map(x =>
      if(x._1 == 'M')
        Cell(line._2 - 1, x._2, Ingredient.Mushroom)
      else
        Cell(line._2 - 1, x._2, Ingredient.Tomato)
    ))
  val pizza = Pizza(cells.flatten.toArray, params(0).toInt, params(1).toInt)


  val slices = Strategy.run(pizza, params(2).toInt, params(3).toInt, 1000)
  print(slices)
  val usedCells = slices.flatMap(slice => Strategy.getSliceCells(pizza, slice))
  val updatedPizzaCells = pizza.cells.map(cell =>
    if(usedCells.exists(ce => ce.x == cell.x && ce.y == cell.y))
      Cell(cell.x, cell.y, cell.ingredient, true)
    else
      cell)
  val result = Strategy.expandPizzaSlices(Pizza(updatedPizzaCells, pizza.nbRows, pizza.nbCols), slices)

  new PrintWriter("result.txt") {
    write(result.size.toString + "\n")
    result.map(x =>  write(x.toString + " \n"));
    close }
}
