package pizza

import scala.io.Source

object main extends App {

  val filePath = "src/main/scala/source/small.in"
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
  val pizza = Pizza(cells.flatten.toArray)
  print(pizza.cells.toList)
}
