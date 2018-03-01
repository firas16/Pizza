package hashcode

import java.io.PrintWriter

import scala.io.Source

object main extends App {

  val filePath = "src/main/scala/source/big.in"
  for (line <- Source.fromFile(filePath).getLines){
  }
  val lines = Source.fromFile(filePath).getLines.toList
  val params = lines(0).split(' ').map(_.toInt)
  val nbVehicles = params(2)
  val nbSteps = params(5)
  val bonus = params(4)
  val grid = Grid(params(0), params(1))

  val rides = lines.drop(1).map(line =>  line.split(' ').map(_.toInt))
    .map(x => Ride(Coord(x(0), x(1)), Coord(x(2), x(3)), x(4), x(5)))
}
