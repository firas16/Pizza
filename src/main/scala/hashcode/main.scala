package hashcode

import java.io.PrintWriter

import scala.io.Source

object main extends App {

  val filePath = "src/main/scala/source/a_example.in"
  for (line <- Source.fromFile(filePath).getLines){
  }
  val lines = Source.fromFile(filePath).getLines.toList
  val params = lines(0).split(' ').map(_.toInt)
  val nbVehicles = params(2)
  val nbSteps = params(5)
  val bonus = params(4)
  val grid = Grid(params(0), params(1))
  val vehicles = Range(1, nbVehicles+1).map(x => Vehicle(Coord(0,0), false)).toList
  val rides = lines.drop(1).map(line =>  line.split(' ').map(_.toInt)).zipWithIndex
    .map(x => Ride(x._2, Coord(x._1(0), x._1(1)), Coord(x._1(2), x._1(3)), x._1(4), x._1(5)))

  val result = StrategyFlotte.run(grid, rides, vehicles)

  new PrintWriter("result.txt") {
    result.map(x =>  write(x._2.size.toString +" " + x._2.mkString (" ")  + "\n"))
    close }
}
