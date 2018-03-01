package hashcode

import java.io.PrintWriter

import scala.io.Source

object main extends App {

  //c_no_hurry
  //d_metropolis
  //b_should_be_easy
  val filePath = "src/main/scala/source/d_metropolis.in"
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

  val sortedRides = rides.sortBy(ride => ride.t1 -> - ride.t2)
  val result1 = StrategyFlotte.run2( sortedRides, vehicles, 0)
  println(result1)
  val result = result1.groupBy(_._1)
    .map(x => (x._1, x._2.map(_._2)))
    .toList
  println(result)
  new PrintWriter("resultb.txt") {
    result.map(x =>  write(x._2.size.toString +" " + x._2.mkString (" ")  + "\n"))
    close }
}
