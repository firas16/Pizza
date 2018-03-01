package hashcode


object StrategyFlotte {

  def run(grid: Grid, rides: List[Ride], vehicles: List[Vehicle]): List[(Int, List[Int])] = {

    val sortedRides = rides.sortBy(ride => ride.t1 -> - ride.t2)
    val result2 = sortedRides.map(ride => (ride.index % vehicles.size, ride.index))
      .groupBy(_._1)
    val result = result2
      .map(x => (x._1, x._2.map(_._2))).toList
    return result

  }

  def getRideFinishTime(vehicle: Vehicle, ride: Ride): Int = {

    ???
  }

}
