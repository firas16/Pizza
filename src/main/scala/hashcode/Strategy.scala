package hashcode


object StrategyFlotte {

  def run(grid: Grid, rides: List[Ride], vehicles: List[Vehicle]): List[(Int, List[Int])] = {

    val sortedRides = rides.sortBy(ride => ride.t1 -> - ride.t2)
    val result = sortedRides.map(ride => (ride.index % vehicles.size + 1, ride.index))
      .groupBy(_._1)
      .map(x => (x._1, x._2.map(_._2)))
      .toList
    return result

  }

  // update ccords and time of vehicle if it takes ride
  def updateVehicleStatus(vehicle: Vehicle, ride: Ride): Vehicle = {
    val newT = getRideFinishTime(vehicle, ride)
    val newPosition = ride.endPosition
    Vehicle(newPosition, false, newT)

  }

  def get_distance(coord1: Coord, coord2: Coord): Int = Math.abs(coord1.x-coord2.x) + Math.abs(coord1.y-coord2.y)

  //returns time when vehicle finishes if it takes ride
  def getRideFinishTime(vehicle: Vehicle, ride: Ride): Int = {
    val timeTogetCustomer = vehicle.timeDispo + get_distance(vehicle.coord, ride.startPosition)
    val timeToTravel = get_distance(ride.startPosition, ride.endPosition)
    return Math.max(timeTogetCustomer, ride.t1) + timeToTravel
  }

}
