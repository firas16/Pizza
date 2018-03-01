package hashcode


object StrategyFlotte {

  def run(grid: Grid, rides: List[Ride], vehicles: List[Vehicle]): List[(Int, List[Int])] = {

    val sortedRides = rides.sortBy(ride => ride.t1 -> - ride.t2)
    val result = sortedRides.map(ride =>
      (ride.index % vehicles.size + 1, ride.index))
      .groupBy(_._1)
      .map(x => (x._1, x._2.map(_._2)))
      .toList
    return result

  }

  def findVehicle(ride: Ride, vehicles: List[Vehicle]): (Option[Int], Option[Vehicle]) = {

    val validVeh = vehicles.zipWithIndex.find(v => getRideFinishTime(v._1, ride) <= ride.t2)
   validVeh match {
      case None => (None,None)
      case Some(rs) => (Some(rs._2), Some(rs._1))
    }

  }

  def updateFlote(id: Int, veh: Vehicle, flotte: List[Vehicle]): List[Vehicle] = {
    if(id>0) flotte.take(id-1) ++ List(veh) ++ flotte.drop(id) else veh :: flotte.drop(1)

  }

  def run2(rides: List[Ride], vehicles: List[Vehicle], score: Int): List[(Int, Int)] = {

    rides match {
      case ride :: tail =>
        val (id, veh) = findVehicle(ride, vehicles)
        veh match {
          case Some(v) =>
            val updatedVehicle = updateVehicleStatus(v, ride)
            val updatedFlotte = updateFlote(id.get, updatedVehicle, vehicles)
            val newScore = get_distance(ride.startPosition, ride.endPosition) + score
            val bonus = if(v.timeDispo + get_distance(v.coord, ride.startPosition) <= ride.t1) 25 else 0
            println(newScore + bonus)
            (id.get, ride.index) :: run2(tail, updatedFlotte, newScore + bonus)
          case None => run2(tail, vehicles, score)
        }

      case Nil => Nil
    }

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
