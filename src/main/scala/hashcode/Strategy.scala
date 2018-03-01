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

  def findVehicle(ride: Ride, vehicles: List[Vehicle]): (Option[Int], Option[Vehicle] = {

    val validVeh = vehicles.zipWithIndex.filter(v => getRideFinishTime(v._1, ride)<= ride.t2)
    validVeh match {
      case Nil => (None,None)
      case ls =>
        val rs = ls.sortBy(x => getRideFinishTime(x._1, ride)).head
        (Some(rs._2), Some(rs._1))
    }

  }

  def updateFlotte(id: Int, veh: Vehicle, flotte: List[Vehicle]): List[Vehicle] = {
    val flot = if(id>0) flotte.take(id-1) ++ flotte.drop(id) else flotte.drop(1)
    flot ++ List(veh)
  }

  def run2(rides: List[Ride], vehicles: List[Vehicle]): List[(Int, Int)] = {

    rides match {
      case ride :: tail =>
        val (id, veh) = findVehicle(ride, vehicles)
        veh match {
          case Some(v) =>
            val updatedVehicle = updateVehicleStatus(v, ride)
            val updatedFlotte = updatedFlotte(id, updatedVehicle, vehicles)
            (id.get, ride.index) :: run2(tail, vehicles)
          case None => run2(tail, vehicles)
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
