package hashcode

/**
  * Created by Firas on 01/03/2018.
  */
case class Grid(R: Int, C: Int) {
}
case class Coord(x: Int, y: Int)
case class Ride(startPosition: Coord, endPosition: Coord, t1: Int, t2: Int)
