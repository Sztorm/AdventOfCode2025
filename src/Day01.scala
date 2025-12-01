import scala.annotation.tailrec
import scala.math.abs

object Day01:
  private def parseRotations(input: List[String]) = input
    .map(line =>
      val direction = line(0) match
        case 'L' => -1
        case _ => 1
      val number = line.drop(1).toInt

      direction * number
    )

  def part1(input: List[String]) =
    @tailrec
    def loop(rotations: List[Int], dialNum: Int, result: Int): Int =
      rotations match
        case Nil => result
        case rotation :: tail =>
          val dn = (dialNum + rotation % 100 + 100) % 100
          loop(tail, dn, if dn == 0 then result + 1 else result)

    input
      .|>(parseRotations)
      .|>(rotations => loop(rotations, dialNum = 50, result = 0))

  def part2(input: List[String]) =
    @tailrec
    def loop(rotations: List[Int], dialNumber: Int, result: Int): Int =
      rotations match
        case Nil => result
        case rotation :: tail =>
          val dialNum = (dialNumber + rotation % 100 + 100) % 100
          val rotationLoops = abs(rotation / 100)
          val singleRotation = rotation % 100
          val relativeRotation = dialNumber + singleRotation
          val res =
            if dialNumber == 0 || (1 to 99).contains(relativeRotation) then rotationLoops + result
            else 1 + rotationLoops + result
          loop(tail, dialNum, res)

    input
      .|>(parseRotations)
      .|>(rotations => loop(rotations, dialNumber = 50, result = 0))
