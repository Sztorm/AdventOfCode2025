import scala.annotation.tailrec
import scala.math.pow

object Day03:
  private def parseBatteryBanks(input: List[String]) = input
    .map(line =>
      line
        .map(_.asDigit)
        .toList
    )

  private def max(count: Int)(bank: List[Int]): Long =
    @tailrec
    def loop(slice: List[Int], remaining: Int, result: Long): Long =
      if remaining == 0 then result
      else
        val singleMax = slice.dropRight(remaining - 1).max
        loop(
          slice.drop(slice.indexOf(singleMax) + 1),
          remaining - 1,
          result + singleMax * pow(10, remaining - 1).toLong
        )

    loop(bank, count, 0)

  def part1(input: List[String]) = input
    .|>(parseBatteryBanks)
    .map(max(2))
    .sum

  def part2(input: List[String]) = input
    .|>(parseBatteryBanks)
    .map(max(12))
    .sum