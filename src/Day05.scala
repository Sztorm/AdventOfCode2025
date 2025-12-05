import scala.annotation.tailrec
import scala.math.max

object Day05:
  private def parseRangesAndIds(input: List[String]) = input
    .splitAt(input.indexWhere(_.isBlank))
    .|>((ranges, ids) =>
      val longRanges = ranges
        .map(s => s
          .split('-')
          .map(_.toLong) match
          case Array(start, end) => LongRange(start, end)
        )
      val longIds = ids
        .drop(1)
        .map(_.toLong)
      (longRanges, longIds)
    )

  def part1(input: List[String]) = input
    .|>(parseRangesAndIds)
    .|>((ranges, ids) =>
      ids.count(id =>
        ranges.exists(_.contains(id))
      )
    )

  @tailrec
  private def mergeRanges(ranges: List[LongRange]): List[LongRange] =
    @tailrec
    def loop(ranges: List[LongRange], result: List[LongRange]): List[LongRange] =
      ranges match
        case Nil => result
        case head :: tail =>
          result match
            case Nil => loop(tail, head :: result)
            case prev :: resultTail =>
              if head.start <= prev.start && head.end >= prev.start then
                loop(tail, LongRange(head.start, max(head.end, prev.end)) :: resultTail)
              else
                loop(tail, head :: result)

    val sortedRanges = ranges
      .sortBy(range => (range.start, range.end))(using Ordering[(Long, Long)].reverse)
    val mergedRanges = loop(sortedRanges, List.empty)

    if mergedRanges == ranges then ranges
    else mergeRanges(mergedRanges)

  def part2(input: List[String]) = input
    .|>(parseRangesAndIds)
    .|>((ranges, _) => mergeRanges(ranges))
    .map(_.length)
    .sum