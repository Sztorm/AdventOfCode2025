object Day02:
  private def parseRanges(input: List[String]) = input
    .map(line => line
      .split(',')
      .map(range => range
        .split('-') match
        case Array(start, end) => start.toLong to end.toLong
      )
    )
    .last

  def part1(input: List[String]) = input
    .|>(parseRanges)
    .map(range => range
      .filter(id =>
        val idString = id.toString
        val isInvalid = idString.length % 2 == 0 &&
          idString.splitAt(idString.length / 2).|>((a, b) => a == b)
        isInvalid
      )
      .sum
    )
    .sum

  def part2(input: List[String]) = input
    .|>(parseRanges)
    .map(range => range
      .filter(id =>
        val idString = id.toString
        val isInvalid = idString.length > 1 &&
          idString.forall(_ == idString(0)) ||
          (2 until idString.length)
            .exists(length =>
              val chunk = idString.take(length)
              idString
                .grouped(length)
                .forall(s => s == chunk)
            )
        isInvalid
      )
      .sum
    )
    .sum