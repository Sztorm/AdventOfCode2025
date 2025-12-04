import scala.annotation.tailrec

object Day04:
  private def parseGrid(input: List[String]) = input
    .zipWithIndex
    .flatMap((str, row) => str
      .zipWithIndex
      .map((char, col) => (row, col) -> char)
      .filter((index, char) => char == '@')
    )
    .toMap

  private def canBeRemoved(index: (Int, Int), grid: Map[(Int, Int), Char]) =
    val (i, j) = index
    if grid(i, j) == '@' then
      val adjacentPaperRolls =
        (grid.getOrElse((i, j - 1), '.') == '@').toInt +
          (grid.getOrElse((i, j + 1), '.') == '@').toInt +
          (grid.getOrElse((i + 1, j - 1), '.') == '@').toInt +
          (grid.getOrElse((i + 1, j), '.') == '@').toInt +
          (grid.getOrElse((i + 1, j + 1), '.') == '@').toInt +
          (grid.getOrElse((i - 1, j - 1), '.') == '@').toInt +
          (grid.getOrElse((i - 1, j), '.') == '@').toInt +
          (grid.getOrElse((i - 1, j + 1), '.') == '@').toInt
      adjacentPaperRolls < 4
    else false

  private def countRemovablePaper(grid: Map[(Int, Int), Char]): Int =
    @tailrec
    def loop(grid: Map[(Int, Int), Char], result: Int): Int =
      val removed = grid.filterNot((index, char) => canBeRemoved(index, grid))

      if (removed == grid) result
      else loop(removed, result + grid.size - removed.size)

    loop(grid, 0)

  def part1(input: List[String]) = input
    .|>(parseGrid)
    .|>(grid => grid
      .count((index, char) => canBeRemoved(index, grid))
    )

  def part2(input: List[String]) = input
    .|>(parseGrid)
    .|>(countRemovablePaper)