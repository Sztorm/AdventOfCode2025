import java.nio.file.Paths
import scala.io.Source

@main def main() =
  val inputPath = Paths.get("inputs/input.txt").toAbsolutePath.toString
  val inputSource = Source.fromFile(inputPath)
  val input =
    try inputSource.getLines().toList
    finally inputSource.close()

  println(Day01.part1(input))
  println(Day01.part2(input))