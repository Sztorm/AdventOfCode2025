import java.nio.file.Paths
import scala.io.Source

@main def main() =
  val testInputPath = Paths.get("inputs/test-input.txt").toAbsolutePath.toString
  val inputPath = Paths.get("inputs/input.txt").toAbsolutePath.toString
  val testInput = Source.fromFile(testInputPath)
    .|>(source =>
      try source.getLines().toList
      finally source.close()
    )
  val input = Source.fromFile(inputPath)
    .|>(source =>
      try source.getLines().toList
      finally source.close()
    )

  println(s"test part 1: ${Day04.part1(testInput)}")
  println(s"part 1:      ${Day04.part1(input)}")
  println(s"test part 2: ${Day04.part2(testInput)}")
  println(s"part 2:      ${Day04.part2(input)}")