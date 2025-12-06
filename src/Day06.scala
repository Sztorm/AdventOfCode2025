private enum Operation:
  case Sum, Product

private case class Homework(numbers: List[Long], operation: Operation):
  def solve = operation match
    case Operation.Sum => numbers.sum
    case Operation.Product => numbers.product

object Day06:
  private def parseOperations(input: String) = input
    .split(' ')
    .filterNot(_.isBlank)
    .map {
      case "+" => Operation.Sum
      case _ => Operation.Product
    }
    .toList

  private def parseHomeworks(input: List[String]) = input
    .splitAt(input.length - 1)
    .|>((numberRows, operationRow) =>
      val numberColumns = numberRows
        .map(row => row
          .split(' ')
          .filterNot(_.isBlank)
          .map(_.toLong)
          .toList
        )
        .transpose
      val operations = parseOperations(operationRow.head)

      numberColumns
        .zip(operations)
        .map((column, operation) => Homework(column, operation))
    )

  private def parseCephalopodHomeworks(input: List[String]) = input
    .splitAt(input.length - 1)
    .|>((numberRows, operationRow) =>
      val maxLength = numberRows.maxBy(_.length).length
      val paddedNumbers = numberRows.map(_.padTo(maxLength, ' '))
      val numberColumns = paddedNumbers
        .transpose
        .map(_.filterNot(_.isWhitespace))
        .split(_.isEmpty)
        .map(column => column
          .map(_.mkString.toLong)
        )
      val operations = parseOperations(operationRow.head)

      numberColumns
        .zip(operations)
        .map((column, operation) => Homework(column, operation))
    )

  def part1(input: List[String]) = input
    .|>(parseHomeworks)
    .map(_.solve)
    .sum

  def part2(input: List[String]) = input
    .|>(parseCephalopodHomeworks)
    .map(_.solve)
    .sum