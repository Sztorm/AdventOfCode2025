extension [A](value: A)
  def |>[B](f: A => B): B = f(value)

extension (value: Boolean)
  def toInt = if value then 1 else 0

extension [A](list: List[A])
  def split(p: A => Boolean): List[List[A]] = list
    .foldRight(List(List.empty)) { (item, acc) =>
      if p(item) then List.empty :: acc
      else acc match
        case Nil => List(item) :: acc
        case head :: tail => (item :: head) :: tail
    }