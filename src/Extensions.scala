extension [A](value: A)
  def |>[B](f: A => B): B = f(value)

extension (value: Boolean)
  def toInt = if value then 1 else 0