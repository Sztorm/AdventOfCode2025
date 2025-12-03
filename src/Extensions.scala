extension [A](value: A)
  def |>[B](f: A => B): B = f(value)