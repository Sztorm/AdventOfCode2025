private case class LongRange(start: Long, end: Long):
  def length = end - start + 1

  def contains(value: Long) = value >= start && value <= end