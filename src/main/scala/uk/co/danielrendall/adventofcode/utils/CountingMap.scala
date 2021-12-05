package uk.co.danielrendall.adventofcode.utils

case class CountingMap[T, N](map: Map[T, N])
                            (using num: Numeric[N]) {

  def add(t: T): CountingMap[T, N] = map.get(t) match {
    case Some(count) => CountingMap(map.+(t -> num.plus(count, num.one)))
    case None => CountingMap(map.+(t -> num.one))
  }
}

object CountingMap {
  def apply[T, N]()
                 (using num: Numeric[N]): CountingMap[T, N] = new CountingMap(Map.empty)
}
