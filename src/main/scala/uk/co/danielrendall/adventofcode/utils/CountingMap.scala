package uk.co.danielrendall.adventofcode.utils

case class CountingMap[T, N](map: Map[T, N])
                            (using num: Numeric[N]) {

  def sortedSizeFirst: Seq[(T, N)] = map.toSeq.sortBy(_._2)(Ordering[N].reverse)

  def add(t: T): CountingMap[T, N] = map.get(t) match {
    case Some(count) => CountingMap(map.+(t -> num.plus(count, num.one)))
    case None => CountingMap(map.+(t -> num.one))
  }

  def add(t: T, howMany: N): CountingMap[T, N] = map.get(t) match {
    case Some(count) => CountingMap(map.+(t -> num.plus(count, howMany)))
    case None => CountingMap(map.+(t -> howMany))
  }

  def add(iterable: Iterable[T]): CountingMap[T, N] =
    iterable.foldLeft(this) { case (cMap, item) => cMap.add(item)}

  def addAll(other: CountingMap[T, N]): CountingMap[T, N] =
    other.map.foldLeft(this) { case (curMap, newEntry) => curMap.add(newEntry._1, newEntry._2) }
}

object CountingMap {
  def apply[T, N]()
                 (using num: Numeric[N]): CountingMap[T, N] = new CountingMap(Map.empty)
}
