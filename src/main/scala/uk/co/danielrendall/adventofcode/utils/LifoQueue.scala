package uk.co.danielrendall.adventofcode.utils

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import scala.reflect.ClassTag

/**
 * A mutable LIFO queue implementation
 */
class LifoQueue[T](implicit ct: ClassTag[T]) {

  private val arrayRef: AtomicReference[Array[T]] = new AtomicReference[Array[T]](Array.ofDim(2))

  // The point where the next item should be inserted
  private val insertPosRef: AtomicInteger = new AtomicInteger(0)

  // The point where the next item should be removed; if this is equal to insertPtr then there's nothing in the queue
  private val removePosRef: AtomicInteger = new AtomicInteger(0)

  def pushAll(ts: Iterable[T]): Unit = this.synchronized {
    ts.foreach(push)
  }

  def push(t: T): Unit = this.synchronized {
    val insertPos = insertPosRef.get()
    val removePos = removePosRef.get()
    val array = arrayRef.get()
    array(insertPos) = t
    val nextInsertPos = (insertPos + 1) % array.length
    if (nextInsertPos == removePos) {
      val newArray = Array.ofDim[T](array.length * 2)
      // We have .... elements 1 ... NextInsertPos = removePos ... elements 2 ...
      // where either of the elements things could have zero length
      // Copy elements 1 to the start of the new array; nextInsertPos doesn't need to change
      System.arraycopy(array, 0, newArray, 0, nextInsertPos)
      // Now copy elements 2 to the end of the new array and move the removePos
      val numberOfElementsLeftToRemove = array.length - removePosRef.get()
      val newRemovePos = newArray.length - numberOfElementsLeftToRemove
      System.arraycopy(array, removePos, newArray, newRemovePos, numberOfElementsLeftToRemove)
      arrayRef.set(newArray)
      removePosRef.set(newRemovePos)
    }
    insertPosRef.set(nextInsertPos)
  }

  def pop(): T = this.synchronized {
    val removePos = removePosRef.get()
    if (isEmpty) {
      throw new NoSuchElementException("Out of elements")
    }
    val array = arrayRef.get()
    val element = array(removePos)
    array(removePos) = null.asInstanceOf[T]
    val nextRemovePos = (removePos + 1) % array.length
    removePosRef.set(nextRemovePos)
    element
  }

  def peek(): T = this.synchronized {
    val removePos = removePosRef.get()
    if (isEmpty) {
      throw new NoSuchElementException("Out of elements")
    }
    val array = arrayRef.get()
    array(removePos)
  }
  
  def isEmpty: Boolean = insertPosRef.get() == removePosRef.get()

}
