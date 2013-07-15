package edu.knowitall.tac2013.pattern

import scala.collection.mutable

class Counter[T] private (private var backingMap: mutable.Map[T, MutInt] = new mutable.HashMap[T, MutInt]) {
  
  def add(t: T): Counter[T] = add(t, 1)
  
  private def add(s: T, n: Int): Counter[T] = {
    backingMap.getOrElseUpdate(s, new MutInt).add(n)
    this
  }
  
  /**
   * Keep the top strings only
   */
  def trim(max: Int): Counter[T] = {
    val newMap = backingMap.iterator.toSeq.sortBy(-_._2.value).take(max).toMap
    backingMap.clear()
    backingMap ++= newMap
    this
  }
  
  def map = backingMap.toMap
  
  def top(num: Int) = {
    backingMap.iterator.toSeq.sortBy(-_._2.value).take(num).map { case (str, count) => (str, count.value) }
  }
  
  def addAll(other: Counter[T]): Counter[T] = {
    def allElements = (backingMap.iterator ++ other.backingMap.iterator).toSeq
    val newMap = allElements.groupBy(_._1).map { case (str, strCounts) => 
      val newCount = new MutInt(strCounts.map(_._2.value).sum)
      (str, newCount)
    }
    backingMap.clear()
    backingMap ++= newMap
    this
  }
}

class MutInt(var value: Int = 0) {
  def inc: Unit = add(1)
  def add(n: Int) = value += n
}

object Counter {
  
  def from[T](ts: Seq[T]) = {
    var counter = new Counter[T]() 
    ts.foreach(t => counter = counter.add(t))
    counter
  }
}