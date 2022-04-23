package com.github.trackiss.rampart

object OrderingUtil:
  enum CompareResult:
    case LT, EQ, GT

  extension [T](ord: Ordering[T])
    def cmp(x: T, y: T): CompareResult =
      import CompareResult.*

      val result = ord.compare(x, y)
      if result < 0 then LT
      else if result > 0 then GT
      else EQ
