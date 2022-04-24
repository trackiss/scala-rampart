package net.trackiss.rampart

import scala.math.Ordering.Implicits.*

/** This class provides types and functions for defining intervals and
  * determining how they relate to each other. This can be useful to determine
  * if an event happened during a certain time frame, or if two time frames
  * overlap (and if so, how exactly they overlap).
  *
  * This class was inspired by James F. Allen's report, ''Maintaining Knowledge
  * About Temporal Intervals''. It also uses terminology from that report. You
  * should not need to read the report in order to understand this module, but
  * if you want to read it you can find it here:
  * [[https://hdl.handle.net/1802/10574]].
  */
final case class Interval[T: Ordering] private (private val x: T, private val y: T):
  import Relation.*

  require(x <= y)

  /** Gets the lesser value from an [[Interval]].
    *
    * {{{
    * assert(Interval(1, 2).lesser == 1)
    * assert(Interval(2, 1).lesser == 1)
    * }}}
    * {{{
    * assert(Interval(x, y) == x.min(y))
    * }}}
    */
  def lesser: T = x

  /** Gets the greater value from an [[Interval]].
    *
    * {{{
    * assert(Interval(1, 2).lesser == 2)
    * assert(Interval(2, 1).lesser == 2)
    * }}}
    * {{{
    * assert(Interval(x, y) == x.max(y))
    * }}}
    */
  def greater: T = y

  /** Converts an [[Interval]] into a tuple. Generally you can think of this as
    * the inverse of [[Interval.apply]]. However the tuple returned by this
    * function may be swapped compared to two value passed to
    * [[Interval.apply]].
    *
    * {{{
    * assert(Interval(1, 2).extract == (1, 2))
    * assert(Interval(2, 1).extract == (1, 2))
    * }}}
    * {{{
    * assert(Interval(x, y).extract == (x.min(y), x.max(y)))
    * }}}
    */
  def extracts: (T, T) = (lesser, greater)

  /** Returns `true` if the given [[Interval]] is empty, `false` otherwise. An
    * [[Interval]] is empty if its [[lesser]] is equals to its [[greater]].
    *
    * {{{
    * assert(Interval(1, 1) == true)
    * assert(Interval(1, 2) == false)
    * }}}
    *
    * See [[nonEmpty]] for the opposite check.
    */
  def isEmpty: Boolean = !nonEmpty

  /** Returns `true` if the given [[Interval]] is non-empty, `false` otherwise.
    * An [[Interval]] is non-empty if its [[lesser]] is not equal to its
    * [[greater]].
    *
    * {{{
    * assert(Interval(1, 2) == true)
    * assert(Interval(1, 1) == false)
    * }}}
    *
    * See [[isEmpty]] for the opposite check.
    */
  def nonEmpty: Boolean = x != y

  /** Relates this [[Interval]] and that [[Interval]]. Calling `x.relate(y)`
    * tells you how [[Interval]] x relates to [[Interval]] y. Consult the
    * [[Relation]] documentation for an explanation of all the possible results.
    *
    * @see
    *   [[Relation.from]]
    */
  def relates(that: Interval[T]): Relation = Relation.from(this, that)

  /** Returns `true` if this [[Interval]] is before that [[Interval]], `false`
    * otherwise.
    * @see
    *   [[Relation.Before]]
    */
  def isBefore(that: Interval[T]): Boolean = this.relates(that) == Before

  /** Returns `true` if this [[Interval]] meets that [[Interval]], `false`
    * otherwise.
    * @see
    *   [[Relation.Meets]]
    */
  def meets(that: Interval[T]): Boolean = this.relates(that) == Meets

  /** Returns `true` if this [[Interval]] overlaps that [[Interval]], `false`
    * otherwise.
    * @see
    *   [[Relation.Overlaps]]
    */
  def overlaps(that: Interval[T]): Boolean = this.relates(that) == Overlaps

  /** Returns `true` if this [[Interval]] is finished by that [[Interval]],
    * `false` otherwise.
    * @see
    *   [[Relation.FinishedBy]]
    */
  def isFinishedBy(that: Interval[T]): Boolean = this.relates(that) == FinishedBy

  /** Returns `true` if this [[Interval]] contains that [[Interval]], `false`
    * otherwise.
    * @see
    *   [[Relation.Contains]]
    */
  def contains(that: Interval[T]): Boolean = this.relates(that) == Contains

  /** Returns `true` if this [[Interval]] starts that [[Interval]], `false`
    * otherwise.
    * @see
    *   [[Relation.Starts]]
    */
  def starts(that: Interval[T]): Boolean = this.relates(that) == Starts

  /** Returns `true` if this [[Interval]] is equal to that [[Interval]], `false`
    * otherwise.
    * @see
    *   [[Relation.Equal]]
    */
  def isEqualTo(that: Interval[T]): Boolean = this.relates(that) == Equal

  /** Returns `true` if this [[Interval]] is started by that [[Interval]],
    * `false` otherwise.
    * @see
    *   [[Relation.StartedBy]]
    */
  def isStartedBy(that: Interval[T]): Boolean = this.relates(that) == StartedBy

  /** Returns `true` if this [[Interval]] is during that [[Interval]], `false`
    * otherwise.
    * @see
    *   [[Relation.During]]
    */
  def isDuring(that: Interval[T]): Boolean = this.relates(that) == During

  /** Returns `true` if this [[Interval]] finishes that [[Interval]], `false`
    * otherwise.
    * @see
    *   [[Relation.Finishes]]
    */
  def finishes(that: Interval[T]): Boolean = this.relates(that) == Finishes

  /** Returns `true` if this [[Interval]] is overlapped by that [[Interval]],
    * `false` otherwise.
    * @see
    *   [[Relation.OverlappedBy]]
    */
  def isOverlappedBy(that: Interval[T]): Boolean = this.relates(that) == OverlappedBy

  /** Returns `true` if this [[Interval]] is met by that [[Interval]], `false`
    * otherwise.
    * @see
    *   [[Relation.MetBy]]
    */
  def isMetBy(that: Interval[T]): Boolean = this.relates(that) == MetBy

  /** Returns `true` if this [[Interval]] is after that [[Interval]], `false`
    * otherwise.
    * @see
    *   [[Relation.After]]
    */
  def isAfter(that: Interval[T]): Boolean = this.relates(that) == After
end Interval

object Interval:
  /** Converts the two `T` values into an [[Interval]].
    *
    * Note that this requires an `Ordering[T]` instance so that the Interval can
    * be sorted on construction. If `T` implements `Comparable[T]` etc., it can
    * be implicitly derived by importing `scala.math.Ordering.Implicits`.
    *
    * {{{
    * import java.time.Instant
    * import scala.math.Ordering.Implicits.*
    *
    * // can compile
    * val i = Interval(Instant.now(), Instant.now())
    * }}}
    *
    * Use [[Interval.extracts]] to go in the other direction.
    */
  def apply[T: Ordering](x: T, y: T): Interval[T] = if x > y then new Interval(y, x) else new Interval(x, y)
