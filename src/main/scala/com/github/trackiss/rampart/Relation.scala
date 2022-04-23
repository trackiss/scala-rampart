package com.github.trackiss.rampart

import scala.annotation.targetName

/** This type describes how two [[Interval]]s relate to each other. Each
  * constructor represents one of the 13 possible relations. Taken together
  * these relations are mutually exclusive and exhaustive.
  *
  * Use `Relation.from(x, y)` to determine the relation between [[Interval]] x
  * and [[Interval]] y.
  *
  * The following image shows all 13 possible [[Interval]] relations. If for
  * whatever reason you can't see the image, each constructor for this type has
  * ASCII art showing the [[Interval]] relation.
  */
sealed trait Relation:
  import Relation.*

  /** Inverts a [[Relation]]. Every [[Relation]] has an inverse.
    *
    * {{{
    * import Relation.*
    *
    * assert(Before.inverted       == After)
    * assert(After.inverted        == Before)
    * assert(Meets.inverted        == MetBy)
    * assert(MetBy.inverted        == Meets)
    * assert(Overlaps.inverted     == OverlappedBy)
    * assert(OverlappedBy.inverted == Overlaps)
    * assert(Starts.inverted       == StartedBy)
    * assert(StartedBy.inverted    == Starts)
    * assert(Finishes.inverted     == FinishedBy)
    * assert(FinishedBy.inverted   == Finishes)
    * assert(Contains.inverted     == During)
    * assert(During.inverted       == Contains)
    * assert(Equal.inverted        == Equal)
    * }}}
    *
    * Inverting a [[Relation]] twice will return the original [[Relation]].
    *
    * {{{
    * assert(r.inverted.inverted == r)
    * }}}
    *
    * Inverting a [[Relation]] is like swapping the arguments to
    * `Relation.from`.
    *
    * {{{
    * assert(Relation.from(x, y).inverted == Relation.from(y, x))
    * }}}
    */
  def inverted: Relation = this match
    case Before       => After
    case After        => Before
    case Meets        => MetBy
    case MetBy        => Meets
    case Overlaps     => OverlappedBy
    case OverlappedBy => Overlaps
    case FinishedBy   => Finishes
    case Finishes     => FinishedBy
    case Contains     => During
    case During       => Contains
    case Starts       => StartedBy
    case StartedBy    => Starts
    case Equal        => Equal

  @targetName("inverted_ops")
  def unary_! : Relation = this.inverted
end Relation

object Relation:
  /** Determine the relation between [[Interval]] x and [[Interval]] y.
    *
    * {{{
    * import Relation.*
    *
    * assert(Relation.from(Interval(1, 2), Interval(3, 7)) == Before)
    * assert(Relation.from(Interval(2, 3), Interval(3, 7)) == Meets)
    * assert(Relation.from(Interval(2, 4), Interval(3, 7)) == Overlaps)
    * assert(Relation.from(Interval(2, 7), Interval(3, 7)) == FinishedBy)
    * assert(Relation.from(Interval(2, 8), Interval(3, 7)) == Contains)
    * assert(Relation.from(Interval(3, 4), Interval(3, 7)) == Starts)
    * assert(Relation.from(Interval(3, 7), Interval(3, 7)) == Equal)
    * assert(Relation.from(Interval(3, 8), Interval(3, 7)) == StartedBy)
    * assert(Relation.from(Interval(4, 6), Interval(3, 7)) == During)
    * assert(Relation.from(Interval(6, 7), Interval(3, 7)) == Finishes)
    * assert(Relation.from(Interval(6, 8), Interval(3, 7)) == OverlappedBy)
    * assert(Relation.from(Interval(7, 8), Interval(3, 7)) == MetBy)
    * assert(Relation.from(Interval(8, 9), Interval(3, 7)) == After)
    * }}}
    *
    * Note that relating an empty interval with a non-empty interval may be
    * surprising when the intervals share an endpoint.
    *
    * {{{
    * import Relation.*
    *
    * assert(Relation.from(Interval(3, 3), Interval(3, 7)) == Overlaps)
    * assert(Relation.from(Interval(7, 7), Interval(3, 7)) == OverlappedBy)
    * assert(Relation.from(Interval(3, 7), Interval(3, 3)) == OverlappedBy)
    * assert(Relation.from(Interval(3, 7), Interval(7, 7)) == Overlaps)
    * }}}
    */
  def from[T: Ordering](x: Interval[T], y: Interval[T]): Relation =
    import OrderingUtil.*
    import CompareResult.*

    val ord = summon[Ordering[T]]

    val ll = ord.cmp(x.lesser, y.lesser)
    val lg = ord.cmp(x.lesser, y.greater)
    val gl = ord.cmp(x.greater, y.lesser)
    val gg = ord.cmp(x.greater, y.greater)

    (ll, lg, gl, gg) match
      case (EQ, _, _, EQ)  => Relation.Equal
      case (_, _, LT, _)   => Relation.Before
      case (LT, _, EQ, LT) => Relation.Meets
      case (_, _, EQ, _)   => Relation.Overlaps
      case (GT, EQ, _, GT) => Relation.MetBy
      case (_, EQ, _, _)   => Relation.OverlappedBy
      case (_, GT, _, _)   => Relation.After
      case (LT, _, _, LT)  => Relation.Overlaps
      case (LT, _, _, EQ)  => Relation.FinishedBy
      case (LT, _, _, GT)  => Relation.Contains
      case (EQ, _, _, LT)  => Relation.Starts
      case (EQ, _, _, GT)  => Relation.StartedBy
      case (GT, _, _, LT)  => Relation.During
      case (GT, _, _, EQ)  => Relation.Finishes
      case (GT, _, _, GT)  => Relation.OverlappedBy
  end from

  /** [[Interval]] `x` is before [[Interval]] `y`.
    *
    * {{{
    * assert(x.greater < y.lesser)
    * }}}
    * {{{
    * +---+
    * | x |
    * +---+
    *       +---+
    *       | y |
    *       +---+
    * }}}
    */
  case object Before extends Relation

  /** [[Interval]] `x` meets [[Interval]] `y`.
    *
    * {{{
    * assert(x.nonEmpty)
    * assert(x.greater == y.lesser)
    * }}}
    * {{{
    * +---+
    * | x |
    * +---+
    *     +---+
    *     | y |
    *     +---+
    * }}}
    */
  case object Meets extends Relation

  /** [[Interval]] `x` overlaps [[Interval]] `y`.
    *
    * {{{
    * assert(x.lesser < y.lesser)
    * assert(x.greater > y.lesser)
    * assert(x.greater < y.greater)
    * }}}
    * {{{
    * +---+
    * | x |
    * +---+
    *   +---+
    *   | y |
    *   +---+
    * }}}
    */
  case object Overlaps extends Relation

  /** [[Interval]] `x` is finished by [[Interval]] `y`.
    *
    * {{{
    * assert(y.nonEmpty)
    * assert(x.lesser < y.lesser)
    * assert(x.greater == y.greater)
    * }}}
    * {{{
    * +-----+
    * |  x  |
    * +-----+
    *   +---+
    *   | y |
    *   +---+
    * }}}
    */
  case object FinishedBy extends Relation

  /** [[Interval]] `x` contains [[Interval]] `y`.
    *
    * {{{
    * assert(x.lesser < y.lesser)
    * assert(x.greater > y.greater)
    * }}}
    * {{{
    * +-------+
    * |   x   |
    * +-------+
    *   +---+
    *   | y |
    *   +---+
    * }}}
    */
  case object Contains extends Relation

  /** [[Interval]] `x` starts [[Interval]] `y`.
    *
    * {{{
    * assert(x.nonEmpty)
    * assert(x.lesser == y.lesser)
    * assert(x.greater < y.greater)
    * }}}
    * {{{
    * +---+
    * | x |
    * +---+
    * +-----+
    * |  y  |
    * +-----+
    * }}}
    */
  case object Starts extends Relation

  /** [[Interval]] `x` is equal to [[Interval]] `y`.
    *
    * {{{
    * assert(x.lesser == y.lesser)
    * assert(x.greater == y.greater)
    * }}}
    * {{{
    * +---+
    * | x |
    * +---+
    * +---+
    * | y |
    * +---+
    * }}}
    */
  case object Equal extends Relation

  /** [[Interval]] `x` is started by [[Interval]] `y`.
    *
    * {{{
    * assert(y.nonEmpty)
    * assert(x.lesser == y.lesser)
    * assert(x.greater > y.greater)
    * }}}
    * {{{
    * +-----+
    * |  x  |
    * +-----+
    * +---+
    * | y |
    * +---+
    * }}}
    */
  case object StartedBy extends Relation

  /** [[Interval]] `x` is during [[Interval]] `y`.
    *
    * {{{
    * assert(x.lesser > y.lesser)
    * assert(x.greater < y.greater)
    * }}}
    * {{{
    *   +---+
    *   | x |
    *   +---+
    * +-------+
    * |   y   |
    * +-------+
    * }}}
    */
  case object During extends Relation

  /** [[Interval]] `x` finishes [[Interval]] `y`.
    *
    * {{{
    * assert(x.nonEmpty)
    * assert(x.lesser > y.lesser)
    * assert(x.greater == y.greater)
    * }}}
    * {{{
    *   +---+
    *   | x |
    *   +---+
    * +-----+
    * |  y  |
    * +-----+
    * }}}
    */
  case object Finishes extends Relation

  /** [[Interval]] `x` is overlapped by [[Interval]] `y`.
    *
    * {{{
    * assert(x.lesser > y.lesser)
    * assert(x.lesser < y.greater)
    * assert(x.greater > y.greater)
    * }}}
    * {{{
    *   +---+
    *   | x |
    *   +---+
    * +---+
    * | y |
    * +---+
    * }}}
    */
  case object OverlappedBy extends Relation

  /** [[Interval]] `x` is met by [[Interval]] `y`.
    *
    * {{{
    * assert(y.nonEmpty)
    * assert(x.lesser == y.greater)
    * }}}
    * {{{
    *     +---+
    *     | x |
    *     +---+
    * +---+
    * | y |
    * +---+
    * }}}
    */
  case object MetBy extends Relation

  /** [[Interval]] `x` is after [[Interval]] `y`.
    *
    * {{{
    * assert(x.lesser > y.greater)
    * }}}
    * {{{
    *       +---+
    *       | x |
    *       +---+
    * +---+
    * | y |
    * +---+
    * }}}
    */
  case object After extends Relation
end Relation
