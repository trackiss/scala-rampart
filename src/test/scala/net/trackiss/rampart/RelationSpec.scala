package net.trackiss.rampart

import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.math.Ordering.Implicits.*

class RelationSpec extends AnyFreeSpecLike with Matchers with TableDrivenPropertyChecks:
  import Relation.*

  "Relation" - {
    "from" - {
      "It should return the relation between two intervals" in {
        val fractions = Table(
          ("x", "y", "expects"),
          (Interval(1, 2), Interval(3, 7), Before),
          (Interval(2, 3), Interval(3, 7), Meets),
          (Interval(2, 4), Interval(3, 7), Overlaps),
          (Interval(2, 7), Interval(3, 7), FinishedBy),
          (Interval(2, 8), Interval(3, 7), Contains),
          (Interval(3, 4), Interval(3, 7), Starts),
          (Interval(3, 7), Interval(3, 7), Equal),
          (Interval(3, 8), Interval(3, 7), StartedBy),
          (Interval(4, 6), Interval(3, 7), During),
          (Interval(6, 7), Interval(3, 7), Finishes),
          (Interval(6, 8), Interval(3, 7), OverlappedBy),
          (Interval(7, 8), Interval(3, 7), MetBy),
          (Interval(8, 9), Interval(3, 7), After)
        )

        forAll(fractions) { (x, y, expects) =>
          val actual = Relation.from(x, y)

          actual shouldBe expects
        }
      }
    }
  }

  "it" - {
    "inverted" - {
      "It should return inverted the relation" in {
        val fractions = Table(
          ("original", "expects"),
          (Before, After),
          (After, Before),
          (Meets, MetBy),
          (MetBy, Meets),
          (Overlaps, OverlappedBy),
          (OverlappedBy, Overlaps),
          (Starts, StartedBy),
          (StartedBy, Starts),
          (Finishes, FinishedBy),
          (FinishedBy, Finishes),
          (Contains, During),
          (During, Contains),
          (Equal, Equal)
        )

        forAll(fractions) { (original, expects) =>
          val actual = original.inverted

          actual shouldBe expects
        }
      }
    }

    "It should be equal to it inverted twice" in {
      val relations = Seq(
        Before,
        Meets,
        Overlaps,
        FinishedBy,
        Contains,
        Starts,
        Equal,
        StartedBy,
        During,
        Finishes,
        OverlappedBy,
        MetBy,
        After
      )

      relations.foreach { original =>
        val actual  = original.inverted.inverted
        val expects = original

        actual shouldBe expects
      }
    }

    "It should be equal to inverted relation between the swapped intervals" in {
      val fractions = Table(
        ("x", "y"),
        (Interval(1, 2), Interval(3, 7)),
        (Interval(2, 3), Interval(3, 7)),
        (Interval(2, 4), Interval(3, 7)),
        (Interval(2, 7), Interval(3, 7)),
        (Interval(2, 8), Interval(3, 7)),
        (Interval(3, 4), Interval(3, 7)),
        (Interval(3, 7), Interval(3, 7)),
        (Interval(3, 8), Interval(3, 7)),
        (Interval(4, 6), Interval(3, 7)),
        (Interval(6, 7), Interval(3, 7)),
        (Interval(6, 8), Interval(3, 7)),
        (Interval(7, 8), Interval(3, 7)),
        (Interval(8, 9), Interval(3, 7))
      )

      forAll(fractions) { (x, y) =>
        val actual  = Relation.from(x, y).inverted
        val expects = Relation.from(y, x)

        actual shouldBe expects
      }
    }
  }
end RelationSpec
