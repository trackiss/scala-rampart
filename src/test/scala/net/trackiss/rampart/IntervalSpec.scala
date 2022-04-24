package net.trackiss.rampart

import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class IntervalSpec extends AnyFreeSpecLike with Matchers with TableDrivenPropertyChecks:
  "Interval" - {
    "apply" - {
      "It should return the interval between two values" in {
        noException `should` be `thrownBy` Interval(1, 1)
        noException `should` be `thrownBy` Interval(1, 2)
        noException `should` be `thrownBy` Interval(2, 1)
      }
    }
  }

  "it" - {
    "lesser" - {
      "It should return the lesser value" in {
        Interval(1, 2).lesser shouldBe 1
        Interval(2, 1).lesser shouldBe 1

        val x = 123
        val y = 456

        Interval(x, y).lesser shouldBe x.min(y)
      }
    }

    "greater" - {
      "It should return the greater value" in {
        Interval(1, 2).greater shouldBe 2
        Interval(2, 1).greater shouldBe 2

        val x = 123
        val y = 456

        Interval(x, y).greater shouldBe x.max(y)
      }
    }

    "extract" - {
      "It should return the two values" in {
        Interval(1, 2).extracts shouldBe (1, 2)
        Interval(2, 1).extracts shouldBe (1, 2)

        val x = 123
        val y = 456

        Interval(x, y).extracts shouldBe (x.min(y), x.max(y))
      }
    }

    "isEmpty" - {
      "It should return whether there is an interval between the two values" in {
        Interval(1, 1).isEmpty shouldBe true
        Interval(1, 2).isEmpty shouldBe false
      }
    }

    "nonEmpty" - {
      "It should return whether there is not an interval between the two values" in {
        Interval(1, 2).nonEmpty shouldBe true
        Interval(1, 1).nonEmpty shouldBe false
      }
    }
  }
end IntervalSpec
