package annotations

import org.specs2.mutable.Specification

case class SW(s: String)
case class IW(c: Int)
case class DW(d: Double)
case class PD(sw: SW, iw: IW, bw: DW)

case class InnerInner(i: Int)
case class Inner(i: Int, inner: InnerInner)
case class Outer(i: Int, inner: Inner)

class TemplateSpec extends Specification {

  "Template" should {
    "process nested patterns" in {
      @template(
        (i1: Int, i2: Int, i3: Int) => Outer(i1, Inner(i2, InnerInner(i3)))
      ) object Flatten
      Flatten apply(1, 2, 3) mustEqual Outer(1, Inner(2, InnerInner(3)))
      Flatten unapply Outer(1, Inner(2, InnerInner(3))) mustEqual Some(1, 2, 3)
    }

    "has same ordered tuple in unapply result as the apply function arguments" in {
      @template(
        (i: Int, s: String, d: Double) => PD(SW(s), IW(i), DW(d))
      ) object OrderTest

      OrderTest apply(1, "s", 2d) mustEqual PD(SW("s"), IW(1), DW(2d))
      OrderTest unapply PD(SW("s"), IW(1), DW(2d)) mustEqual Some(1, "s", 2d)
    }
  }

}
