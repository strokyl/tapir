package sttp.tapir.internal

import org.scalatest.{FlatSpec, Matchers}
import reflect.runtime.universe.typeOf

class ParamsToSeqWithTypeTest extends FlatSpec with Matchers {
  "ParamsToSeqWithType" should "transform double tuple in seq of tuple associating Type with correct value" in {
    val result = ParamsToSeqWithType((42, true))

    result should equal(Seq(
      Some(typeOf[Int]) -> 42,
      Some(typeOf[Boolean]) -> true
    ))
  }

  "ParamsToSeqWithType" should "transform simple value in seq one element with correct type" in {
    val result = ParamsToSeqWithType(42)

    result should equal(Seq(
      Some(typeOf[Int]) -> 42
    ))
  }

  "ParamsToSeqWithType" should "handle correctly no value" in {
    val result = ParamsToSeqWithType(())

    result should equal(Seq())
  }

  "ParamsToSeqWithType" should "handle correctly complexe type" in {
    val result = ParamsToSeqWithType((
      Some(Right[Int, Boolean](true)),
      Some(Left[Int, Boolean](0)),
      None
    ))

    result should equal(Seq(
      Some(typeOf[Some[scala.util.Right[Int, Boolean]]]) -> Some(Right[Int, Boolean](true)),
      Some(typeOf[Some[scala.util.Left[Int, Boolean]]]) -> Some(Left[Int, Boolean](0)),
      Some(typeOf[None.type]) -> None
    ))
  }

  "ParamsToSeqWithType" should "at least return correctly valure when no correct type" in {
    val result = ParamsToSeqWithType((
      Some(Right[Int, Boolean](true)),
      Some(Left[Int, Boolean](0)),
      None
    ): Any)

    result should equal(Seq(
      None -> Some(Right[Int, Boolean](true)),
      None -> Some(Left[Int, Boolean](0)),
      None -> None
    ))
  }
}
