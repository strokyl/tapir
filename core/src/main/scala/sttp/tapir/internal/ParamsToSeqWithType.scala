package sttp.tapir.internal

import scala.reflect.runtime.universe.{TypeTag, Type, typeOf}

private[tapir] object ParamsToSeqWithType {
  def apply[T: TypeTag](a: T): Seq[(Option[Type], Any)] = {
    val topType =  typeOf[T]
    val seqWithoutType = ParamsToSeq(a)
    seqWithoutType match {
      case Seq()            => Seq()
      case Seq(singleValue) => Seq(Some(topType) -> singleValue)
      case _                => {
        if (seqWithoutType.size != topType.typeArgs.size) {
          //TypeTag get lost somehow
          seqWithoutType.map(None -> _)
        } else {
          topType.typeArgs.map(Some(_)).zip(seqWithoutType)
        }
      }
    }
  }
}
