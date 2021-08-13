package sttp.tapir.generic.internal

import sttp.tapir.Validator

trait VariousImplicitSchemaStuff {

  def isBasicValue(v: Any): Boolean = v match {
    case _: String     => true
    case _: Int        => true
    case _: Long       => true
    case _: Float      => true
    case _: Double     => true
    case _: Boolean    => true
    case _: BigDecimal => true
    case _: BigInt     => true
    case null          => true
    case _             => false
  }

  implicit class ValidatorSyntax[T](v: Validator[T]) {
    def asPrimitiveValidators: Seq[Validator.Primitive[_]] = {
      def toPrimitives(v: Validator[_]): Seq[Validator.Primitive[_]] = {
        v match {
          case Validator.Mapped(wrapped, _) => toPrimitives(wrapped)
          case Validator.All(validators)    => validators.flatMap(toPrimitives)
          case Validator.Any(validators)    => validators.flatMap(toPrimitives)
          case Validator.Custom(_, _)       => Nil
          case bv: Validator.Primitive[_]   => List(bv)
        }
      }
      toPrimitives(v)
    }

    def traversePrimitives[U](handle: PartialFunction[Validator.Primitive[_], Vector[U]]): Vector[U] =
      asPrimitiveValidators.collect(handle).flatten.toVector

    def inferEnumerationEncode: Validator[T] = {
      v match {
        case Validator.Enumeration(possibleValues, None, name) =>
          if (possibleValues.forall(isBasicValue)) Validator.Enumeration(possibleValues, Some((x: T) => Some(x)), name) else v
        case Validator.Mapped(wrapped, g) => Validator.Mapped(wrapped.inferEnumerationEncode, g)
        case Validator.All(validators)    => Validator.All(validators.map(_.inferEnumerationEncode))
        case Validator.Any(validators)    => Validator.Any(validators.map(_.inferEnumerationEncode))
        case _                            => v
      }
    }
  }
}
