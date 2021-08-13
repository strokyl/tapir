package sttp.tapir

import scala.annotation.StaticAnnotation

package object annotations {
  class description(val text: String) extends StaticAnnotation
  class encodedExample(val example: Any) extends StaticAnnotation
  class default[T](val default: T) extends StaticAnnotation
  class format(val format: String) extends StaticAnnotation
  class deprecated extends StaticAnnotation
  class encodedName(val name: String) extends StaticAnnotation
}
