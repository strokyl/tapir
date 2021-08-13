package sttp.tapir.internal

import sttp.tapir.Schema
import sttp.tapir.SchemaType.SBinary

import java.nio.file.Path

trait SchemaExtensions {
  implicit val schemaForPath: Schema[Path] = Schema(SBinary())
}
