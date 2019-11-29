package sttp.tapir.example

import org.http4s.{EntityBody, HttpRoutes}
import _root_.zio.{IO, Task}
import sttp.tapir.Endpoint
import sttp.tapir.server.http4s.Http4sServerOptions
import _root_.zio.interop.catz._
import sttp.tapir.server.ServerEndpoint
import reflect.runtime.universe.TypeTag

package object zio {
  implicit class ZioEndpoint[I, E: TypeTag, O: TypeTag](e: Endpoint[I, E, O, EntityBody[Task]]) {
    def toZioRoutes(logic: I => IO[E, O])(implicit serverOptions: Http4sServerOptions[Task]): HttpRoutes[Task] = {
      import sttp.tapir.server.http4s._
      e.toRoutes(i => logic(i).either)
    }

    def zioServerLogic(logic: I => IO[E, O]): ServerEndpoint[I, E, O, EntityBody[Task], Task] = ServerEndpoint(e, logic(_).either)
  }
}
