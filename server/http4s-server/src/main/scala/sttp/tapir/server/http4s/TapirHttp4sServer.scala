package sttp.tapir.server.http4s

import cats.Monad
import cats.effect.{ContextShift, Sync}
import cats.implicits._
import org.http4s.{EntityBody, HttpRoutes}
import sttp.tapir.Endpoint
import sttp.tapir.Endpoint
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.typelevel.ReplaceFirstInTuple

import scala.reflect.ClassTag
import reflect.runtime.universe.TypeTag

trait TapirHttp4sServer {
  implicit class RichHttp4sHttpEndpoint[I, E: TypeTag, O: TypeTag, F[_]](e: Endpoint[I, E, O, EntityBody[F]]) {
    def toRoutes(
        logic: I => F[Either[E, O]]
    )(implicit serverOptions: Http4sServerOptions[F], fs: Sync[F], fcs: ContextShift[F]): HttpRoutes[F] = {
      new EndpointToHttp4sServer(serverOptions).toRoutes(e.serverLogic(logic))
    }

    def toRouteRecoverErrors(logic: I => F[O])(
        implicit serverOptions: Http4sServerOptions[F],
        fs: Sync[F],
        fcs: ContextShift[F],
        eIsThrowable: E <:< Throwable,
        eClassTag: ClassTag[E]
    ): HttpRoutes[F] = {
      new EndpointToHttp4sServer(serverOptions).toRoutesRecoverErrors(e)(logic)
    }
  }

  implicit class RichHttp4sServerEndpoint[I, E: TypeTag, O: TypeTag, F[_]](se: ServerEndpoint[I, E, O, EntityBody[F], F]) {
    def toRoutes(implicit serverOptions: Http4sServerOptions[F], fs: Sync[F], fcs: ContextShift[F]): HttpRoutes[F] =
      new EndpointToHttp4sServer(serverOptions).toRoutes(se)
  }

  implicit class RichHttp4sServerEndpoints[F[_]](serverEndpoints: List[ServerEndpoint[_, _, _, EntityBody[F], F]]) {
    def toRoutes(implicit serverOptions: Http4sServerOptions[F], fs: Sync[F], fcs: ContextShift[F]): HttpRoutes[F] = {
      new EndpointToHttp4sServer(serverOptions).toRoutes(serverEndpoints)
    }
  }

  implicit class RichToMonadFunction[T, U, F[_]: Monad](a: T => F[U]) {
    def andThenFirst[U_TUPLE, T_TUPLE, O](
        l: U_TUPLE => F[O]
    )(implicit replaceFirst: ReplaceFirstInTuple[T, U, T_TUPLE, U_TUPLE]): T_TUPLE => F[O] = { tTuple =>
      val t = replaceFirst.first(tTuple)
      a(t).flatMap { u =>
        val uTuple = replaceFirst.replace(tTuple, u)
        l(uTuple)
      }
    }
  }

  implicit class RichToMonadOfEitherFunction[T, U, E, F[_]: Monad](a: T => F[Either[E, U]]) {
    def andThenFirstE[U_TUPLE, T_TUPLE, O](
        l: U_TUPLE => F[Either[E, O]]
    )(implicit replaceFirst: ReplaceFirstInTuple[T, U, T_TUPLE, U_TUPLE]): T_TUPLE => F[Either[E, O]] = { tTuple =>
      val t = replaceFirst.first(tTuple)
      a(t).flatMap {
        case Left(e) => implicitly[Monad[F]].point(Left(e))
        case Right(u) =>
          val uTuple = replaceFirst.replace(tTuple, u)
          l(uTuple)
      }
    }
  }
}
