package issue

import zio._

object NoIssue extends ZIOAppDefault {

  class Console {
    def print(s: String): UIO[Unit] = ZIO.debug(s)
  }

  class Cleanup(pool: ZPoolWithoutDisconnect[Nothing, Console]) {
    def cleanup: UIO[Unit] = ZIO.scoped(pool.get.flatMap(_.print("Clean")))
  }

  val cleanupLayer: ULayer[Cleanup] = ZLayer.scoped {
    ZPoolWithoutDisconnect.make(ZIO.succeed(new Console), 2).map(new Cleanup(_))
  }

  val scopedPrinter: URIO[Scope with Cleanup, Unit] = for {
    c <- ZIO.service[Cleanup]
    _ <- ZIO
      .debug("running...")
      .repeat(Schedule.spaced(1.seconds))
      .ensuring(
        ZIO.debug("Stopping") *> c.cleanup.tapErrorCause(t => ZIO.logErrorCause(s"Error cleaning up", t)).ignore *> ZIO
          .debug(
            "Stopped"
          )
      )
      .forkScoped
      .ignore
  } yield ()

  val printerLayer: URLayer[Cleanup, Unit] = ZLayer.scoped {
    scopedPrinter
  }

  override def run: ZIO[ZIOAppArgs with Scope, Any, Any] = ZIO
    .sleep(2.seconds)
    .provide(
      cleanupLayer,
      printerLayer
    )
}
