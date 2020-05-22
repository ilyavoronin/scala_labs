package lecture10

import cats.syntax.all._
import cats.effect._
import cats.effect.concurrent.{MVar, Ref, Semaphore}

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._

object CounterApp extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    gracefulShutdownProgram.use(_ => IO.never)
  }

  val gracefulShutdownProgram: Resource[IO, Unit] = for {
    mvar <- Resource.make(MVar.empty[IO, String])(_ => IO(println("mvar destroyed")))
    _ <- runPrinter(mvar)
    _ <- runCounter(mvar)
  } yield ()

  def runPrinter(mvar: MVar[IO, String]): Resource[IO, Unit] = {
    def rec: IO[Unit] = for {
      value <- mvar.take
      _ <- IO(println(value))
      _ <- rec
    } yield ()

    runCycle(rec.start, "Printer closed")
  }

  def runCounter(mvar: MVar[IO, String]): Resource[IO, Unit] = {
    def rec(counter: Long): IO[Unit] = for {
      _ <- mvar.put(counter.toString)
      _ <- IO.sleep(1.seconds)
      _ <- rec(counter + 1)
    } yield ()

    runCycle(rec(0).start, "Counter closed")
  }

  private def runCycle(actionCycle: IO[Fiber[IO, Unit]], closeMessage: String = "closed"): Resource[IO, Unit] =
    Resource.make(actionCycle)(_.cancel.flatMap(_ => IO(println(closeMessage)))).void
}
