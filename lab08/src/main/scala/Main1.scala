package lab8

import java.nio.file.{Files, Path, Paths}

import cats.{Applicative, Functor, Id, Monad}
import cats.syntax.all._
import cats.instances.list._

import scala.language.higherKinds

trait MkDir[F[_], Dir] {
  def mkDir(dir: Dir, name: String): F[Dir]
}

trait MkFile[F[_], Dir, File] {
  def mkFile(dir: Dir, name: String): F[File]
}

trait MoveFile[F[_], Dir, File] {
  def moveFile(file: File, destDir: Dir): F[File]
}

trait GetFiles[F[_], Dir, File] {
  def getFiles(dir: Dir): F[List[File]]
}

trait FileHandler[F[_], File] {
  def getFirstLetters(files: List[File]): F[List[Char]]
}

trait Printer[F[_], File] {
  def printFileNames(files: List[File]): F[Unit]
}

class Program[F[_], Dir, File](implicit
                               F: Monad[F],
                               mkDir: MkDir[F, Dir],
                               mkFile: MkFile[F, Dir, File],
                               mvFile: MoveFile[F, Dir, File],
                               getFiles: GetFiles[F, Dir, File],
                               fileHandler: FileHandler[F, File],
                               printer: Printer[F, File]) {
  def run(dir: Dir): F[Unit] = for {
    testDir <- mkDir.mkDir(dir, "test_dir")
    _ <- mkFile.mkFile(testDir, "foo")
    _ <- mkFile.mkFile(testDir, "bar")
    _ <- mkFile.mkFile(testDir, "baz")
    files <- getFiles.getFiles(testDir)
    _ <- printer.printFileNames(files)
    flet <- fileHandler.getFirstLetters(files)
    _ <- flet.zip(files).traverse { case (fl, file) =>
      for {
        ddir <- mkDir.mkDir(dir, fl.toString)
        _ <- mvFile.moveFile(file, ddir)
      } yield ()
    }

  } yield ()
}

class RealFileSystem[F[_] : Applicative] extends MkDir[F, Path]
  with MkFile[F, Path, Path]
  with MoveFile[F, Path, Path]
  with FileHandler[F, Path]
  with GetFiles[F, Path, Path]
{
  override def mkDir(dir: Path, name: String): F[Path] =
    Files.createDirectories(dir.resolve(name)).pure[F]

  override def mkFile(dir: Path, name: String): F[Path] =
    Files.createFile(dir.resolve(name)).pure[F]

  override def moveFile(file: Path, destDir: Path): F[Path] =
    Files.move(file, destDir.resolve(file.getFileName)).pure[F]

  override def getFirstLetters(files: List[Path]): F[List[Char]] =
    files.map(file => file.getFileName.toString.head).pure[F]

  override def getFiles(dir: Path): F[List[Path]] =
    dir.toFile.listFiles().map(_.toPath).toList.pure[F]
}

class ConsolePrinter[F[_] : Applicative] extends Printer[F, Path] {
  override def printFileNames(files: List[Path]): F[Unit] =
    files.foreach(f => println(f.getFileName.toString)).pure[F]
}

object Main1 {
  def main(args: Array[String]): Unit = {
    implicit val fs: RealFileSystem[Id] = new RealFileSystem[Id]
    implicit val printer: ConsolePrinter[Id] = new ConsolePrinter[Id]
    val program = new Program[Id, Path, Path]()

    program.run(Paths.get("."))
  }
}
