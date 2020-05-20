import java.nio.file.{Files, Path, Paths}

import cats.Id
import lab8.{ConsolePrinter, Program, RealFileSystem}
import org.scalatest.funsuite.AnyFunSuite

import scala.reflect.io.{Directory, File}

class RealFilesystemTest extends AnyFunSuite {
  val tempDir = Files.createTempDirectory("test")
  implicit val fs: RealFileSystem[Id] = new RealFileSystem[Id]
  implicit val printer: ConsolePrinter[Id] = new ConsolePrinter[Id]
  val program = new Program[Id, Path, Path]()

  program.run(tempDir)

  test("All folders exist") {
    assert(Files.exists(tempDir.resolve("b")))
    assert(Files.exists(tempDir.resolve("f")))
  }

  test("All files exist") {
    assert(Files.exists(tempDir.resolve(Paths.get("b", "bar"))))
    assert(Files.exists(tempDir.resolve(Paths.get("b", "baz"))))
    assert(Files.exists(tempDir.resolve(Paths.get("f", "foo"))))
  }

  val dir = new Directory(tempDir.toFile)
  Runtime.getRuntime.addShutdownHook(new Thread {
    override def run(): Unit = dir.deleteRecursively()
  })
}
