package scala2021.ayafimau.task07

import java.io.{File, FileInputStream}
import scala.util.{Failure, Success, Try}

object MainWithTryMonad extends App {

  withResource {
    Connection(port = 9000)
  } {
    conn => conn.run()
  } {
    _.close()
  }

  val tempFile = File.createTempFile("pre-", ".txt")
  tempFile.deleteOnExit()
  val filePath = tempFile.toPath.toString

  withResource(new File(filePath)) {
    file => {
      file.exists()
    }
  }()

  withResource(new File(filePath)) {
    file => {
      file.exists()
      // throw new Exception("RUN   exception")
      // println("file exists check")
    }
  }(_ => {
    // throw new Exception("CLOSE  exception")
    // println("closed file")
  })

  withResource(new FileInputStream(filePath)) {
    fis => {
      val bytes = fis.available()
      println(s"Count bytes are $bytes")
    }
  }(_.close())

  //automatic resource management
  def withResource[T, U]
  (obtainResource: => T)
  (block: T => U)
  (release: T => Unit = (_: T) => ()): U = {

    val executionResult: Try[U] = for {
      resource <- Try(obtainResource)
      // wrap around in another try just to execute consequent release-logic regardless of success or failure
      result <- Try(Try(block(resource)))
      // wrap around in another try, since we might want to return not a release-logic exception, but the exception from above "block(resource)"
      releaseResult <- Try(Try(release(resource)))
    } yield {
      if (result.isFailure)
        result.get
      else if (releaseResult.isFailure)
      // this is not intended for casting to U, this is just to throw Exception in get, but cast required to satisfy compilation
        releaseResult.get.asInstanceOf[U]
      else
        result.get
    }

    executionResult match {
      case Success(result) => result
      case Failure(e) => throw e
    }
  }

  case class Connection(port: Int) {
    def close(): Unit = println("Closed")

    def run(): Unit = println("Run")
  }

}
