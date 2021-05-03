package scala2021.ayafimau.task07

import java.io.{File, FileInputStream}

object Main extends App {

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
  (release: T => Unit = (_: T) => ()) = {

    val tryObtainResource = try {
      Right(obtainResource)
    } catch {
      case e: Exception => Left(e)
    }

    tryObtainResource match {
      case Right(resource) =>
        try {
          block(resource)
        } finally {
          release(resource)
        }
      //nothing to release if we failed to obtain the resource itself
      case Left(e) => throw e
    }
  }

  case class Connection(port: Int) {
    def close(): Unit = println("Closed")

    def run(): Unit = println("Run")
  }

}
