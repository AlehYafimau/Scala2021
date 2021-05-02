package scala2021.ayafimau.task07

import java.io.{File, FileInputStream}

object Main extends App {

  withResource(Connection(port = 9000)) {
    conn => conn.run()
  }(conn => if (conn != null) conn.close())

  val tempFile = File.createTempFile("pre-", ".txt")
  tempFile.deleteOnExit()
  val filePath = tempFile.toPath.toString

  withResource(new File(filePath)) {
    file => file.exists()
  }_

  withResource(new FileInputStream(filePath)){
    fis => {
      val bytes = fis.available()
      println(s"Count bytes are $bytes")
    }
  }(fis => if (fis != null) fis.close())

  //automatic resource management
  def withResource[T, U]
  (resource: T)
  (block: T => U)
  (release: T => Unit = (_:T) => ()) = {
    try {
      block(resource)
    } finally {
      release(resource)
    }
  }

  case class Connection(port: Int) {
    def close(): Unit = println("Closed")
    def run(): Unit = println("Run")
  }
}
