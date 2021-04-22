package scala2021.ayafimau.task03

object Main extends App {

  println(encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))

  def encodeDirect(source: List[Symbol]): List[(Int, Symbol)] = {

    def encodeInternal(sourceTail: List[Symbol], countLast: Option[(Int, Symbol)]): List[(Int, Symbol)] = {
      sourceTail match {
        case head :: rest => countLast match {
          case Some((count, `head`)) => encodeInternal(rest, Some((count + 1, head)))
          case prev@Some((_, _)) => prev.value :: encodeInternal(rest, Some((1, head)))
          case _ => encodeInternal(rest, Some((1, head)))
        }
        case _ => countLast match {
          case prev@Some((_, _)) => List(prev.value)
          case _ => Nil
        }
      }
    }

    encodeInternal(source, None)
  }
}
