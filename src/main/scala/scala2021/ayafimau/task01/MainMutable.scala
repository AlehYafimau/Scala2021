package scala2021.ayafimau.task01

import scala.collection.mutable

object MainMutable extends App {
  val counts = Array(
    "900,google.com",
    "60,mail.yahoo.com",
    "10,mobile.sports.yahoo.com",
    "40,sports.yahoo.com",
    "10,stackoverflow.com",
    "2,en.wikipedia.org",
    "1,es.wikipedia.org",
    "1,mobile.sports"
  )

  val countsAggregated = rollupCounts(counts)

  println(countsAggregated.mkString("\r\n"))

  def rollupCounts(rawCounts: Array[String]): Array[(Int, String)] = {
    val treeRoot = new TreeNode("", 0)

    val pattern = "(\\d+),(.*)".r
    for (record <- rawCounts) {
      val pattern(hitCount, domain) = record
      val subdomains = domain.split('.')

      treeRoot.addSubPath(subdomains.reverse, hitCount.toInt)
    }

    treeRoot.print()
  }

  class TreeNode(subDomain: String, var hitCount: Int) {

    val children: mutable.Map[String, TreeNode] = mutable.Map()

    def addSubPath(subPath: Array[String], rawHitCount: Int): Unit = {
      subPath match {
        case Array(head, tail@_*) =>

          val child = children.getOrElseUpdate(head, {
            val childDomain = if (subDomain == "") head else head + "." + subDomain
            new TreeNode(childDomain, 0)
          })
          child.hitCount += rawHitCount
          if (tail.nonEmpty) {
            child.addSubPath(tail.toArray, rawHitCount)
          }
        case _ => throw new IllegalArgumentException("Domain cannot be empty")
      }
    }

    def print(): Array[(Int, String)] = {
      val currentOutput = if (subDomain == "") Array[(Int, String)]() else Array((hitCount, subDomain))

      currentOutput ++ children.values.flatMap(x => x.print())
    }
  }

}



