package com.fd.modelling.day1.session2

import scala.collection.mutable

class Purity() {

  //We accumulate logged messages here
  val fakeLog: mutable.Buffer[String] = mutable.Buffer.empty

  //A function we'll use to log messages to our logger
  def logMsg(s: String): Unit = fakeLog += s

  //We get our names from a resource over the network
  //While we will not do so in this test, use your imagination and pretend this is happening.
  def getNamesFromNetwork: List[String] = {
    //logMsg("Got all names from across the network")
    List("Aphrodite", "Ares", "Demeter", "Dionysus", "Hermes", "Zeus")
  }

  //Now we "publish" our names to some (fake) remote service, one at a time
  def publishName(name: String): String = {
    //logMsg(s"Publishing: $name")
    name
  }

  //Here is our main program. We want to constrain all side-effects to here, and have a "pure core".
  //Currently, that is not the case, and the entire application logic is side-effecting.
  //Rewrite this function to use `pureCore` below and separate out the pure code from the impure code.
  def mainProcedure: List[String] = {
    logMsg("Got all names from across the network")
    val names = getNamesFromNetwork
    val (containsH, doesNotContainH) = pureCore(names)
    containsH.foreach(s => logMsg(s"Filtered out name: $s"))
    doesNotContainH.foreach { n =>
      logMsg(s"Publishing: $n")
      publishName(n)
    }
    doesNotContainH
  }

  //Define a function `pureCore` here that performs the operations above that we know are "pure"
  //It should return both the names filtered-out (left-side) and the correct values (right-side)
  def pureCore(names: List[String]): (List[String], List[String]) = {
    val (containsH, doesNotContainH) =
      names.partition(_.toLowerCase.contains("h"))
    containsH -> doesNotContainH.map(s => s"Greek God/Goddess: $s")
  }

}
