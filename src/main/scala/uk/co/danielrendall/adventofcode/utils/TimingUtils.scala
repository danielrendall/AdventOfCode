package uk.co.danielrendall.adventofcode.utils

object TimingUtils {

  def timed[T](name: String)(block: =>T ): T = {
    val start = System.currentTimeMillis()
    try {
      block

    } finally {
      val end = System.currentTimeMillis()
      println(s"$name: ${end - start}ms")
    }
  }

}
