package uk.co.danielrendall.adventofcode.utils

import java.io.{BufferedReader, InputStream, InputStreamReader}
import java.nio.charset.StandardCharsets
import scala.collection.{Factory, mutable}
import scala.jdk.Accumulator
import scala.jdk.StreamConverters.*

object StreamUtils {
  extension (stream: InputStream)
    def lines: LazyList[String] =
      new BufferedReader(new InputStreamReader(stream, StandardCharsets.UTF_8)).lines().toScala(LazyList)

}
