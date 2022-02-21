package io.navidjalali.swoosh

import io.navidjalali.swoosh.models.HexString
import io.navidjalali.swoosh.parse.Parser

import java.nio.charset.StandardCharsets
import scala.util.Try

object Main {
  def main(args: Array[String]): Unit = {
    val file = HexString.fromString("ffd8ffe000104a464946000101020076".toUpperCase).get
    val jpeg = HexString.fromString("FFD8FFE000104A4649460001").get
    val r = Parser.sequence(jpeg.underlyingBytes).map(_ => "JPEG")
    println(r.parse(file.underlyingBytes))
  }


}
