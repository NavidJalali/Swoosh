package io.navidjalali.swoosh.syntax

import java.util.Base64

object BytesSyntax {

  def toHex(byte: Byte): String = String.format("%02x", Byte.box(byte)).toUpperCase

  def buildWith(f: StringBuilder => Any): String = {
    val builder = new StringBuilder
    f(builder)
    builder.toString
  }

  implicit class ByteSequenceOps(private val bytes: Seq[Byte]) {
    def toHexString: String =
      buildWith(builder => bytes.foreach {
        toHex _ andThen builder.append
      })

    def toBase64: String = Base64.getEncoder.encodeToString(bytes.toArray)
  }

  implicit class ByteArrayOps(private val bytes: Array[Byte]) {
    def toHexString: String =
      buildWith(builder => bytes.foreach {
        toHex _ andThen builder.append
      })

    def toBase64: String = Base64.getEncoder.encodeToString(bytes)
  }
}
