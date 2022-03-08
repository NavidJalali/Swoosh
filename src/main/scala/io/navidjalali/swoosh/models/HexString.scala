package io.navidjalali.swoosh.models

import io.navidjalali.swoosh.syntax.BytesSyntax._

import java.nio.charset.Charset
import java.util.Base64
import scala.util.Try

final case class HexString private(underlyingBytes: Array[Byte], underlyingString: String) {
  def toBytes: Array[Byte] = underlyingBytes

  def +(other: HexString): HexString =
    HexString(underlyingBytes ++ other.underlyingBytes)

  def +(other: Array[Byte]): HexString =
    HexString(underlyingBytes ++ other)

  override def toString: String = underlyingString

  def show(charset: Charset) = new String(underlyingBytes, charset)

  def toBase64: String = Base64.getEncoder.encodeToString(underlyingBytes)
}

object HexString {
  private def toByteArray(hexString: String): Option[Array[Byte]] =
    Try {
      hexString
        .sliding(2, 2)
        .toArray
        .map(Integer.parseInt(_, 16).toByte)
    }.toOption

  def apply(bytes: Array[Byte]): HexString =
    HexString(bytes, bytes.toIndexedSeq.toHexString)

  def fromString(string: String): Option[HexString] =
    toByteArray(string.filterNot(commonSeparators.contains).toUpperCase).map(bytes => HexString(bytes, string))

  def fromBase64(string: String): Option[HexString] =
    Try(Base64.getDecoder.decode(string)).toOption.map(HexString(_))

  val hexSet: Set[Char] = ('A' to 'F').toSet ++ ('0' to '9').toSet

  private val commonSeparators = Set(' ', '-')
}
