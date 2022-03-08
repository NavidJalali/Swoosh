package io.navidjalali.swoosh

import io.navidjalali.swoosh.models.HexString
import io.navidjalali.swoosh.parse.Parser
import zio.Console.printLine
import zio.stream.{ZPipeline, ZSink, ZStream}
import zio.{Chunk, ZEnv, ZIO, ZIOAppArgs, ZIOAppDefault, ZManaged}

import scala.util.chaining.scalaUtilChainingOps

object Main extends ZIOAppDefault {

  val separatorParser: Parser[String] = Parser.string("; ")

  val patternParser: Parser[Vector[Either[Int, HexString]]] =
    (for {
      bytes <-
        Parser.anyChar
          .suchThat(HexString.hexSet.contains, "Not a hex char")
          .repeatedly
          .suchThat(_.nonEmpty, "is empty")
      wildcards <- Parser.char('?').repeatedly
    } yield Vector(Right(HexString.fromString(bytes.mkString).get), Left(wildcards.length))).repeatedly.map(_.flatten)

  val parser = for {
    name <- Parser.anyChar.suchThat(_ != ';', "end").repeatedly.map(_.mkString)
    _ <- separatorParser
    pattern <- patternParser
    _ <- separatorParser
    offset <- Parser.anyLong
    _ <- separatorParser
    _ <- Parser.char('[')
    ext <- Parser.anyChar.suchThat(_ != ']', "encountered ]").repeatedly.map(_.mkString.split(',').toList)
    _ <- Parser.char(']')
    _ <- separatorParser
    desc <- Parser.anyChar.repeatedly.map(_.mkString)
  } yield (name, pattern, offset, ext, desc)

  def createPattern(pattern: Vector[Either[Int, HexString]], offset: Long): String = {
    val offsetParser = if (offset == 0) Nil else List(s"Parser.anyByte.repeat(${offset})")
    val patternParsers = pattern.flatMap {
      case Left(value) =>
        if (value == 0) Nil
        else List(s"Parser.anyByte.repeat(${value / 2})".ensuring(value % 2 == 0))
      case Right(hs) => List(s"Parser.sequence(${hs.underlyingBytes.mkString("Array(", ", ", ")")}.map(_.toByte)).unit()")
    }
    offsetParser ++ patternParsers match {
      case head :: Nil => "    " + head
      case Nil => ""
      case list =>
        s"""    for {
           |${list.map("      _ <- " + _).mkString("\n")}
           |    } yield ()
           |""".stripMargin
    }
  }

  def co(name: String, description: String, pattern: Vector[Either[Int, HexString]], offset: Long, ext: List[String], mimes: Set[String]) =
    s"""
       |/**
       |* $description
       |*/
       |case object $name extends FileType {
       |  override val parser =
       |${createPattern(pattern, offset)}
       |
       |  override val extensions = ${if (ext.isEmpty) "Set.empty" else ext.mkString("Set(", ", ", ")")}
       |
       |  override val priority = ${offset + pattern.map(_.fold(i => i /2, _.underlyingBytes.length)).sum}
       |
       |  override val mimeTypes = ${if (mimes.isEmpty) "Set.empty" else mimes.mkString("Set(", ", ", ")")}
       |}
       |""".stripMargin

  val mimes =
    ZStream.fromFileString("mimes")
      .via(ZPipeline.utfDecode)
      .via(ZPipeline.splitLines)
      .filter(s => !s.startsWith("#"))
      .map(_.split('\t').filter(_.nonEmpty).pipe(arr => arr(1).split(' ').map(_ -> arr.head).toMap))
      .runFold(Map.empty[String, String])(_ ++ _)
      //.tap(printLine(_))

  val app =
    ZManaged.fromZIO(mimes).flatMap {
      mt =>
        ZStream.fromFileString("base")
          .via(ZPipeline.utfDecode)
          .via(ZPipeline.splitLines)
          .map(str =>
            parser.parse(str.getBytes) match {
              case Left(value) => None -> s"// COULD NOT GENERATE[$value] ${str} \n"
              case Right((name, pt, ofs, ext, dsc)) => Some(name) -> co(name, dsc, pt, ofs, ext.filter(_.nonEmpty).map(str => s""""$str""""), ext.flatMap(mt.get).toSet.filter(_.nonEmpty).map(str => s""""$str""""))
            }
          )
          .broadcast(2, 1)
          .flatMap {
            streams =>
              ZManaged.fromZIO(
                ZIO.collectAllPar(
                  Chunk (
                    streams(0).flatMap { case (_, s) => ZStream.fromChunk(Chunk.fromArray(s.getBytes)) }.run(ZSink.fromFileString("files.scala")),
                    streams(1).map(_._1).runFold(Set.empty[String]) {
                      (acc, next) => next.fold(acc)(acc + _)
                    }.tap(printLine(_))
                  )
                )
              )
          }
    }.useNow


  override def run: ZIO[ZEnv with ZIOAppArgs, Any, Any] =
    ZIO.succeed(FileType.all.toList.sortBy(_.priority).reverse).tap(printLine(_)).exitCode
    //app.catchAllCause(printLine(_)).exitCode

}
