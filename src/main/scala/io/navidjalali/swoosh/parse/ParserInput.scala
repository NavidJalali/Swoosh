package io.navidjalali.swoosh.parse

import akka.stream.scaladsl.Source
import akka.util.ByteString

trait ParserInput[A] {
}

//object ParserInput {
//  val src: Source[ByteString] = ???
//
//  val f:
//
//  val t = src
//    .flatMapConcat(bs => Source(bs.toIndexedSeq))
//    .via(
//
//    )
//}