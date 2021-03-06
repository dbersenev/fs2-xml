/*
 * Copyright [2019] [Dmitry Bersenev]
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.dbersenev.fs2.xml.parsing

import java.io.InputStream

import cats.effect._
import fs2._
import javax.xml.stream.{XMLEventReader, XMLInputFactory}
import javax.xml.stream.events.XMLEvent

import scala.io.Codec


object EventStream {

  val inf: XMLInputFactory = XMLInputFactory.newInstance()
  inf.setProperty(XMLInputFactory.IS_COALESCING, true)

  class ParsingException extends Throwable

  private def evReader[F[_] : Sync](ins: InputStream, enc: Option[Codec]): F[XMLEventReader] = {
    enc.map(e => Sync[F].delay(inf.createXMLEventReader(ins, e.name))).getOrElse(Sync[F].delay(inf.createXMLEventReader(ins)))
  }

  /**
    * Produces XML events from stream of bytes
    * Because function uses "InputStream" to integrate with XMLEventReader
    * one should select dedicated Blocker independent from the one used to get stream of bytes.
    * Otherwise "InputStream" will block
    *
    * @param blocker
    * @param enc
    * @param s
    * @tparam F
    * @return
    */
  def apply[F[_] : ContextShift : ConcurrentEffect](blocker: Blocker, enc: Option[Codec] = None)(s: Stream[F, Byte]): Stream[F, XMLEvent] = {

    import scala.jdk.CollectionConverters._

    def go(er: XMLEventReader): Pull[F, XMLEvent, Unit] =
      Pull.eval(
        blocker.delay {
          val hasN = er.hasNext
          if (hasN) {
            Some(er.nextEvent())
          } else {
            None
          }
        }
      ).flatMap(evO => evO.map(ev => Pull.output1(ev) >> go(er)).getOrElse(Pull.done)
      )

    //different approaches to check input stream for emptiness
    s.pull.uncons1.flatMap {
      case Some((b, tail)) =>
        (Stream.emit(b) ++ tail).through(io.toInputStream).flatMap(ins =>
          Stream.bracket(blocker.blockOn(evReader(ins, enc)))(r =>
            blocker.delay(r.close())
          ).flatMap(r => Stream.fromBlockingIterator.apply(blocker, r.asInstanceOf[java.util.Iterator[XMLEvent]].asScala))
        ).pull.echo
      case _ => Pull.done
    }.stream
  /*  s.head.flatMap(_ =>
      s.through(io.toInputStream).flatMap(ins =>
        Stream.bracket(blocker.blockOn(evReader(ins, enc)))(r =>
          blocker.delay(r.close())
        ).flatMap(r => Stream.fromBlockingIterator.apply(blocker, r.asInstanceOf[java.util.Iterator[XMLEvent]].asScala))
      )
    )*/
  }

}
