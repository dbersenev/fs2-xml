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

import scala.language.higherKinds

import cats._
import cats.implicits._
import fs2._
import javax.xml.stream.events.{StartElement, XMLEvent}

import javax.xml.stream.events.{StartElement, XMLEvent, Attribute => JAttr}
import scala.xml._

object ElementStream {
  import scala.collection.JavaConverters._
  import java.util.{Iterator => JIter}

  case class ElemWrap(elem: Elem, isClosed: Boolean)

  def jStartToScala(startElement: StartElement): Elem = {
    val attrs = startElement.getAttributes.asInstanceOf[JIter[JAttr]].asScala.toList
      .map(a => Attribute(None, a.getName.getLocalPart, Text(a.getValue), Null).asInstanceOf[MetaData])
      .reduceOption((a1, a2) => MetaData.concatenate(a1, a2)).getOrElse(Null)
    Elem(null, startElement.getName.getLocalPart, attrs, TopScope, true)
  }

  def apply[F[_]](stream: Stream[F, XMLEvent]): Stream[F, Elem] = {

    def process(acc: Vector[ElemWrap], l: Stream[F, XMLEvent]): Pull[F, Elem, Unit] = {

      /**
        * index and last incomplete element
        *
        * @return
        */
      def openIndex: Option[(Int, ElemWrap)] = {
        val ind = acc.lastIndexWhere(!_.isClosed)
        if (ind != -1) {
          Some(ind -> acc(ind))
        } else {
          None
        }
      }

      /**
        * insert node into incomplete element
        *
        * @param chld
        * @return
        */
      def insertAtOpen(chld: Node): Vector[ElemWrap] = {
        openIndex.map(oi => {
          val spl = acc.splitAt(oi._1) //last part contains incomplete element
          //copy element with node appended
          val newEl = oi._2.copy(elem = oi._2.elem.copy(child = oi._2.elem.child :+ chld))
          //union lists
          (spl._1 :+ newEl) ++ spl._2.drop(1)
        }).getOrElse(acc)
      }

      l.pull.uncons1.flatMap(v => v match {
        case Some((ev, s)) =>
          if (ev.isStartElement) {
            val el = ElemWrap(jStartToScala(ev.asStartElement()), false)
            process(acc :+ el, s)
          } else if (ev.isCharacters) {
            val chEv = ev.asCharacters()
            val what = if (chEv.isCData) PCData(ev.asCharacters().getData) else Text(ev.asCharacters().getData)
            process(insertAtOpen(what), s)
          } else if (ev.isEndElement) {
            val endEl = ev.asEndElement
            val newAcc: Vector[ElemWrap] = openIndex.map(oi => {
              //check if we have correct closing tag
              if (oi._2.elem.label === endEl.getName.getLocalPart) {
                val spl = acc.splitAt(oi._1)
                //copy all elements before last incomplete element (from the end) to that element
                spl._1 :+ oi._2.copy(elem = oi._2.elem.copy(child = oi._2.elem.child ++ spl._2.drop(1).map(_.elem)), isClosed = true)
              } else {
                acc
              }
            }).getOrElse(acc)
            //if only one element is left in accumulator output it and wait for other elements
            if (newAcc.size == 1 && newAcc.head.isClosed) Pull.output1(newAcc.head.elem) >> process(Vector.empty, s)
            else process(newAcc, s)
          } else {
            process(acc, s)
          }
        case _ => Pull.done
      }
      )
    }

    process(Vector.empty, stream).stream
  }
}
