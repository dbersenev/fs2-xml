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
import cats.effect._
import javax.xml.stream.events.{StartElement, XMLEvent}

object SelectedEventStream {

  import EventStream._
  import XMLSelector._

  private case class SelectionConfig(
                              isWithin:Boolean = false,
                              lastMainPop:Option[XMLSelectorElement] = None,
                              ignoreInnerEvents:Boolean = false
                            )

  def apply[F[_]:RaiseThrowable](selector:String)(stream:Stream[F,XMLEvent]):Stream[F,XMLEvent] =
    apply(XMLSelector(selector.selPath))(stream)

  def apply[F[_] : RaiseThrowable](selector: XMLSelector)(stream: Stream[F, XMLEvent]): Stream[F, XMLEvent] = {
    //simple path with full inclusion

    val exclLast = selector.props.contains(options.ExcludeLast)

    def filterEvents(acc: Vector[String], srcS: Stream[F, XMLEvent], extraEls: Vector[StartElement], cfg:SelectionConfig): Pull[F, XMLEvent, Unit] = {

      srcS.pull.uncons1.flatMap {
        case Some((ev, tails)) =>

          //shortcut functions
          def callFound(acc1: Vector[String]):Pull[F, XMLEvent, Unit] = filterEvents(acc1, tails, Vector.empty, SelectionConfig(isWithin = true))

          if (cfg.isWithin) { //if within searched path
            if (ev.isEndElement) {
              if (extraEls.isEmpty) { //element is closing but nothing yet processed within
                //check if selected path is closing
                if (acc.lastOption.exists(el => el === ev.asEndElement().getName.getLocalPart)) {
                  val newAcc = acc.dropRight(1) //removed last element from current path
                  val endEl = Some(selector.path.toVector.last) //last selector path element
                  //output last element if not excluded
                  val tailF = if(endEl.get.onlyFirst) () => {
                    println("\ndone first only within")
                    println(ev)
                    Pull.done
                  } else () => filterEvents(newAcc, tails, Vector.empty, SelectionConfig(lastMainPop = endEl))
                  if (!exclLast) Pull.output1(ev) >> tailF()
                  else tailF()
                } else {
                  Pull.raiseError(new ParsingException)
                }
              } else {
                //if within stack not empty
                if (extraEls.lastOption.exists(el => el.getName.equals(ev.asEndElement().getName))) {
                  //just output and pop
                  Pull.output1(ev) >> filterEvents(acc, tails, extraEls.dropRight(1), SelectionConfig(cfg.isWithin))
                } else {
                  Pull.raiseError(new ParsingException)
                }
              }
            } else {
              //if not end element
              //stack is appended in case of start element
              val newWithin = if (ev.isStartElement) extraEls :+ ev.asStartElement() else extraEls
              Pull.output1(ev) >> filterEvents(acc, tails, newWithin, SelectionConfig(isWithin = cfg.isWithin))
            }
          } else {
            //selection part
            if (ev.isStartElement) {
              //if not matched tail is present and not ignoring events
              //events are ignored when last element of selector disallows neighbours after it
              //flag is set afte rsuch element is closed
              if (extraEls.isEmpty && !cfg.ignoreInnerEvents) {
                //tentative path
                val newAcc = acc :+ ev.asStartElement().getName.getLocalPart
                //if stop paths exist
                if(selector.isInStops(newAcc)) {
                  println("done stop")
                  println(ev)
                  Pull.done
                } else if (selector.isPrefix(newAcc)) {
                  //if path is matched or partially matched
                  if (newAcc.length == selector.path.length) {
                    //if path is matched
                    //in case of last element exclusion do not produce it in the output
                    if (!exclLast) Pull.output1(ev) >> callFound(newAcc) else callFound(newAcc)
                  } else {
                    filterEvents(newAcc, tails, extraEls, SelectionConfig())
                  }
                } else {
                  //if nothing is matches or events are ignored
                  val isLastSelectorClosed = cfg.lastMainPop.contains(selector.path.last) //if previously last selector element was closed
                  val autoStop = cfg.lastMainPop.exists(lel => lel.onlyFollowed) //if previously popped element (from acc) disallow following neighbours

                  //finish if following not in last element
                  if (autoStop && !isLastSelectorClosed) {
                    println("\ndone1")
                    println(ev)
                    Pull.done
                  }
                  else filterEvents(acc, tails, extraEls :+ ev.asStartElement(), SelectionConfig(ignoreInnerEvents = autoStop && isLastSelectorClosed)) //keep building filter stack
                }
              } else {
                filterEvents(acc, tails, extraEls :+ ev.asStartElement(), SelectionConfig(ignoreInnerEvents = cfg.ignoreInnerEvents))
              }
            } else if (ev.isEndElement) {
              //popping selection stack

              val endEl = ev.asEndElement()

              //if not matched tail contains some elements
              if (extraEls.nonEmpty) {
                //correctly popping not matched elements
                if (extraEls.last.getName == endEl.getName) {
                  filterEvents(acc, tails, extraEls.dropRight(1), SelectionConfig(ignoreInnerEvents = cfg.ignoreInnerEvents))
                } else {
                  Pull.raiseError(new ParsingException)
                }
              } else {
                //if popping from matched path
                if (acc.lastOption.exists(el => el === endEl.getName.getLocalPart)) {
                  /* if(lastMainPop.exists(_.autoStop)) {
                     println("\ndone2")
                     println(ev)
                     Pull.done
                   }*/
                  //pop and remember popped selector
                  val selLast = selector.path.toVector.drop(acc.length - 1).head
                  if(selLast.onlyFirst) {
                    println("\nfirst only")
                    println(ev)
                    Pull.done
                  }
                  else filterEvents(acc.dropRight(1), tails, Vector.empty, SelectionConfig(lastMainPop = Some(selLast)))
                } else {
                  Pull.raiseError(new ParsingException)
                }
              }
            } else {
              //ignoring events
              filterEvents(acc, tails, extraEls, cfg)
            }
          }
        case _ => Pull.done
      }
    }

    filterEvents(Vector.empty, stream, Vector.empty, SelectionConfig()).stream
  }


}
