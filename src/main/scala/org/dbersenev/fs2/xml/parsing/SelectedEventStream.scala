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

import cats.implicits._
import fs2._
import javax.xml.stream.events.{Attribute, StartElement, XMLEvent}
import org.dbersenev.fs2.xml.parsing.selector.{ExcludeLastSelectorElement, ExtractedAttr, Selector, SelectorAttribute, SelectorElement, StopBeforeSelector}

import scala.jdk.CollectionConverters._

object SelectedEventStream {

  import EventStream._
  import Selector._

  private case class SelectionConfig(
                                      isMatched: Boolean = false,
                                      lastMainPop: Option[SelectorElement] = None,
                                      ignoreInnerEvents: Boolean = false
                                    )

  private class PreparedSelectorElement(val selectorElement:SelectorElement) {

    val compiledAttrs: Map[String, SelectorAttribute] = selectorElement.attrs.map(a => (a.name, a)).toMap

    def attributesMatches(other: Set[ExtractedAttr]): Boolean = other.forall(extAttr => compiledAttrs.get(extAttr.name)
      .forall(_.value.exists(_ == extAttr.value))
    )

  }

  private class PreparedSelector(val selector:Selector) {

    val compiledElements:Vector[PreparedSelectorElement] = selector.path.map(new PreparedSelectorElement(_)).toVector

    val compiledAsString: Vector[String] = compiledElements.map(_.selectorElement.entry)

    //TODO implement complex stops (by attributes, etc)
    val compiledStringStops: Set[Vector[String]] = selector.props.collect {
      case StopBeforeSelector(p) => p.toVector.map(_.entry)
    }

    val isLastElementExcluded:Boolean = selector.props.contains(ExcludeLastSelectorElement)
    val isIgnoreAdjacentElements:Boolean = !selector.path.exists(v => !v.stopOnAdjacent)

    def isPrefix(l: Vector[String], lastAttrs: Set[ExtractedAttr]): Boolean =
      (l.length <= selector.path.length && compiledAsString.startsWith(l)) && (lastAttrs.isEmpty || matchedSel(l).exists(_.attributesMatches(lastAttrs)))

    def isInStops(l: Vector[String]): Boolean = compiledStringStops.contains(l)

    private def matchedSel(l: Vector[String]): Option[PreparedSelectorElement] = compiledElements.drop(l.size - 1).headOption

  }

  def apply[F[_] : RaiseThrowable](selectorPath: String)(stream: Stream[F, XMLEvent]): Stream[F, XMLEvent] =
    apply(Selector(selectorPath))(stream)

  def apply[F[_] : RaiseThrowable](selector: Selector)(stream: Stream[F, XMLEvent]): Stream[F, XMLEvent] = {
    //simple path with full inclusion

    val prepared = new PreparedSelector(selector)

    def filterEvents(acc: Vector[String], srcS: Stream[F, XMLEvent], extraEls: Vector[StartElement], cfg: SelectionConfig): Pull[F, XMLEvent, Unit] = {

      srcS.pull.uncons1.flatMap {
        case Some((ev, tails)) =>

          //shortcut functions
          def callFound(acc1: Vector[String]): Pull[F, XMLEvent, Unit] = filterEvents(acc1, tails, Vector.empty, SelectionConfig(isMatched = true))

          if (cfg.isMatched) { //if within searched path
            if (ev.isEndElement) {
              if (extraEls.isEmpty) { //element is closing but nothing yet processed within
                //check if selected path is closing
                if (acc.lastOption.exists(el => el === ev.asEndElement().getName.getLocalPart)) {
                  val newAcc = acc.dropRight(1) //removed last element from current path
                  val endEl = Some(selector.path.toVector.last) //last selector path element
                  //output last element if not excluded
                  val tailF = if (prepared.isIgnoreAdjacentElements) () => Pull.done
                  else () => filterEvents(newAcc, tails, Vector.empty, SelectionConfig(lastMainPop = endEl))
                  if (!prepared.isLastElementExcluded) Pull.output1(ev) >> tailF()
                  else tailF()
                } else {
                  Pull.raiseError(new ParsingException)
                }
              } else {
                //if within stack not empty
                if (extraEls.lastOption.exists(el => el.getName.equals(ev.asEndElement().getName))) {
                  //just output and pop
                  Pull.output1(ev) >> filterEvents(acc, tails, extraEls.dropRight(1), SelectionConfig(cfg.isMatched))
                } else {
                  Pull.raiseError(new ParsingException)
                }
              }
            } else {
              //if not end element
              //stack is appended in case of start element
              val newWithin = if (ev.isStartElement) extraEls :+ ev.asStartElement() else extraEls
              Pull.output1(ev) >> filterEvents(acc, tails, newWithin, SelectionConfig(isMatched = cfg.isMatched))
            }
          } else {
            //selection part
            if (ev.isStartElement) {
              //if not matched tail is present and not ignoring events
              //events are ignored when last element of selector disallows neighbours after it
              //flag is set after such element is closed
              val asStart = ev.asStartElement()
              if (extraEls.isEmpty && !cfg.ignoreInnerEvents) {
                //tentative path
                val newAcc = acc :+ asStart.getName.getLocalPart

                if (prepared.isInStops(newAcc)) {
                  Pull.done
                } else if (
                  prepared.isPrefix(newAcc,
                    if (selector.hasAttributeSelectors) asStart.getAttributes
                      .asInstanceOf[java.util.Iterator[Attribute]].asScala.toSet.map((a: Attribute) => ExtractedAttr(a.getName.getLocalPart, a.getValue, None))
                    else Set.empty
                  )
                ) {
                  if(cfg.lastMainPop.exists(v => selector.path.toVector.drop(newAcc.length - 1).headOption.exists(v2 => v2 == v && v.stopOnAdjacent))){
                    filterEvents(acc, tails, Vector(asStart), cfg.copy(ignoreInnerEvents = true))
                  } else {
                    //if path is matched or partially matched
                    if (newAcc.length == selector.path.length) {
                      //if path is matched
                      //in case of last element exclusion do not produce it in the output
                      if (!prepared.isLastElementExcluded) Pull.output1(ev) >> callFound(newAcc) else callFound(newAcc)
                    } else {
                      filterEvents(newAcc, tails, Vector.empty, SelectionConfig())
                    }
                  }
                } else {
                  filterEvents(acc, tails, extraEls :+ asStart, cfg.copy(ignoreInnerEvents = true))
                } //keep building filter stack

              } else {
                filterEvents(acc, tails, extraEls :+ asStart, cfg)
              }
            } else if (ev.isEndElement) {
              //popping selection stack

              val endEl = ev.asEndElement()

              //if not matched tail contains some elements
              if (extraEls.nonEmpty) {
                //correctly popping not matched elements
                if (extraEls.last.getName == endEl.getName) {
                  val tmpExtra = extraEls.dropRight(1)
                  filterEvents(acc, tails, tmpExtra, cfg.copy(ignoreInnerEvents = tmpExtra.nonEmpty))
                } else {
                  Pull.raiseError(new ParsingException)
                }
              } else {
                //if popping from matched path
                if (acc.lastOption.exists(el => el === endEl.getName.getLocalPart)) {
                  //pop and remember popped selector
                  val (v1, v2) = selector.path.toVector.splitAt(acc.length - 1)
                  val selLast = v2.head
                  if (prepared.isIgnoreAdjacentElements || (selLast.stopOnAdjacent && v1.forall(el => el.stopOnAdjacent))) {
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
