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


import scala.language.implicitConversions
import cats._
import cats.implicits._
import cats.data._
import org.dbersenev.fs2.xml.events.XMLAttribute


object XMLSelectorIntAttr {
  def apply(name:String, value:Int, ns:Option[String] = None):XMLAttribute = XMLAttribute(name, value.toString.some, ns)
}

case class ExtractedAttr(name: String, value: String, ns: Option[String])

case class XMLSelectorElement(
                               entry: String,
                               stopOnAdjacent: Boolean = true, //do not process adjacent elements
                               ns: Option[String] = None,
                               attrs: List[XMLAttribute] = List.empty
                             ) {

  lazy val compiledAttrs: Map[String, XMLAttribute] = attrs.map(a => (a.name, a)).toMap

  def allowingAdjacent: XMLSelectorElement = this.copy(stopOnAdjacent = false)

  def withNs(namespace: String): XMLSelectorElement = this.copy(ns = Some(namespace))

  def withAttr(a:XMLAttribute): XMLSelectorElement = this.copy(attrs = a :: attrs)

  def attributesMatches(other: Set[ExtractedAttr]): Boolean = other.forall(extAttr => compiledAttrs.get(extAttr.name)
    .forall(_.value.exists(_ == extAttr.value))
  )
}

object options {

  sealed trait SelOpt

  case object ExcludeLastSelectorElement extends SelOpt //exclude last element from the selector path
  case class CloseElementWith(path: NonEmptyVector[XMLSelectorElement]) extends SelOpt

  case class StopBeforeSelector(path: NonEmptyVector[XMLSelectorElement]) extends SelOpt //stop before provided selector match
}

case class XMLSelectorPathBuilder(path: NonEmptyVector[XMLSelectorElement]) {
  def |\!|(name: String): XMLSelectorPathBuilder = generic(name, identity)

  def |\!|(name: String, mod: XMLSelectorElement => XMLSelectorElement): XMLSelectorPathBuilder =
    generic(name, mod)

  def |\!|(el: XMLSelectorElement): XMLSelectorPathBuilder = XMLSelectorPathBuilder(path.append(el))

  def |\|(name: String): XMLSelectorPathBuilder = generic(name, _.allowingAdjacent)

  def |\|(name: String, mod: XMLSelectorElement => XMLSelectorElement): XMLSelectorPathBuilder =
    generic(name, mod.andThen(_.allowingAdjacent))

  def |\|(el: XMLSelectorElement): XMLSelectorPathBuilder = XMLSelectorPathBuilder(path.append(el.allowingAdjacent))

  private def generic(name: String, mod: XMLSelectorElement => XMLSelectorElement) =
    XMLSelectorPathBuilder(path.append(mod(XMLSelectorElement(name))))

  def selector: XMLSelector = XMLSelector(path)
}

object XMLSelector {

  implicit class AnyStrToXmlSelEl(val s: String) extends AnyVal {

    def toSelElem: XMLSelectorElement = XMLSelectorElement(s)

    def toSelPath: NonEmptyVector[XMLSelectorElement] = NonEmptyVector.fromVector(
      s.split("/")
        .map(_.trim)
        .filter(_.nonEmpty)
        .zipWithIndex
        .map {
          case (el, 0) => XMLSelectorElement(el)
          case (el, _) => XMLSelectorElement(el, stopOnAdjacent = false)
        }
        .toVector
    ).get

    def toSelBuilder: XMLSelectorPathBuilder = XMLSelectorPathBuilder(s.toSelPath)

    def toSelector: XMLSelector = XMLSelector(s.toSelBuilder.path)
  }

  implicit class XmlSelectorAttributeExt(val a:XMLAttribute) extends AnyVal {
    def withIntValue(v:Int):XMLAttribute = a.copy(value = v.toString.some)
    def app:XMLSelectorElement => XMLSelectorElement = _.withAttr(a)
  }

  def root(name: String, stopAdj: Boolean = true): XMLSelectorPathBuilder = XMLSelectorPathBuilder(NonEmptyVector.one(XMLSelectorElement(name, stopOnAdjacent = stopAdj)))

}

case class XMLSelector(
                        path: NonEmptyVector[XMLSelectorElement],
                        props: Set[options.SelOpt] = Set.empty,
                        ns: Option[String] = None
                      ) {
  val compiledStringEls: Vector[String] = path.map(_.entry).toVector
  val compiledStringStops: Set[Vector[String]] = props.collect {
    case options.StopBeforeSelector(p) => p.toVector.map(_.entry)
  }

  lazy val hasAttributeSels: Boolean = path.exists(_.attrs.nonEmpty)

  def isPrefix(l: Vector[String], lastAttrs: Set[ExtractedAttr]): Boolean =
    (l.length <= path.length && compiledStringEls.startsWith(l)) && (lastAttrs.isEmpty || matchedSel(l).exists(_.attributesMatches(lastAttrs)))

  private def matchedSel(l: Vector[String]): Option[XMLSelectorElement] = path.toVector.drop(l.size - 1).headOption

  def excludeLast: XMLSelector = this.copy(props = props + options.ExcludeLastSelectorElement)

  def isInStops(l: Vector[String]): Boolean = compiledStringStops.contains(l)

}
