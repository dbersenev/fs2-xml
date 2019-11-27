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


case class XMLSelectorAttr(name: String, value: Option[String] = None, ns: Option[String] = None)

case class ExtractedAttr(name: String, value: String, ns: Option[String])

case class XMLSelectorElement(
                               entry: String,
                               stopOnAdjacent: Boolean = true, //do not process adjacent elements
                               ns: Option[String] = None,
                               attrs:Seq[XMLSelectorAttr] = Seq.empty
                             ) {

  lazy val compiledAttrs:Map[String,XMLSelectorAttr] = attrs.map(a => (a.name, a)).toMap

  def allowingAdjacent: XMLSelectorElement = this.copy(stopOnAdjacent = false)

  def withNs(namespace: String): XMLSelectorElement = this.copy(ns = Some(namespace))

  def withAttr(name:String, value:Option[String] = None, ns:Option[String] = None): XMLSelectorElement = this.copy(attrs = this.attrs :+ XMLSelectorAttr(name, value, ns))

  def withIntAttr(name:String, value:Int, ns:Option[String] = None):XMLSelectorElement =
    withAttr(name, value.toString.some, ns)

  def attributesMatches(other:Set[ExtractedAttr]):Boolean = other.forall(extAttr => compiledAttrs.get(extAttr.name)
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
  def |\!|(name: String): XMLSelectorPathBuilder = XMLSelectorPathBuilder(path.append(XMLSelectorElement(name)))
  def |\!|(el: XMLSelectorElement): XMLSelectorPathBuilder = XMLSelectorPathBuilder(path.append(el))

  def |\|(name: String): XMLSelectorPathBuilder = XMLSelectorPathBuilder(path.append(XMLSelectorElement(name, stopOnAdjacent = false)))
  def |\|(el: XMLSelectorElement): XMLSelectorPathBuilder = XMLSelectorPathBuilder(path.append(el.allowingAdjacent))
}

object XMLSelector {

  implicit class AnyStrToXmlSelEl(val s: String) extends AnyVal {
    def sel: XMLSelectorElement = XMLSelectorElement(s)
    def selPath:NonEmptyVector[XMLSelectorElement] = NonEmptyVector.fromVector(s.split("/").map(_.trim).filter(_.nonEmpty).map(XMLSelectorElement(_, stopOnAdjacent = false)).toVector).get
    def selBuilder:XMLSelectorPathBuilder = XMLSelectorPathBuilder(s.selPath)
    def selector:XMLSelector = XMLSelector(s.selBuilder)
  }


  implicit def anyStrToSelectorElement(s:String):XMLSelectorElement = s.sel

  implicit def selPathToXmlPathBuilder(path:NonEmptyVector[XMLSelectorElement]):XMLSelectorPathBuilder = XMLSelectorPathBuilder(path)

  implicit def xmlPathBuilderToVect(bld: XMLSelectorPathBuilder): NonEmptyVector[XMLSelectorElement] = bld.path
  implicit def xmlPathBuilderToSelector(bld: XMLSelectorPathBuilder): XMLSelector = XMLSelector(bld.path)

  def root(name: String, stopAdj:Boolean = true): XMLSelectorPathBuilder = XMLSelectorPathBuilder(NonEmptyVector.one(XMLSelectorElement(name, stopOnAdjacent = stopAdj)))

  implicit def anyStrToXmlPathBuilder(s:String):XMLSelectorPathBuilder = root(s)

}

case class XMLSelector(
                        path: NonEmptyVector[XMLSelectorElement],
                        props: Set[options.SelOpt] = Set.empty,
                        ns: Option[String] = None
                      ) {
  val compiledStringEls: Vector[String] = path.map(_.entry).toVector
  val compiledStringStops:Set[Vector[String]] = props.collect{
    case options.StopBeforeSelector(p) => p.toVector.map(_.entry)
  }

  lazy val hasAttributeSels:Boolean = path.exists(_.attrs.nonEmpty)

  def isPrefix(l: Vector[String], lastAttrs:Set[ExtractedAttr]): Boolean =
    (l.length <= path.length && compiledStringEls.startsWith(l)) && (lastAttrs.isEmpty || matchedSel(l).exists(_.attributesMatches(lastAttrs)))

  private def matchedSel(l: Vector[String]):Option[XMLSelectorElement] = path.toVector.drop(l.size - 1).headOption

  def excludeLast: XMLSelector = this.copy(props = props + options.ExcludeLastSelectorElement)

  def isInStops(l:Vector[String]):Boolean = compiledStringStops.contains(l)

}
