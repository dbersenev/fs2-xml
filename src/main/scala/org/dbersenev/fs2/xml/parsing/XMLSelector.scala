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


case class XMLSelectorAttr(name: String, value: Option[String], ns: Option[String])

case class XMLSelectorElement(entry: String, onlyFollowed: Boolean = true, ns: Option[String] = None, onlyFirst:Boolean = false) {
  def allowingOthers: XMLSelectorElement = this.copy(onlyFollowed = false)

  def withNs(namespace: String): XMLSelectorElement = this.copy(ns = Some(namespace))

  def first: XMLSelectorElement = this.copy(onlyFirst = true)
}

object options {
  sealed trait SelOpt
  case object ExcludeLast extends SelOpt
  case class Close(path: NonEmptyVector[XMLSelectorElement]) extends SelOpt
  case class StopBefore(path: NonEmptyVector[XMLSelectorElement]) extends SelOpt
}

case class XMLSelectorPathBuilder(path: NonEmptyVector[XMLSelectorElement]) {
  def |\|(name: String): XMLSelectorPathBuilder = XMLSelectorPathBuilder(path.append(XMLSelectorElement(name)))
  def |\|(el: XMLSelectorElement): XMLSelectorPathBuilder = XMLSelectorPathBuilder(path.append(el))

  def |\\|(name: String): XMLSelectorPathBuilder = XMLSelectorPathBuilder(path.append(XMLSelectorElement(name, onlyFollowed = false)))
}

object XMLSelector {

  implicit class AnyStrToXmlSelEl(val s: String) extends AnyVal {
    def sel: XMLSelectorElement = XMLSelectorElement(s)
    def selPath:NonEmptyVector[XMLSelectorElement] = NonEmptyVector.fromVector(s.split("/").map(_.trim).filter(_.nonEmpty).map(XMLSelectorElement(_)).toVector).get
  }

  implicit def xmlPathBuilderToVect(bld: XMLSelectorPathBuilder): NonEmptyVector[XMLSelectorElement] = bld.path

  def root(name: String): XMLSelectorPathBuilder = XMLSelectorPathBuilder(NonEmptyVector.one(XMLSelectorElement(name)))

}

case class XMLSelector(path: NonEmptyVector[XMLSelectorElement], props: Set[options.SelOpt] = Set.empty, ns: Option[String] = None) {
  val compiledStringEls: Vector[String] = path.map(_.entry).toVector
  val compiledStringStops:Set[Vector[String]] = props.collect{
    case options.StopBefore(p) => p.toVector.map(_.entry)
  }

  def isPrefix(l: Vector[String]): Boolean = l.length <= path.length && compiledStringEls.startsWith(l)
  def excludeLast: XMLSelector = this.copy(props = props + options.ExcludeLast)

  def isInStops(l:Vector[String]):Boolean = compiledStringStops.contains(l)

}
