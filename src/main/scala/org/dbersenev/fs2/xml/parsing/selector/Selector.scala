/*
 * Copyright [2020] [Dmitry Bersenev]
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

package org.dbersenev.fs2.xml.parsing.selector

import cats.data._
import cats.implicits._
import org.dbersenev.fs2.xml.parsing.selector.SelectorPath.StringToPathExt

import scala.language.implicitConversions

object Selector {
  case class ExtractedAttr(name: String, value: String, ns: Option[String])

  implicit class PathBuilderToSelector(val pb:SelectorPathBuilder) extends AnyVal {
    def toSelector:Selector = Selector(pb.path)
  }

  implicit def pathBuilderToSelector(pb:SelectorPathBuilder):Selector = Selector(pb.path)

  implicit def vecPathToSelector(path:NonEmptyVector[SelectorElement]):Selector = Selector(path)

  def apply(path:String):Selector = Selector(path.toSelPath)
}

import org.dbersenev.fs2.xml.parsing.selector.Selector._

case class Selector(
                     path: NonEmptyVector[SelectorElement],
                     props: Set[SelectorOption] = Set.empty,
                     ns: Option[String] = None
                      ) {
  val compiledStringEls: Vector[String] = path.map(_.entry).toVector
  val compiledStringStops: Set[Vector[String]] = props.collect {
    case StopBeforeSelector(p) => p.toVector.map(_.entry)
  }

  lazy val hasAttributeSels: Boolean = path.exists(_.attrs.nonEmpty)

  def isPrefix(l: Vector[String], lastAttrs: Set[ExtractedAttr]): Boolean =
    (l.length <= path.length && compiledStringEls.startsWith(l)) && (lastAttrs.isEmpty || matchedSel(l).exists(_.attributesMatches(lastAttrs)))

  private def matchedSel(l: Vector[String]): Option[SelectorElement] = path.toVector.drop(l.size - 1).headOption

  def excludeLast: Selector = this.copy(props = props + ExcludeLastSelectorElement)

  def isInStops(l: Vector[String]): Boolean = compiledStringStops.contains(l)
}
