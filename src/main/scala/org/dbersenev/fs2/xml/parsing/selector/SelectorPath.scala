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

import cats.data.NonEmptyVector

import scala.language.implicitConversions

case class SelectorPathBuilder(path: NonEmptyVector[SelectorElement]) {
  def |\!|(el: SelectorElement): SelectorPathBuilder = SelectorPathBuilder(path.append(el))

  def |\|(el: SelectorElement): SelectorPathBuilder = SelectorPathBuilder(path.append(el.allowingAdjacent))
}

object SelectorPath {

  def root(name: String, stopAdj: Boolean = true): SelectorPathBuilder = SelectorPathBuilder(NonEmptyVector.one(SelectorElement(name, stopOnAdjacent = stopAdj)))

  implicit class StringToPathExt(val path:String) extends AnyVal {
    def toSelPath: NonEmptyVector[SelectorElement] = NonEmptyVector.fromVector(
      path.split("/")
        .map(_.trim)
        .filter(_.nonEmpty)
        .zipWithIndex
        .map {
          case (el, 0) => SelectorElement(el)
          case (el, _) => SelectorElement(el, stopOnAdjacent = false)
        }
        .toVector
    ).get
  }

  implicit def builderToVecExtr(builder:SelectorPathBuilder):NonEmptyVector[SelectorElement] = builder.path


}