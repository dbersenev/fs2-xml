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
import org.dbersenev.fs2.xml.parsing.selector.SelectorImplicits.StringToPathExt

object Selector {
  def apply(path: String): Selector = Selector(path.toSelPath)
}

/**
  * Allows selection of XML parts
  * @param path - path to repeated elements, root is included
  * @param props - configure behavior of selection
  * @param ns - XML namespace
  */
case class Selector(
                     path: NonEmptyVector[SelectorElement],
                     props: Set[SelectorOption] = Set.empty,
                     ns: Option[String] = None
                   ) {

  lazy val hasAttributeSelectors: Boolean = path.exists(_.attrs.nonEmpty)

  def excludeLast: Selector = this.copy(props = props + ExcludeLastSelectorElement)
}
