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

sealed trait SelectorOption

case object ExcludeLastSelectorElement extends SelectorOption //exclude last element from the selector path
case class CloseElementWith(path: NonEmptyVector[SelectorElement]) extends SelectorOption

case class StopBeforeSelector(path: NonEmptyVector[SelectorElement]) extends SelectorOption //stop before provided selector match
