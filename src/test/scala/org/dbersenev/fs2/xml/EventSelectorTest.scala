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

package org.dbersenev.fs2.xml

import cats.effect._
import javax.xml.stream.events.XMLEvent
import org.dbersenev.fs2.xml.events.EventUtils._
import org.dbersenev.fs2.xml.parsing.SelectedEventStream
import org.scalatest.{FunSuite, Matchers}
import org.dbersenev.fs2.xml.parsing.XMLSelector._

class EventSelectorTest extends FunSuite with Matchers {

  val rootSelector = root("root").selector
  val SingleElement = "<root></root>"
  def rootSelectorSelection(txt:String):IO[List[XMLEvent]] =
    StreamFunctions.eventStream(txt)
      .through(SelectedEventStream(rootSelector)).compile.toList

  test("Single root xml selection") {

    val seq = rootSelectorSelection(SingleElement).unsafeRunSync()

    seq should contain inOrder (startElementEvent("root"), endElementEvent("root"))
  }

  val SingleElementText = "<root>text</root>"

  test("Single root xml selection with text") {

    val seq = rootSelectorSelection(SingleElementText).unsafeRunSync()

    seq should contain inOrder (startElementEvent("root"), charactersEvent("text"), endElementEvent("root"))
  }

  val TwoElementText = "<root><item>text</item></root>"

  test("Two element inner element selection") {
    val seq = StreamFunctions.eventStream(TwoElementText)
      .through(SelectedEventStream(root("root") |\!| "item" selector)).compile.toList.unsafeRunSync()

    seq should contain inOrder(startElementEvent("item"), charactersEvent("text"), endElementEvent("item"))
  }

  val TwoSameElementsText = "<root><item>text1</item><item>text2</item></root>"

  test("Two same elements inner elements selection") {
    val seq = StreamFunctions.eventStream(TwoSameElementsText)
      .through(SelectedEventStream(root("root") |\| "item" selector)).compile.toList.unsafeRunSync()

    seq should contain theSameElementsInOrderAs  List(
      startElementEvent("item"), charactersEvent("text1"), endElementEvent("item"),
      startElementEvent("item"), charactersEvent("text2"), endElementEvent("item")
    )
  }

  test("Two same elements inner single element selection") {
    val seq = StreamFunctions.eventStream(TwoSameElementsText)
      .through(SelectedEventStream(root("root") |\!| "item" selector)).compile.toList.unsafeRunSync()

    seq should contain theSameElementsInOrderAs  List(
      startElementEvent("item"), charactersEvent("text1"), endElementEvent("item")
    )
  }

}
