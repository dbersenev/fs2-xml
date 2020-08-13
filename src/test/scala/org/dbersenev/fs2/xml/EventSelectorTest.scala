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
import cats.implicits.catsSyntaxOptionId
import javax.xml.stream.events.{Characters, EndElement, StartElement, XMLEvent}
import EventUtils._
import org.dbersenev.fs2.xml.parsing.SelectedEventStream
import org.dbersenev.fs2.xml.parsing.selector.SelectorPath.root
import org.dbersenev.fs2.xml.parsing.selector._
import org.dbersenev.fs2.xml.parsing.selector.SelectorPath._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class EventSelectorTest extends AnyFunSuite with Matchers {

  val rootSelector = Selector("root")
  val SingleElement = "<root></root>"
  def rootSelectorSelection(txt:String):IO[List[XMLEvent]] =
    StreamFunctions.eventStream(txt)
      .through(SelectedEventStream(rootSelector)).compile.toList

  test("Single root xml selection") {

    val seq = rootSelectorSelection(SingleElement).unsafeRunSync()

    seq.map(eventToExpl) should contain inOrder (startElementEvent("root"), endElementEvent("root"))
  }

  val SingleElementText = "<root>text</root>"

  test("Single root xml selection with text") {

    val seq = rootSelectorSelection(SingleElementText).unsafeRunSync()

    seq.map(eventToExpl) should contain inOrder (startElementEvent("root"), charactersEvent("text"), endElementEvent("root"))
  }

  val TwoElementText = "<root><item>text</item></root>"

  test("Two element inner element selection") {
    val seq = StreamFunctions.eventStream(TwoElementText)
      .through(
        SelectedEventStream(root("root") |\!| "item")
      ).compile.toList.unsafeRunSync()

    seq.map(eventToExpl) should contain inOrder(startElementEvent("item"), charactersEvent("text"), endElementEvent("item"))
  }

  val TwoSameElementsText = "<root><item>text1</item><item>text2</item></root>"

  test("Two same elements inner elements selection") {
    val seq = StreamFunctions.eventStream(TwoSameElementsText)
      .through(
        SelectedEventStream((root("root") |\| "item"))
      ).compile.toList.unsafeRunSync()

    seq.map(eventToExpl) should contain theSameElementsInOrderAs  List(
      startElementEvent("item"), charactersEvent("text1"), endElementEvent("item"),
      startElementEvent("item"), charactersEvent("text2"), endElementEvent("item")
    )
  }

  test("Two same elements inner single element selection") {
    val seq = StreamFunctions.eventStream(TwoSameElementsText)
      .through(
        SelectedEventStream(root("root") |\!| "item")
      ).compile.toList.unsafeRunSync()

    seq.map(eventToExpl) should contain theSameElementsInOrderAs  List(
      startElementEvent("item"), charactersEvent("text1"), endElementEvent("item")
    )
  }

  private def eventToExpl(ev:XMLEvent):TestEvent = ev match {
    case se:StartElement => (se.getName.getLocalPart.some, "start", None)
    case ch:Characters =>   (None, "characters", ch.getData.some)
    case ee:EndElement =>   (ee.getName.getLocalPart.some, "end", None)
  }

}
