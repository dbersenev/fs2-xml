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

import java.nio.file.Paths
import java.util.concurrent.Executors

import cats._
import cats.implicits._
import cats.effect._
import org.http4s.client.{Client, JavaNetClientBuilder}
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.client.dsl.io._
import org.http4s.headers._
import org.http4s.{Headers, MediaType, Method, Request, Uri}
import fs2._

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}
import scala.concurrent.duration._
import scala.xml.Elem

import parsing.XMLSelector._
import parsing._

object OrcidTest extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    IO(Executors.newFixedThreadPool(30)).bracket(ex => IO(ExecutionContext.fromExecutor(ex))
      .flatMap(ec => BlazeClientBuilder[IO](ec).withRequestTimeout(10.seconds).withMaxTotalConnections(200)
        .resource.use(cl => withClient(args.head, args(1), cl)).as(ExitCode.Success))
    )(ex => IO(ex.shutdownNow()))
  }

  private def withClient(input:String, output:String, client: Client[IO]): IO[Unit] = {

    def newReq(orcid: String): Request[IO] = Request[IO](
      method = Method.GET,
      uri = Uri.fromString(s"https://pub.orcid.org/v2.1/$orcid/record").toOption.get,
      headers = Headers.empty.put(Accept(MediaType.application.xml))
    )

    val tp1 = IO(Executors.newFixedThreadPool(16))
    val tp2 = IO(Executors.newFixedThreadPool(18))

    tp2.bracket(ex2 => IO(ExecutionContext.fromExecutor(ex2))
      .flatMap(ec2 => {
        tp1.bracket(ex1 => IO(ExecutionContext.fromExecutor(ex1))
          .flatMap(ec1 => {
            val oin: Stream[IO, (String, Long)] = io.file.readAll[IO](Paths.get(input), ec1, 16000).through(text.utf8Decode)
              .through(text.lines).map(line => line.split(",").head).drop(1).zipWithIndex
            val result = oin.parEvalMapUnordered(50)(orcid => IO(println(s"Processed line ${orcid._2 + 1}")) >> client.stream(newReq(orcid._1))
              .flatMap(resp =>
                resp.body.filter(_ != 0)
                .through(EventStream[IO](ec2))
                .through(SelectedEventStream(
                  XMLSelector(root("record") |\| "person" |\| "external-identifiers" |\| "external-identifier"))
                )
                  .through(ElementStream.apply)
                  .collectFirst {
                    case el: Elem if (el \ "external-id-type").exists(n => n.text.trim === "ResearcherID") =>
                      (orcid._1, (el \ "external-id-value").head.text, "ok")
                  }.handleErrorWith(err => Stream.empty)
              ).handleErrorWith(err => Stream.empty).compile.last
            ).unNone.map(orcRidStat => s"${orcRidStat._1}|${orcRidStat._2}")
              .intersperse("\n")
              .through(text.utf8Encode)
              .through(io.file.writeAll(Paths.get(output), ec2))
            result.compile.drain
          })
        )(ex => IO(ex.shutdownNow()))
      }
      ))(ex2 => IO(ex2.shutdownNow()))

  }
}
