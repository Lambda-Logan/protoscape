package example

import org.scalajs.dom
import org.scalajs
import org.scalajs.dom.{
  document,
  Node,
  Element,
  HTMLElement,
  HTMLSelectElement,
  HTMLTextAreaElement,
  HTMLInputElement
}

import cats._
import cats.effect.IO
import cats.effect.IOApp
import cats.implicits._
import cats.syntax.traverse
import scala.collection.WithFilter
import reflect.ClassTag
import collection.immutable.LazyList
import scala.scalajs.js.{Date}
import cats.syntax.flatMap
import org.w3c.dom.html
import cats.data.Chain
//import scala.scalajs.js.Promise
import scala.scalajs.js.timers
import scala.concurrent._
import scala.scalajs.concurrent._
//import ExecutionContext.Implicits.global
//import scala.scalajs.concurrent.JSExecutionContext.Implicits._
//import scala.concurrent.duration.{Duration, MILLISECONDS}
import cats.effect.Clock
import scala.concurrent.duration.FiniteDuration
import java.util.concurrent.ScheduledExecutorService
//import Timer[cats.effect.IO]
//import cats.effect.{IO,  Clock}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

trait Timer[F[_]] {
  def clock: Clock[F]
  def sleep(duration: FiniteDuration): F[Unit]
}

final class MyTimer(ec: ExecutionContext, sc: ScheduledExecutorService)
    extends Timer[IO] {
  override val clock: Clock[IO] =
    new Clock[IO] {
      override def realTime(unit: TimeUnit): IO[Long] =
        IO(unit.convert(System.currentTimeMillis(), MILLISECONDS))

      override def monotonic(unit: TimeUnit): IO[Long] =
        IO(unit.convert(System.nanoTime(), NANOSECONDS))
    }

  override def sleep(timespan: FiniteDuration): IO[Unit] =
    IO.cancelable { cb =>
      val tick = new Runnable {
        def run() = ec.execute(new Runnable {
          def run() = cb(Right(()))
        })
      }
      val f = sc.schedule(tick, timespan.length, timespan.unit)
      IO(f.cancel(false)).void
    }
}
/// A namespace for different ways of refering to nodes of a webpage
/// All selectors return a functor of N where N <: Node
trait Selectors {

  type Selector[F[_]] = String => F[Node]

  def getElementById: Selector[Option] = { s =>
    val elem = document.getElementById(s)
    if (elem != null) {
      Some(elem)
    } else { None }
  }

  def getElementsByClassName: Selector[List] = { s =>
    val elems = document.getElementsByClassName(s)

    if (elems != null) {
      elems.toList
    } else { List() }
  }

  def getElementsByName: Selector[List] = { s =>
    val elems = document.getElementsByName(s)
    if (elems != null) {
      elems.toList
    } else { List() }
  }

  def getElementsByTagName: Selector[List] = { s =>
    val elems = document.getElementsByTagName(s)
    if (elems != null) {
      elems.toList
    } else { List() }
  }

  def xFindAll: Selector[List] = { s =>
    val elems = document.querySelectorAll(s)
    if (elems != null) {
      elems.toList
    } else { List() }
  }
  def xFind: Selector[Option] = { s =>
    val elem = document.querySelector(s)
    if (elem != null) {
      Some(elem)
    } else { None }
  }

  def allLinks: List[Element] = document.links.toList

  // many selectors are written in a curried form
  // this is to make traverse and flatMap more ergonomic

  def getChildByXPath(path: String): (Node => Option[Node]) = { node =>
    val x = document.evaluate(
      path,
      node,
      null.asInstanceOf[dom.XPathNSResolver],
      dom.XPathResult.FIRST_ORDERED_NODE_TYPE,
      null
    )
    val elem = x.singleNodeValue

    if (elem != null) {
      Some(elem)
    } else { None }
  }

  def getElemByXPath(path: String): Option[Node] = {
    getChildByXPath(path)(document)
  }

  //https://stackoverflow.com/questions/3813294/how-to-get-element-by-innertext
  def elemThatContainsText(text: String): Option[Node] = {
    //document.evaluate()
    childThatContainsText(text)(document)
  }

  //https://stackoverflow.com/questions/3813294/how-to-get-element-by-innertext
  def childThatContainsText(text: String): (Node => Option[Node]) = {
    //document.evaluate()
    getChildByXPath(s"//*[contains(text(),'$text')]")
  }

}

/// A namespace for actions to be performed
trait ElemActions {

  def convertAsTypeOf[T: ClassTag](anyValue: Any): Option[T] = {
    anyValue match {
      case t: T =>
        return Some(t);
    }
    None
  }

  type ElemAction[T] = Node => IO[T]

  def click: ElemAction[Unit] = { e =>
    //dom.window.alert(e.toString())
    IO {
      e match {
        case _: HTMLElement =>
          e.asInstanceOf[HTMLElement].click()
        case otherwise =>
          IO.raiseError(new Exception("Expected an HTMLElement!"))
      }
    } *> IO.unit
  }

  def changeIndex(idx: Int): ElemAction[Unit] = { e =>
    IO {
      e match {
        case _: HTMLSelectElement =>
          e.asInstanceOf[HTMLSelectElement].selectedIndex = idx;
        case otherwise =>
          IO.raiseError(new Exception("Expected an HTMLSelectElement!"))
      }
    } *> IO.unit
  }

  def changeValue(value: String): ElemAction[Unit] = { e =>
    var inpt = e.asInstanceOf[HTMLInputElement]
    IO {
      e.asInstanceOf[HTMLInputElement].value = value
    }
  }

  def getSubDomain(url: String): IO[Unit] = {
    IO { dom.window.location.href = url } *>
      IO.unit
  }

  def selectEachOption(n: HTMLSelectElement): List[IO[Unit]] = {
    n.options.toList.zipWithIndex.map((opt_idx => changeIndex(opt_idx._2)(n)))
  }

  /*
  def delay(milliseconds: Int): IO[Unit] = { //Future[Unit] = {
    val p = scala.concurrent.Promise[Unit]()
    timers.setTimeout(milliseconds) {
      p.success(())
    }
    IO { Await.result(p.future, Duration(10 * milliseconds, MILLISECONDS)) }
  } */

  /*
  def sleep(timespan: FiniteDuration): IO[Unit] =
    IO.cancelable { cb =>
      val tick = new Runnable {
        def run() = ec.execute(new Runnable {
          def run() = cb(Right(()))
        })
      }
      val f = sc.schedule(tick, timespan.length, timespan.unit)
      IO(f.cancel(false)).void
    } */

  def clickWithText(
      text: String
  ): (Node => IO[Unit]) = { n =>
    IO {
      throw new Exception()
      for (opt <- n.childNodes) {
        dom.window.alert(
          (opt.innerText, opt.nodeValue, opt.textContent).toString
        )
        if (opt.innerText == text) {
          println(opt)
          opt.asInstanceOf[HTMLElement].click()
          //return IO.unit
        }
      }
    } *>
      IO.unit
  }

}

trait WebsiteConfig {
  val searchButtonID =
    "cphContent_cphMainContent_Search1_PremiumDataSearch_btnSearch"

  val zipInputID =
    "cphContent_cphMainContent_Search1_PremiumDataSearch_txtZipCodes"

  val minIncomeDropdown =
    "[//select[@name=\"ctl00$ctl00$cphContent$cphMainContent$Search1$PremiumDataSearch$FinancialForm$ddlMinIncome\"]"

  val scores = List(
    "800+",
    "750-799",
    "750-799",
    "750-799",
    "750-799",
    "750-799",
    "750-799",
    "750-799",
    "750-799",
    "750-799",
    "700-749",
    "650-699",
    "600-649",
    "550-559", //# error in cole#"550-599",
    "500-549",
    "499 & Less"
  )

  val gens = List("1", "2", "3")

  val lang = "Spanish"

  val race = "Hispanic - Hispanic Origin"

  //zipcode_id = "cphContent_cphMainContent_Search1_PremiumDataSearch_txtZipCodes"

  val search_button_id =
    "cphContent_cphMainContent_Search1_PremiumDataSearch_btnSearch"

  val pageRangeClickId =
    "cphContent_cphMainContent_SearchResults_txtPages"

  val fileTypeDropDownID =
    "ctl00_ctl00_cphContent_cphMainContent_SearchResults_rcbDownloadToComputerExportTypes_Arrow"
  val fileType = "ASCII Text File (Tab Separated)"
}

object Hello extends IOApp with Selectors with ElemActions with WebsiteConfig {

  def delay(milliseconds: Int): IO[Unit] = {
    ///IO {
    // val start = milliseconds + Date.now()
    //  while (start > Date.now()) {}
    //  } *> IO.unit
    IO.sleep(FiniteDuration(milliseconds, MILLISECONDS))
  }

  def delayed[T](milliseconds: Int, action: IO[T]): IO[T] = {
    delay(milliseconds) >> action
  }

  def delayedAction[T, A](
      milliseconds: Int,
      action: A => IO[T]
  ): (A => IO[T]) = { a =>
    delayed(milliseconds, action(a))
  }

  def enterZipcode(zipcode: String): IO[Unit] = {
    (getElementById(zipInputID) traverse changeValue(zipcode)) *> IO.unit
  }

  val clickSearch: IO[Unit] =
    (getElementById(searchButtonID) traverse click) *> IO.unit

  val fileTypeElem = getElementById(fileTypeDropDownID)

  val pageRanges =
    List("1-15", "16-30", "31-45", "46-60", "61-75", "76-90", "91-100")

  val makeEffect: (String => List[IO[Any]]) = (
      range => //pageRanges flatMap ((range: String) =>
        List(
          getElemByXPath("//img[@alt='OK']") traverse click,
          getElementById(fileTypeDropDownID) traverse click,
          delay(1000),
          elemThatContainsText(fileType) traverse click,
          getElemByXPath("//label[contains(text(),'Pages')]") traverse click,
          getElementById(pageRangeClickId) traverse changeValue(range),
          getElemByXPath("//label[contains(text(),'Pages')]") traverse click,
          delay(4 * 1000),
          getElementById(pageRangeClickId) traverse changeValue(range),
          getElemByXPath("//label[contains(text(),'Pages')]") traverse click,
          delay(500),
          getElementById("btnExportRecords") traverse click,
          delay(5 * 1000)
        )
  )
  def run(args: List[String]): IO[cats.effect.ExitCode] =
    pageRanges.flatMap(makeEffect).sequence *> IO {
      cats.effect.ExitCode(0)
    }

}
