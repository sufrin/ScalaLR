package org.sufrin.utility
import java.nio.file.{Files, Path}
import scala.Console.out

/**
* A `Cursor` for text sequences that also keeps track of the coordinates of
 * `current`.
*/
class SourceTextCursor(iterator: Iterator[Char]) extends Cursor[Char] {
  private var _lines = 1
  private var _chars = 0
  private var _lastChar: Char = 0 // Dummy
  var promptString: String = ""

  def withStartLocation(atLine: Int, atCol: Int = 0): this.type = {
    _lines = atLine+1
    _chars = atCol
    this
  }

  def withPrompt(thePrompt: String): this.type = {
    promptString=thePrompt
    this
  }

  def prompt(): Unit = if (promptString.nonEmpty) { out.flush(); print(promptString); out.flush() }


  /** `(lines, chars)` are the coordinates of `current`  */
  def chars: Int = _chars
  /** `(lines, chars)` are the coordinates of `current`  */
  def lines: Int = _lines

  /**
   * Returns the next character from the iterator; maintaining `chars`, `lines` as
   */
  @inline private def getNext(): Char = {
    val n = iterator.next()
    _lastChar match {
      case 0 =>
      case '\n' =>
        _lines+=1
        _chars= 0
      case _ =>
        _chars += 1
    }

    _lastChar=n
    n
  }

  def hasCurrent: Boolean =
    buffer.isDefined || (iterator.hasNext && { buffer=Some(getNext()); true })
  private var buffer: Option[Char] = None
  def current: Char = buffer.get
  def current_=(element: Char): Unit = buffer=Some(element)
  def next(): Unit = {
    buffer = None
    hasCurrent
    ()
  }
}

object SourceTextCursor {

  def apply(iterator: Iterator[Char]): SourceTextCursor = new SourceTextCursor(iterator)

  def apply(iterable: Iterable[Char]): SourceTextCursor = new SourceTextCursor(iterable.iterator)

  def apply(path: Path): SourceTextCursor = {
    if (path.toString == "/dev/console")
       console
    else
       new SourceTextCursor(Files.readString(path).iterator)
  }

    def console: SourceTextCursor = {
      val reader = Option(System.console()) match {
        case Some(console) => console.reader()
        case None => new java.io.InputStreamReader(System.in)
      }
      val it = new Iterator[Char] {
        def hasNext: Boolean =  true
        def next(): Char = {
          reader.read() match {
            case -1    => '\u0004'
            case other => other.toChar
          }
        }
      }
      new SourceTextCursor(it).withPrompt("> ")
  }
}

/*
object SourceTextCursorTest extends App {
  val c = SourceTextCursor("the\nrain\nin")
  while (c.hasCurrent) {
    println(s"(${c.lines}.${c.chars} (${c.current}")
    c.next()
  }
}
*/
