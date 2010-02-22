package com.etsy.phps

import scala.io.Source


class PHPSException(message : String) extends Exception(message)


object PHPS {

  def parse(s : Source) : Any = (new PHPSParser(s)).parse

}


/**
 * The obvious question might be, "why not use the scala parser library?" Well,
 * I tried that at first and gave up on it. The length-prefixed strings in PHPS
 * (possibly containing unescaped quotes) are just easier to deal with using
 * this method.
 */
private[phps] class PHPSParser(var source : Source) {

  def parse : Any = {
    if(!source.hasNext) {
      null
    } else {
      source.next match {
        case 'i' => parseInt()
        case 'd' => parseDouble()
        case 's' => parseString()
        case 'b' => parseBool()
        case 'N' => parseNull
        case 'a' => parseArray()
        case x => error("Unexpected character: "+x)
      }
    }
  }

  private def parser[A](f : => A) = () => {
    discard(':')
    val x = f
    x
  }

  private def str(cs : Iterator[Char]) =  new String(Array(cs.toList : _*))
  
  private def until(c : Char) = { 
    val x = str(source.takeWhile(_!=c))
    x 
  }
  
  private lazy val parseInt = parser { until(';').toInt }

  private lazy val parseDouble = parser { until(';').toDouble }

  private lazy val parseString = parser { 
    val l = parseLength
    val s = str(source.drop(1).take(l))
    discard("\";")
    s
  }

  private lazy val parseBool = parser { 
    source.next match {
      case '1' => true
      case '0' => false
    }
  }

  private def parseNull = { source.drop(1) ; null }

  private lazy val parseArray = parser { 
    val l = parseLength
    discard('{')
    val vs = (0 until l).map(_ => parseAssoc).force
    discard('}')
    simpleArray(vs) match {
      case Some(a) => a
      case None => Map(vs.toList : _*)
    }
  }

  private def simpleArray(vs : RandomAccessSeq[(Any, Any)]) : 
    Option[List[Any]] = {
    val xs = for(i <- 0 until vs.length ; if vs(i)._1 == i) 
               yield vs(i)._2
    if(xs.length == vs.length) Some(xs.force.toList) else None
  }


  private def parseAssoc = parse -> parse

  private def parseLength = until(':').toInt

  private def error(msg : String) {
    throw new PHPSException("Error at position %s: %s".format(source.pos, msg))
  }

  private def discard(s : String) { 
    (0 until s.length).foreach(i => discard(s.charAt(i)))
  }

  private def discard(c : Char) {
    if(!source.hasNext) { 
      error("Unexpected EOF. Expected %s.".format(c))
    }
    source.next match {
      case x : Char if x == c => ()
      case x => error("Expected %s but found %s".format(c, x))
    }
  }

}
