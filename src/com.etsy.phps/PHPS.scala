package com.etsy.phps

import scala.io.Source


/**
 * Exception raised when there are parser errors.
 */
class PHPSException(message : String) extends Exception(message)


// Access levels for object fields 
case class Access()
case object Public extends Access
case object Private extends Access
case object Protected extends Access


/**
 * Represents a serialized PHP object instance. 
 */
class PHPObject(val className : String, 
                val fields : Map[String, Any],
                val fieldAccess : Map[String, Access])


/**
 * Object that can be used to parse PHP-serialized data. 
 */
object PHPS {

  /**
   * Parses the input PHPS data and returns scala objects.
   */
  def parse(s : Source) : Any = (new PHPSParser(s)).parse
}



/**
 * The obvious question might be, "why not use the scala parser library?" Well,
 * I tried that at first and gave up on it. The length-prefixed strings in PHPS
 * (possibly containing unescaped quotes) seemed easier to deal with using 
 * this method. Nevertheless, having done this, I think it might be worthwhile
 * to revisit using the parser library at some point. If for no other reason
 * than the fact that error reporting would work much better. 
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
        case 'O' => parseObject()
        case x => error("Unexpected character: "+x)
      }
    }
  }

  private def parser[A](f : => A) = () => {
    discard(':')
    f
  }

  private def str(cs : Iterator[Char]) = new String(Array(cs.toList : _*))
  
  private def until(c : Char) = str(source.takeWhile(_!=c))
  
  private lazy val parseInt = parser { until(';').toInt }

  private lazy val parseDouble = parser { until(';').toDouble }

  private lazy val parseString = parser { 
    val s = stringBody
    discard(";")
    s
  }

  private def stringBody = {
    val l = parseLength
    val s = str(source.drop(1).take(l))
    discard('"')
    s
  }

  private lazy val parseBool = parser { 
    val b = source.next match {
      case '1' => true
      case '0' => false
    }
    discard(';')
    b
  }

  private def parseNull = { discard(';') ; null }

  private lazy val parseArray = parser { 
    val vs = arrayBody
    simpleArray(vs) match {
      case Some(a) => a
      case None => Map(vs.toList : _*)
    }
  }

  private def arrayBody = {
    val l = parseLength
    discard('{')
    val vs = (0 until l).map(_ => parseAssoc).force
    discard('}')
    vs
  }

  private lazy val parseObject = parser {
    val name = stringBody
    discard(':')
    val fields = arrayBody
    val (values, access) = parseFields(fields)
    new PHPObject(name, values, access)
  }

  private val FieldName = "\0(.*)\0(.*)".r

  private def parseFields(fields : RandomAccessSeq[(Any, Any)]) :
    (Map[String, Any], Map[String, Access]) = {
      var vs = List[(String, Any)]()
      var as = List[(String, Access)]()

      fields.foreach { case (k : String, v) => {
          val (access, name) = k match {
            case FieldName(access, name) => access match {
              case "*" => Protected -> name
              case _ => Private -> name
            }
            case name : String => Public -> name
          }
          vs = name -> v :: vs
          as = name -> access :: as
        }
      }
      Map(vs : _*) -> Map(as : _*)
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
