package com.boosed

package traits

import java.util.List

import scala.collection.JavaConversions._
import scala.xml.MetaData
import scala.xml.NodeSeq.seqToNodeSeq
import scala.xml.pull._

/**
 * Trait to handle stream processing of XML by extracting specified hierarchies of elements.
 */
trait XMLProcessor {

  // type alias for node we process using until operation below
  // items are tag name, inner text & metadata
  type XMLNode = (String, String, MetaData)

  // refinement to XMLAttr
  implicit def metadata2XMLAttr(metadata: MetaData) = XMLAttr(metadata)

  // refinement to XMLSeq
  implicit def reader2XMLSeq(reader: Iterator[XMLEvent]) = XMLSeq(reader)

  // refinement to XMLTag
  implicit def string2XMLTag(elem: String) = elem.exists(':'==) match {
    case false => XMLTag(elem)
    case true => val args = elem.split(':'); XMLTag(args(1), args(0))
  }

  // first element of tuple is namespace, second is element
  implicit def pair2XMLTag(pair: (String, String)) = XMLTag(pair._2, pair._1)

  // iterator to singleton
  implicit def iter2Singleton[A](iter: Iterator[A]) = iter.next

  // iterator to list
  implicit def iter2List[A](iter: Iterator[A]): List[A] = iter.toList

  // walk through sequence until ending element tag is matched
  def until[A, B](end: A)(cond: PartialFunction[XMLNode, Unit])(
    implicit reader: Iterator[XMLEvent],
    ev: A => XMLTag) {

    // iterate through 
    reader foreach {
      // perform partial function on discovered tag
      // lift handles all undefined points so doesn't need to account
      // for catch all case in partial functions
      case EvElemStart(pre, label, attrs, _) =>
        cond lift (Option(pre).fold(label)(x => "%s:%s" format (x, label)), reader.text, attrs)
        
      // terminate on end tag
      case EvElemEnd(pre, label) if end.check(pre, label) => return

      case _ => // do nothing
    }
  }

  // refinement class to retrieve element attributes
  case class XMLAttr(md: MetaData) {

    // retrieve an attribute for this tag (e.g., attrs \ "id"); if not present,
    // returns an empty string
    //def \(id: String) = md.get(id).toRight("").fold(identity, _.text.trim)
    //def \(id: String) = md(id).text.trim
	def \(id: String) = md.get(id)
  }

  // refinement class to pull attributes & inner text from XML events
  // NOTE: this class is mutable and causes the passed iterator to advance; for this reason
  // it is recommended to ensure conversion to Attributable occurs only once
  case class XMLSeq(reader: Iterator[XMLEvent]) { //extends Iterator[XMLEvent] {
    //
    //    // implementations of iterator
    //    def hasNext = reader.hasNext
    //    def next = reader.next
    //
    //    // change these to methods so that references will update wrt current position
    //    // of the iterator
    //    //    private def start = if (reader.hasNext) Option(reader.next) else None
    //    //else throw new NoSuchElementException("no element available for metadata")
    //    //    private def inner = if (reader.hasNext) Option(reader.next) else None
    //    //else throw new NoSuchElementException("no element available for text")
    //
    //    //    // return attribute from element
    //    //    def attr(name: String) = start
    //    //      // if no elements, throw exception
    //    //      .toRight(throw new NoSuchElementException("no element available for metadata"))
    //    //      .fold(
    //    //        identity, // for exception
    //    //        // partial function for start element
    //    //        {
    //    //          case EvElemStart(_, _, attrs, _) => attrs.get(name).toRight("").fold(
    //    //            identity,
    //    //            b => b.text)
    //    //          case _ => ""
    //    //        })

    // return inner text from element
    def text = if (reader.hasNext) reader.next match {
      case EvText(text) => text.trim.replaceAll("\\s+", " ")
      case _ => "" // empty string if iterator has no inner text node
    }
    else "" // return empty string if reader is empty

    //    //    def text = inner
    //    //      // if no inner text, throw exception
    //    //      .toRight( /* throw new NoSuchElementException("no element available for text")*/ "")
    //    //      .fold(
    //    //        identity, // for exception
    //    //        // return text or empty string if not an event text node
    //    //        {
    //    //          case EvText(text) => text.trim.replaceAll("\\s+", " ")
    //    //          case _ => ""
    //    //        })
    //
    //    //    // pulls out the elements by section
    //    //    def parseXml[A <% XMLTag](tags: A*): XMLSeq = {
    //    //
    //    //      // parse for the nested tags
    //    //      def doit(tag: A): XMLReader = {
    //    //        // whether the end nested tag has been reached
    //    //        var isNotEnd = true
    //    //
    //    //        // continue stepping through iterator to find matching elements
    //    //        dropWhile {
    //    //          // drop until start tag is reached
    //    //          case EvElemStart(pre, label, _, _) if tag.check(pre, label) => false
    //    //
    //    //          // collect otherwise
    //    //          case _ => true
    //    //        } takeWhile {
    //    //          // take until end tag is reached (this is safe, uses another iterator)
    //    //          case EvElemEnd(pre, label) if tag.check(pre, label) =>
    //    //            // end reached, terminate collection
    //    //            isNotEnd = false
    //    //            true
    //    //
    //    //          // terminates after set false when end element reached 
    //    //          case _ => isNotEnd
    //    //        }
    //    //      }
    //    //
    //    //      // perform parse through the list of tags
    //    //      // (i.e., essentially whittles down the iterator)
    //    //      (this /: tags)((_, tag) => doit(tag))
    //    //    }
    //
    //    //    // pulls out matching nested element and performs function repeatedly until end;
    //    //    // if nesting repeats, be sure to flatten to remove intermediate iterators
    //    //    // USE THIS ONLY IF CHILD TAGS ONLY CONSIST OF ONE ELEMENT TYPE
    //    //    def mapXml[A <% XMLTag, B](tag: A)(f: XMLSeq => B): Iterator[B] =
    //    //      parseXml(tag) match {
    //    //        // if sequence is not empty, continue to parse elements
    //    //        case seq if !seq.isEmpty => Iterator.single(f(seq)) ++ mapXml(tag)(f)
    //    //
    //    //        // return an empty iterator
    //    //        case _ => Iterator[B]() // end is reached
    //    //      }
  }

  // refinement class to test for element matches
  case class XMLTag(elem: String, ns: String = null) {
    // perform a check against tag name (and namespace)
    // val opposed to def so that namespace element is evaluated once only
    val check: (String, String) => Boolean = Option(ns) match {
      // check that label & namespace match
      case Some(x) => x == _ && elem == _

      // check only that label matches
      case None => (_, label) => elem == label
    }
  }
}