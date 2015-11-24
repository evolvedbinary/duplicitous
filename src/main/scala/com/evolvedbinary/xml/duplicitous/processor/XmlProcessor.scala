/*
 * Copyright 2015 Evolved Binary Ltd
 */
package com.evolvedbinary.xml.duplicitous.processor

import java.io.OutputStream
import java.nio.ByteBuffer
import java.nio.channels.SeekableByteChannel
import java.nio.file.{Files, Paths, StandardOpenOption}
import javax.xml.stream.XMLStreamConstants._
import javax.xml.stream.{XMLInputFactory, XMLStreamWriter}

import com.evolvedbinary.xml.duplicitous.processor.XmlProcessor._
import com.evolvedbinary.xml.duplicitous.rewriter.{IdentityRewriter, Rewriter}
import com.fasterxml.aalto.AsyncXMLStreamReader.EVENT_INCOMPLETE
import com.fasterxml.aalto.stax.{InputFactoryImpl, OutputFactoryImpl}
import com.fasterxml.aalto.{AsyncByteBufferFeeder, AsyncXMLStreamReader}
import org.codehaus.stax2.XMLStreamWriter2
import resource._

import scala.annotation.tailrec
import scalax.file.Path
import scalaz._

/**
  * Rewrites the textual content of an XML document
  *
  * @param attributes Whether to rewrite attribute values
  * @param text Whether to rewrite text nodes
  * @param comments Whether to rewrite the content of comments
  * @param cdata Whether to rewrite the content of CDATA sections
  */
class XmlProcessor(attributes: Boolean = false, text: Boolean = false, comments: Boolean = false, cdata: Boolean = false) {

  /**
    * Rewrites an XML file
    *
    * @param src The XML file to transform
    * @param dest The destination for the transformed XML
    * @param rewriter A function which rewrites a string
    * @return Any errors which occur
    */
  def rewrite(src: Path, dest: OutputStream)(rewriter: StaxRewriter = IdentityRewriter): Option[Seq[Throwable]] = {

    @tailrec
    def process(channel: SeekableByteChannel, buf: ByteBuffer, reader: AsyncReader, writer: XMLStreamWriter2, prevEvent: EventType = -1) {
      @tailrec
      def nextEvent(channel: SeekableByteChannel, buf: ByteBuffer, reader: AsyncReader) : EventType = {
        val event = reader.next()
        if(event != EVENT_INCOMPLETE) {
          return event
        }

        //we need to feed more input to the parser
        val read = channel.read(buf)
        buf.flip()

        val feeder = reader.getInputFeeder()
        if (read == -1) {
          feeder.endOfInput()
        } else {
          feeder.feedInput(buf)
        }

        nextEvent(channel, buf, reader)
      }

      def processEvent(event: EventType, reader: AsyncReader, writer: XMLStreamWriter2) : Option[Seq[Throwable]] = {
        event match {

          case CDATA if(cdata) =>
            val rewritten = rewriter.rewrite(event, reader.getText())
            rewritten.map(writer.writeCData)
            rewritten.swap.toOption

          case COMMENT if(comments) =>
            val rewritten = rewriter.rewrite(event, reader.getText())
            rewritten.map(writer.writeComment)
            rewritten.swap.toOption

          case START_ELEMENT if(attributes && reader.getAttributeCount() > 0) =>
            val elemName = reader.getName()
            writer.writeStartElement(elemName.getPrefix(), elemName.getLocalPart(), elemName.getNamespaceURI())
            rewriteAttributes(reader, writer)

          case CHARACTERS if(text) =>
            val rewritten = rewriter.rewrite(event, reader.getText())
            rewritten.map(writer.writeCharacters)
            rewritten.swap.toOption

          case _=>
            writer.copyEventFromReader(reader, false)
            None
        }
      }

      @tailrec
      def rewriteAttributes(reader: AsyncReader, writer: XMLStreamWriter, attrIdx: Int = 0) : Option[Seq[Throwable]] = {
        if(attrIdx == reader.getAttributeCount()) {
          return None
        }

        val attrName = reader.getAttributeName(attrIdx)
        rewriter.rewrite(ATTRIBUTE, reader.getAttributeValue(attrIdx)) match {
          case -\/(errors) =>
            Some(errors)
          case \/-(rewritten) =>
            writer.writeAttribute(attrName.getPrefix(), attrName.getNamespaceURI(), attrName.getLocalPart(), rewritten)
            rewriteAttributes(reader, writer, attrIdx + 1)
        }
      }

      if(prevEvent == END_DOCUMENT) {
        return
      }

      val event = nextEvent(channel, buf, reader)
      processEvent(event, reader, writer)
      process(channel, buf, reader, writer, event)
    }

    val inputFactory = new InputFactoryImpl()
    inputFactory.configureForSpeed()
    inputFactory.setProperty(XMLInputFactory.SUPPORT_DTD, false)
    inputFactory.setProperty(XMLInputFactory.IS_VALIDATING, false)

    val outputFactory = new OutputFactoryImpl()
    outputFactory.configureForSpeed()

    managed(inputFactory.createAsyncForByteBuffer())
      .and(managed(outputFactory.createXMLStreamWriter(dest).asInstanceOf[XMLStreamWriter2]))
      .map {
        case (reader, writer) =>

          managed(Files.newByteChannel(Paths.get(src.path), StandardOpenOption.READ)).map {
            channel =>
              val buf = ByteBuffer.allocate(16384) //TODO(AR) 16kb, whould this be configurable?
              process(channel, buf, reader, writer)
          }.either
      }.either.joinRight.left.toOption
  }
}

object XmlProcessor {
  protected type AsyncReader = AsyncXMLStreamReader[AsyncByteBufferFeeder]

  /**
    * The EventType is an Int value taken
    * from {@link javax.xml.stream.XMLStreamConstants}
    */
  type EventType = Int

  /**
    * A rewriter for StAX based event parsing
    */
  type StaxRewriter = Rewriter[_ >: EventType]
}
