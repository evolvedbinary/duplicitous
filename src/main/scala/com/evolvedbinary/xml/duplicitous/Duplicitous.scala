/*
 * Copyright 2015 Evolved Binary Ltd
 */
package com.evolvedbinary.xml.duplicitous

import com.evolvedbinary.xml.duplicitous.processor.XmlProcessor
import com.evolvedbinary.xml.duplicitous.rewriter.WordLengthRewriter
import grizzled.slf4j.Logger

import scala.annotation.tailrec
import scalax.file.{PathSet, Path}
import scalax.file.PathMatcher.IsFile
import scalax.io.StandardOpenOption
import scalaz._
import Scalaz._
import scalaz.concurrent.Task

import com.evolvedbinary.xml.duplicitous.dictionary.{Dictionary, WordsByLengthDictionary}
import scopt.OptionParser

object DuplicitousApp extends App {
  private implicit val logger = Logger[this.type]

  private val EXIT_CODE_ARGS_ERROR = 1
  private val EXIT_CODE_PROCESSING_ERROR = 2
  private val DEFAULT_DICT = Path.fromString("/usr/share/dict/words")

  //TODO(AR) consider a filter arg, so that we only process source files that match a pattern e.g. *.xml
  private case class Args(ignoreAttributes: Boolean = false, ignoreText: Boolean = false, ignoreComments: Boolean = false, ignoreCdata: Boolean = false, recursive: Boolean = false, dictionary: Path = DEFAULT_DICT, src: Option[Path] = None, dest: Option[Path] = None)

  //used by the scopt parser below
  private implicit val pathRead: scopt.Read[Path] = scopt.Read.reads(Path.fromString(_))
  private def fileExists(f: Path) : Either[String, Unit] = if(f.exists) { Right(()) } else Left(s"${f.path} does not exist")

  private val parser = new OptionParser[Args]("duplicitous") {
    head("duplicitous", "1.0")
    opt[Unit]("ignore-attributes") action { (x, c) =>
      c.copy(ignoreAttributes = true) } text("Ignore attribute values when rewriting")
    opt[Unit]("ignore-text") action { (x, c) =>
      c.copy(ignoreText = true) } text("Ignore text nodes when rewriting")
    opt[Unit]("ignore-comments") action { (x, c) =>
      c.copy(ignoreComments = true) } text("Ignore the text of comments when rewriting")
    opt[Unit]("ignore-cdata") action { (x, c) =>
      c.copy(ignoreCdata = true) } text("Ignore the text of CDATA sections when rewriting")
    opt[Unit]('r', "recursive") action { (x, c) =>
      c.copy(recursive = true) } text("Recursively process files in all descendant directories")
    opt[Path]('d', "dictionary") action { (x, c) =>
      c.copy(dictionary = x) } validate(fileExists) text(s"A dictionary of words to use for substitutions. If not provided then ${DEFAULT_DICT.path} is assumed")
    arg[Path]("<source>") action { (x, c) =>
      c.copy(src = Some(x)) } validate(fileExists) text("The path to the input file(s)")
    arg[Path]("<destination>") action { (x, c) =>
      c.copy(dest = Some(x)) } text("Path to write the output file(s) to")
    help("help") text("Prints this usage text")
    checkConfig { c =>
      if(c.dictionary.exists) {
        if(c.src.map(_.isDirectory).getOrElse(false) && c.dest.map(_.isFile).getOrElse(false)) {
          failure("Source is a directory, but destination is a file")
        } else {
          success
        }
      } else {
        failure("Default dictionary file does not exist")
      }
    }
  }

  parser.parse(args, Args()) match {
    case Some(parsedArgs) =>
      WordsByLengthDictionary.load(parsedArgs.dictionary).map {
        dictionary =>
          new Duplicitous(!parsedArgs.ignoreAttributes, !parsedArgs.ignoreText, !parsedArgs.ignoreComments, !parsedArgs.ignoreCdata, dictionary)
            .process(parsedArgs.src.get, parsedArgs.dest.get, parsedArgs.recursive) match {
              case Some(errors) =>
                for (error <- errors) {
                  logger.error(error.getMessage, error)
                  System.exit(EXIT_CODE_PROCESSING_ERROR)
                }
              case None =>
                //done, all ok :-)
            }
      }

    case None =>
      //args are bad, error and usage info will have been shown
      System.exit(EXIT_CODE_ARGS_ERROR)
  }
}

/**
  * Duplicitous rewrites the content of a document(s)
  * whilst maintaining the structure
  *
  * @param attributes Whether to rewrite attribute values
  * @param text Whether to rewrite text nodes
  * @param comments Whether to rewrite the content of comments
  * @param cdata Whether to rewrite the content of CDATA sections
  * @param dictionary The dictionary to use for word lookups
  */
class Duplicitous(attributes: Boolean, text: Boolean, comments: Boolean, cdata: Boolean, dictionary: Dictionary) {
  private implicit val logger = Logger[this.type]
  private val rewriter = new WordLengthRewriter(dictionary)
  private val xmlProcessor = new XmlProcessor(attributes, text, comments, cdata)

  /**
    * Process one or more documents
    *
    * @param src The source file or directory
    * @param dest The destination file or directory
    * @param recursive If the src is a directory, should we recursively process all files in all sub-directories
    *
    * @return Any errors that may have occurred
    */
  def process(src: Path, dest: Path, recursive: Boolean = false) : Option[Seq[Throwable]] = {
    if(!src.exists) {
      Some(Seq(new IllegalArgumentException("src does not exist")))
    } else {
      if (src.isFile) {
        //file input
        processFile(src, dest)
      } else {
        //directory input
        if (dest.isFile) {
          Some(Seq(new IllegalArgumentException("src is a directory, but dest is a file")))
        } else {
          processDirectory(src, dest, recursive)
        }
      }
    }
  }

  private def processDirectory(src: Path, dest: Path, recursive: Boolean) : Option[Seq[Throwable]] = {
    val files = if(recursive) {
      src.descendants(filter = IsFile)
    } else {
      src.children(filter = IsFile)
    }

    val fileTasks = files.sliding(4, 4) //TODO(AR) make configurable, ...or calculate based on a max number of parallel futures
      .toSeq
      .map(processFiles(_, src, dest))

    Task
      .gatherUnordered(fileTasks, exceptionCancels = true)
      .attemptRun
      .swap
      .toOption
      .map(Seq(_))
  }

  private def processFiles(files: PathSet[Path], srcDir: Path, dest: Path) : Task[Unit] = Task {
    @tailrec
    def process(files: Iterator[Path]) : Unit = {
      if(!files.hasNext) {
        return
      }

      val srcFile = files.next()
      val destFile = dest / srcFile.relativize(srcDir)
      destFile.parent.map(_.createDirectory(true, failIfExists = false))

      processFile(srcFile, destFile) match {
        case Some(errors) =>
          errors.map(error => logger.error(error.getMessage, error))
          throw errors.head //we throw this so we can abort the task
        case None =>
          process(files)
      }
    }

    process(files.iterator)
  }

  private def processFile(src: Path, dest: Path) : Option[Seq[Throwable]] = {
    val destFile = if(dest.isDirectory) {
      dest / src.name
    } else {
      dest
    }

    destFile.outputStream(StandardOpenOption.Read).map {
      os =>
        timed(s"Processed document ${src.path} in") {
          xmlProcessor.rewrite(src, os)(rewriter)
        }
    }.either.left.toOption
  }
}
