/*
 * Copyright 2015 Evolved Binary Ltd
 */
package com.evolvedbinary.xml.duplicitous.dictionary

import java.io.IOException

import com.evolvedbinary.xml.duplicitous.dictionary.Dictionary.{Word, WordLength}

import scala.collection.immutable.TreeMap
import scala.util.{Random, Try}
import scalax.file.Path
import scalaz._
import Scalaz._
import scalaz.concurrent.Task
import scalaz.stream._
import scalaz.stream.io._

import grizzled.slf4j.Logger
import WordsByLengthDictionary._
import com.evolvedbinary.xml.duplicitous.timed

/**
 * A dictionary which organises words by their length
 */
class WordsByLengthDictionary(words: Words) extends Dictionary {

  private lazy val maxWordLength = words.length
  private lazy val rnd = new Random()

  override def random(wordLength: WordLength): \/[DictionaryError, Word] = {
    if(wordLength < 1 || wordLength > maxWordLength) {
      InvalidWordLength(s"Invalid word length: $wordLength. Word length must be between 1 and $maxWordLength").left
    } else {
      val lengthIdx = wordLength - 1
      val wordsOfLengthN = words(lengthIdx).length
      val wordIdx = rnd.nextInt(wordsOfLengthN)
      words(lengthIdx)(wordIdx).right
    }
  }
}

object WordsByLengthDictionary {
  type Occurrences = Int
  type WordStats = TreeMap[WordLength, Occurrences]
  type Words = Array[Array[String]]

  private implicit val logger = Logger[this.type]

  /**
   * Given a dictionary file of words (one per line)
   * we load the words into memory
   *
   * The returned Words are arranged by their length and then by
   * their order within the dictionary file
   *
   * @return 2D array with the structure: words(word-length)(index)
   */
  private def loadDictionary(dictionary: Path) : \/[IOException, Words] = {

    /**
     * Analyzes a provided dictionary file
     *
     * @param dictionary The path to the dictionary file
     * @return Either An ordered map of word lengths and their occurrences or an IOException
     */
    def analyzeDictionary(dictionary: Path): \/[IOException, WordStats] = {
      try {
        io.linesR(dictionary.path)
          .map(_.length)
          .fold(TreeMap.empty[Int, Int]) {
            case (map, length) =>
              val count = map.get(length).getOrElse(0) + 1
              map + (length -> count)
          }
          .runLast
          .run
          .map(_.right)
          .getOrElse(new IOException("Unable to parse dictionary").left)
      } catch {
        case ioe: IOException =>
          ioe.left
      }
    }

    /**
     * Loads the words into memory
     *
     * @param dictionary The path to the dictionary file
     * @return Either The loaded words or an IOException
     */
    def loadWords(dictionary: Path, wordStats: WordStats) : \/[IOException, Words] = {
      /**
       * Stores a word into the words store
       *
       * @param store The words store
       */
      def wordStorage(store: Words) : Sink[Task, Word] = resource(Task.delay(Array.ofDim[Int](wordStats.size)))(_ => Task.delay(Unit)) {
        occurrences =>
          Task.now((word: Word) =>
            Task.delay {
              val idx = occurrences(word.length - 1)
              store(word.length - 1)(idx) = word
              occurrences(word.length - 1) = idx + 1
            }
          )
      }

      //prepare the storage
      val words = Array.ofDim[Array[Word]](wordStats.size)
      for ((wordLength, occurrences) <- wordStats) {
        words(wordLength - 1) = Array.ofDim[Word](occurrences)
      }

      //load the words from the dictionary file into memory
      try {
        io.linesR(dictionary.path)
          .to(wordStorage(words))
          .run
          .run

        words.right
      } catch {
        case e: IOException =>
          e.left
      }
    }

    //we perform two passes on the dictionary file
    //first we analyze the length and occurrences of words of length,
    //and then secondly we load the words into an array in memory
    timed(s"Analyzed dictionary ${dictionary.name} in")(analyzeDictionary(dictionary))
      .flatMap(timed(s"Loaded dictionary ${dictionary.name} in")(loadWords(dictionary, _)))
  }

  private implicit class SafeArray[T](val array: Array[T]) {
    def get(idx: Int) : Option[T] = Try(array(idx)).toOption
  }

  /**
   * Load a dictionary
   *
   * @param dictionary The path to the dictionary file
   *
   * @return Either a WordsByLengthDictionary or an IOException
   */
  def load(dictionary: Path) : \/[IOException, WordsByLengthDictionary] = {
    loadDictionary(dictionary).map {
      words =>
        logger.info(s"Dictionary is ${words.map(_.length).reduceLeft(_ + _)} words, arranged in ${words.length} different word lengths")
        new WordsByLengthDictionary(words)
    }
  }
}
