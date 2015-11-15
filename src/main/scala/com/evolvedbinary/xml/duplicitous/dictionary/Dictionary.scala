/*
 * Copyright 2015 Evolved Binary Ltd
 */
package com.evolvedbinary.xml.duplicitous.dictionary

import com.evolvedbinary.xml.duplicitous.dictionary.Dictionary.{Word, WordLength}
import scalaz._
import Scalaz._

trait Dictionary {

  /**
    * Get a random word of a particular length
    *
    * @param wordLength The length of the random word
    * @return A random word
    */
  def random(wordLength: WordLength): \/[DictionaryError, Word]
}

sealed trait DictionaryError {
  def msg: String
}
case class InvalidWordLength(override val msg: String) extends DictionaryError

object Dictionary {
  type Word = String
  type WordLength = Int
}