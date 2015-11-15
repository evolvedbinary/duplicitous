/*
 * Copyright 2015 Evolved Binary Ltd
 */
package com.evolvedbinary.xml.duplicitous.rewriter

import com.evolvedbinary.xml.duplicitous.dictionary.Dictionary
import com.evolvedbinary.xml.duplicitous.dictionary.Dictionary._

import scala.annotation.tailrec
import scala.util.Random
import scalaz.Scalaz._
import scalaz._

class WordLengthRewriter(dictionary: Dictionary) extends Rewriter[AnyRef] {

  override def rewrite[B >: AnyRef](event: B, string: String): Disjunction[Seq[Throwable], String] = {
    def isWhitespace(c: Char) = c == ' ' || c == '\n' || c == '\r' || c == '\t' || c == 0x0B || c == '\f'
    def isPunctuation(c: Char) = (c >= 0x21 && c <= 0x2F) || (c >= 0x3A && c <= 0x40) || (c >= 0x5B && c <= 60) || (c >= 0x7B && c <= 0X7E)

    def notWord(c: Char) = isWhitespace(c) || isPunctuation(c)
    def isWord(c: Char) = !isWhitespace(c) && !isPunctuation(c)

    def takeWord(s: String, word: StringBuilder = new StringBuilder(), idx: Int = 0): (String, Int) = takeUntil(s, word, idx, notWord)
    def takeNonWord(s: String, whitespace: StringBuilder = new StringBuilder, idx: Int = 0): (String, Int) = takeUntil(s, whitespace, idx, isWord)

    @tailrec
    def takeUntil(s: String, word: StringBuilder, idx: Int, until: Char => Boolean): (String, Int) = {
      if (idx == s.length) {
        return (word.toString(), idx)
      }

      val c = s(idx)
      if (until(c)) {
        return (word.toString(), idx)
      }

      takeUntil(s, word.append(c), idx + 1, until)
    }

    @tailrec
    def processString(s: String, result: StringBuilder = new StringBuilder(), offset: Int = 0): String = {
      if (offset == s.length) {
        return result.toString()
      }

      val (word: String, wsOffset: Int) = takeWord(s, idx = offset)
      val nextOffset = if (wsOffset == offset) {
        val (nonWord, wordOffset) = takeNonWord(s, idx = offset)
        result.append(nonWord)
        wordOffset
      } else {
        result.append(replaceWord(word))
        wsOffset
        //TODO(AR) next step should be takeWhitespace, but will be takeWord... we should be able to shortcut here
      }

      processString(s, result, nextOffset)
    }

    def replaceWord(word: Word): Word = {
      val length = word.length
      dictionary.random(length)
        .getOrElse(generateRandomString(length))
    }

    lazy val rnd = new Random()
    def generateRandomString(length: Int) = {
      def randomBetween(min: Int, max: Int): Int = rnd.nextInt(max - min + 1) + min
      val string = Array.ofDim[Char](length)
      for (i <- 0 until length) {
        string(i) = randomBetween(97, 122).toChar
      }
      new String(string)
    }

    processString(string).right
  }
}
