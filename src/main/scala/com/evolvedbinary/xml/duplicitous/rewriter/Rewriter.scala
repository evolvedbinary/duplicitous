/*
 * Copyright 2015 Evolved Binary Ltd
 */
package com.evolvedbinary.xml.duplicitous.rewriter

import scalaz._
import Scalaz._

trait Rewriter[+T] {

  /**
    * A function for transforming a String from one value to another
    *
    * Given an EventType and String, we expect either a String result
    * or details of an error that occured
    *
    * @param event The type of event that triggered the rewrite
    * @param string The string to rewrite
    * @return Either the rewritten string or an error
    */
  def rewrite[B >: T](event: B, string: String) : \/[Seq[Throwable], String]
}

/**
  * A rewriter which does no rewriting
  *
  * The input is copied to the output
  */
object IdentityRewriter extends Rewriter[AnyRef] {
  override def rewrite[B >: AnyRef](event: B, string: String): Disjunction[Seq[Throwable], String] = string.right
}
