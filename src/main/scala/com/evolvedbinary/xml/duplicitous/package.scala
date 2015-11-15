/*
 * Copyright 2015 Evolved Binary Ltd
 */
package com.evolvedbinary.xml

import grizzled.slf4j.Logger

package object duplicitous {

  /**
   * Logs the time taken by a function
   *
   * @param msg A message which will be appended with " <duration>ms"
   * @param f The function to time
   * @param logger An (implicit) logger to record the time to
   *
   * @return The result of f
   */
  def timed[T](msg: String)(f: => T)(implicit logger: Logger) : T = {
    val start = System.currentTimeMillis()
    val res = f
    logger.info(s"$msg ${System.currentTimeMillis() - start}ms")
    res
  }
}
