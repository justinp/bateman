package org.scalawag.bateman.json

import cats.data.ValidatedNec

/** Tokenizer -> turns a stream of chars into tokens
  *  Parser -> turns a stream of tokens into a stream of JSON events
  *  Eventer -> turns a stream of events into a stream of JAnys
  *  Documenter -> turns a stream of events into a JAny (just pukes on extra? maybe doesn't need to exist.)
  * */
package object parser {
  type ParseResult[+A] = ValidatedNec[SyntaxError, A]
}
