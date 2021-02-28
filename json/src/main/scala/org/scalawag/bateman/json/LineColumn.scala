package org.scalawag.bateman.json

final case class LineColumn(line: Int, column: Int, source: Option[String] = None) {
  override val toString: String = s"${source.map(x => s"$x:").getOrElse("")}$line:$column"
}

object LineColumn {
  implicit val ordering: Ordering[LineColumn] = Ordering.by(pos => pos.source -> pos.line -> pos.column)
}
