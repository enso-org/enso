module org.enso.scala.wrapper {
  requires scala.library;
  requires scala.reflect;
  requires org.jline;
  requires org.slf4j;

  // "org.typelevel" % ("cats-core_" + scalaVer) % "2.10.0",
  exports cats;
  exports cats.arrow;
  exports cats.compat;
  exports cats.conversions;
  exports cats.data;
  exports cats.evidence;
  exports cats.instances;
  exports cats.instances.symbol;
  exports cats.syntax;

  // "org.typelevel" % ("cats-kernel_" + scalaVer) % "2.10.0",
  exports cats.kernel;
  exports cats.kernel.compat;
  exports cats.kernel.instances;
  exports cats.kernel.instances.all;
  exports cats.kernel.instances.arraySeq;
  exports cats.kernel.instances.bitSet;
  exports cats.kernel.instances.map;
  exports cats.kernel.instances.bigInt;
  exports cats.kernel.instances.deadline;
  exports cats.kernel.instances.bigDecimal;
  exports cats.kernel.instances.set;
  exports cats.kernel.instances.seq;
  exports cats.kernel.instances.stream;
  exports cats.kernel.instances.vector;
  exports cats.kernel.instances.sortedMap;
  exports cats.kernel.instances.either;
  exports cats.kernel.instances.duration;
  exports cats.kernel.instances.list;
  exports cats.kernel.instances.unit;
  exports cats.kernel.instances.finiteDuration;
  exports cats.kernel.instances.queue;
  exports cats.kernel.instances.tuple;
  exports cats.kernel.instances.option;
  exports cats.kernel.instances.uuid;
  exports cats.kernel.instances.string;
  exports cats.kernel.instances.sortedSet;

  // "com.github.plokhotnyuk.jsoniter-scala" % ("jsoniter-scala-core_" + scalaVer) %
  // jsoniterVersion,
  exports com.github.plokhotnyuk.jsoniter_scala.core;

  // "com.github.plokhotnyuk.jsoniter-scala" % ("jsoniter-scala-macros_" + scalaVer) %
  // jsoniterVersion,
  exports com.github.plokhotnyuk.jsoniter_scala.macros;

  // "com.typesafe.scala-logging" % ("scala-logging_" + scalaVer) % scalaLoggingVersion,
  exports com.typesafe.scalalogging;

  // "io.circe" % ("circe-core_" + scalaVer) % circeVersion,
  exports io.circe;
  exports io.circe.cursor;
  exports io.circe.export;
  exports io.circe.numbers;
  exports io.circe.syntax;

  // "io.circe" % ("circe-parser_" + scalaVer) % circeVersion,
  exports io.circe.parser;

  // "io.circe" % ("circe-generic_" + scalaVer) % circeVersion,
  exports io.circe.generic.decoding;
  exports io.circe.generic;
  exports io.circe.generic.util;
  exports io.circe.generic.util.macros;
  exports io.circe.generic.codec;
  exports io.circe.generic.auto;
  exports io.circe.generic.encoding;

  // "io.circe" % ("circe-jawn_" + scalaVer) % circeVersion,
  exports io.circe.jawn;

  // "org.typelevel" % ("jawn-parser_" + scalaVer) % "1.5.1",
  exports org.typelevel.jawn;

  // "com.chuusai" % ("shapeless_" + scalaVer) % "2.3.10",
  exports shapeless;
  exports shapeless.ops;
  exports shapeless.ops.record;
  exports shapeless.test;
  exports shapeless.syntax;
  exports shapeless.syntax.std;
}
