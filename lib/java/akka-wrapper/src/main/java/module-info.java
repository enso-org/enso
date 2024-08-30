module org.enso.akka.wrapper {
  requires protobuf.java;
  requires scala.library;
  requires org.reactivestreams;
  // For sun.misc.Unsafe - from akka.util.Unsafe
  requires jdk.unsupported;
  requires org.slf4j;

  // akka-actor
  exports akka;
  exports akka.actor;
  exports akka.actor.dungeon;
  exports akka.actor.setup;
  exports akka.annotation;
  exports akka.compat;
  exports akka.dispatch;
  exports akka.dispatch.affinity;
  exports akka.dispatch.internal;
  exports akka.dispatch.sysmsg;
  exports akka.event;
  exports akka.event.japi;
  exports akka.event.jul;
  exports akka.io;
  exports akka.io.dns;
  exports akka.io.dns.internal;
  exports akka.japi;
  exports akka.japi.function;
  exports akka.japi.pf;
  exports akka.japi.tuple;
  exports akka.pattern;
  exports akka.pattern.extended;
  exports akka.pattern.internal;
  exports akka.routing;
  exports akka.serialization;
  exports akka.util;
  exports akka.util.ccompat;

  // akka-actor-typed
  exports akka.actor.typed;
  exports akka.actor.typed.delivery;
  exports akka.actor.typed.delivery.internal;
  exports akka.actor.typed.eventstream;
  exports akka.actor.typed.internal;
  exports akka.actor.typed.internal.adapter;
  exports akka.actor.typed.internal.jfr;
  exports akka.actor.typed.internal.pubsub;
  exports akka.actor.typed.internal.receptionist;
  exports akka.actor.typed.internal.routing;
  exports akka.actor.typed.javadsl;
  exports akka.actor.typed.pubsub;
  exports akka.actor.typed.receptionist;
  exports akka.actor.typed.scaladsl;
  exports akka.actor.typed.scaladsl.adapter;

  // akka-http-core
  exports akka.http;
  exports akka.http.ccompat.imm;
  exports akka.http.impl.engine;
  exports akka.http.impl.engine.client;
  exports akka.http.impl.engine.client.pool;
  exports akka.http.impl.engine.http2;
  exports akka.http.impl.engine.http2.client;
  exports akka.http.impl.engine.http2.framing;
  exports akka.http.impl.engine.http2.hpack;
  exports akka.http.impl.engine.http2.util;
  exports akka.http.impl.engine.parsing;
  exports akka.http.impl.engine.rendering;
  exports akka.http.impl.engine.server;
  exports akka.http.impl.engine.ws;
  exports akka.http.impl.model;
  exports akka.http.impl.model.parser;
  exports akka.http.impl.settings;
  exports akka.http.impl.util;
  exports akka.http.javadsl;
  exports akka.http.javadsl.model;
  exports akka.http.javadsl.model.headers;
  exports akka.http.javadsl.model.sse;
  exports akka.http.javadsl.model.ws;
  exports akka.http.javadsl.settings;
  exports akka.http.scaladsl;
  exports akka.http.scaladsl.model;
  exports akka.http.scaladsl.model.headers;
  exports akka.http.scaladsl.model.http2;
  exports akka.http.scaladsl.model.sse;
  exports akka.http.scaladsl.model.ws;
  exports akka.http.scaladsl.settings;
  exports akka.http.scaladsl.util;
  exports akka.http.shaded.com.twitter.hpack;

  // akka-http
  exports akka.http.javadsl.coding;
  exports akka.http.javadsl.common;
  exports akka.http.javadsl.marshalling;
  exports akka.http.javadsl.marshalling.sse;
  exports akka.http.javadsl.server;
  exports akka.http.javadsl.server.directives;
  exports akka.http.javadsl.unmarshalling;
  exports akka.http.javadsl.unmarshalling.sse;
  exports akka.http.scaladsl.client;
  exports akka.http.scaladsl.coding;
  exports akka.http.scaladsl.common;
  exports akka.http.scaladsl.marshalling;
  exports akka.http.scaladsl.marshalling.sse;
  exports akka.http.scaladsl.server;
  exports akka.http.scaladsl.server.directives;
  exports akka.http.scaladsl.server.util;
  exports akka.http.scaladsl.unmarshalling;
  exports akka.http.scaladsl.unmarshalling.sse;

  // akka-http-spray-json_2.13:10.2.10
  exports akka.http.scaladsl.marshallers.sprayjson;

  // akka-parsing
  exports akka.http.ccompat;
  exports akka.macros;
  exports akka.parboiled2;
  exports akka.parboiled2.support;
  exports akka.parboiled2.util;
  exports akka.shapeless;
  exports akka.shapeless.ops;
  exports akka.shapeless.syntax;

  // akka-protobuf-v3
  exports akka.protobufv3.internal;
  exports akka.protobufv3.internal.compiler;

  // akka-slf4j
  exports akka.event.slf4j;

  // akka-stream
  exports akka.stream;
  exports akka.stream.impl;
  exports akka.stream.impl.fusing;
  exports akka.stream.impl.io;
  exports akka.stream.impl.io.compression;
  exports akka.stream.impl.streamref;
  exports akka.stream.javadsl;
  exports akka.stream.scaladsl;
  exports akka.stream.serialization;
  exports akka.stream.snapshot;
  exports akka.stream.stage;
  exports com.typesafe.sslconfig.akka;
  exports com.typesafe.sslconfig.akka.util;

  // io.spray:spray-json_2.13
  exports spray.json;

  // scala-lang.modules:scala-arser-combinators_2.13:1.1.2
  // Loaded by classes from com.typesafe.sslconfig.ssl.*
  exports scala.util.parsing.combinator;
  exports scala.util.parsing.combinator.lexical;
  exports scala.util.parsing.combinator.syntactical;
  exports scala.util.parsing.combinator.token;
  exports scala.util.parsing.input;
  exports scala.util.parsing.json;
}
