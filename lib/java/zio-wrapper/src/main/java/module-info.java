module org.enso.zio.wrapper {
  requires scala.library;

  // dev.zio:zio_2.13:2.0.14
  // dev.zio:zio-internal-macros_2.13:2.0.14
  exports zio;
  exports zio.internal;
  exports zio.internal.macros;
  exports zio.internal.metrics;
  exports zio.interop;
  exports zio.metrics;
  exports zio.metrics.jvm;
  exports zio.stm;

  // dev.zio:zio-0stacktracer_2.13:2.0.14
  exports zio.internal.stacktracer;
  exports zio.stacktracer;

  // dev.zio:zio-interop-cats_2.13:23.0.0.6
  // exports zio.interop.console;
  // exports zio.interop.stm;
  // exports zio.stream.interop;

  // dev.zio:izumi-reflect_2.13:2.3.8
  exports izumi.reflect;
  exports izumi.reflect.internal;
  exports izumi.reflect.internal.fundamentals.collections;
  exports izumi.reflect.internal.fundamentals.functional;
  exports izumi.reflect.internal.fundamentals.platform.assertions;
  exports izumi.reflect.internal.fundamentals.platform.basics;
  exports izumi.reflect.internal.fundamentals.platform.console;
  exports izumi.reflect.internal.fundamentals.platform.language;
  exports izumi.reflect.internal.fundamentals.platform.strings;
  exports izumi.reflect.macrortti;

  // dev.zio:izumi-reflect-thirdparty-boopickle-shaded_2.13:2.3.8
  exports izumi.reflect.thirdparty.internal.boopickle;
}
