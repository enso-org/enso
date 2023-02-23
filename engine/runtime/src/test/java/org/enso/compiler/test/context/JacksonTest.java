package org.enso.compiler.test.context;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.module.scala.DefaultScalaModule;
import org.enso.polyglot.Suggestion;
import static org.junit.Assert.assertEquals;

import org.junit.Test;
import scala.Option;
import scala.collection.immutable.Seq;

public class JacksonTest {

  @Test
  public void testSerdeOfSuggestion() throws Exception {
    Object shape = new Suggestion.Module(
            "SampleModule",
            Option.apply("doc"),
            Option.apply("html"),
            Option.empty(),
            Option.empty()
    );
    final ObjectMapper m = new ObjectMapper().registerModule(new DefaultScalaModule());
    String result = m
            .writerWithDefaultPrettyPrinter()
            .writeValueAsString(shape);

    Suggestion suggestion = m.readerFor(Suggestion.class).readValue(result);
    assertEquals("SampleModule", suggestion.name());
    assertEquals("doc", suggestion.documentation().get());
    assertEquals(Suggestion.Module.class, suggestion.getClass());
  }

  @Test
  public void testArraySerdeOfSuggestion() throws Exception {
    Object shape = new Suggestion[]{new Suggestion.Module(
      "SampleModule",
      Option.apply("doc"),
      Option.apply("html"),
      Option.empty(),
      Option.empty()
      )};
    final ObjectMapper m = new ObjectMapper().registerModule(new DefaultScalaModule());
    String result = m
            .writerWithDefaultPrettyPrinter()
            .writeValueAsString(shape);

    var it = m.readerFor(Suggestion.class).readValues(result);
    var suggestion = it.nextValue();
    assertEquals(Suggestion.Module.class, suggestion.getClass());
    if (suggestion instanceof Suggestion.Module module) {
      assertEquals("SampleModule", module.name());
      assertEquals("doc", module.documentation().get());
    }
  }
}
