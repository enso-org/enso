package org.enso.compiler.test.context;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.module.scala.DefaultScalaModule;
import org.enso.polyglot.Suggestion;
import static org.junit.Assert.assertEquals;

import org.junit.Test;
import scala.Option;

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
}
