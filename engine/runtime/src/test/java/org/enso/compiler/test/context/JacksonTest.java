package org.enso.compiler.test.context;

import java.util.List;

import org.enso.polyglot.Suggestion;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;
import org.junit.Test;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.module.scala.DefaultScalaModule;

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
    } else {
      fail("Expecting Suggestion.Module: " + suggestion);
    }
  }

  @Test
  public void testRecordSerdeOfSuggestion() throws Exception {
    Object shape = new SuggestionCache(11, List.of(new Suggestion.Module(
      "SampleModule",
      Option.apply("doc"),
      Option.apply("html"),
      Option.empty(),
      Option.empty()
    )));
    final ObjectMapper m = new ObjectMapper().registerModule(new DefaultScalaModule());
    String result = m
            .writerWithDefaultPrettyPrinter()
            .writeValueAsString(shape);

    var cache = (SuggestionCache) m.readerFor(SuggestionCache.class).readValue(result);
    assertEquals("One suggestion", 1, cache.suggestions.size());
    if (cache.suggestions().get(0) instanceof Suggestion.Module module) {
      assertEquals("SampleModule", module.name());
      assertEquals("doc", module.documentation().get());
    } else {
      fail("Expecting Suggestion.Module: " + cache);
    }
  }

  public record SuggestionCache(
    @JsonProperty("version") int version,
    @JsonProperty("suggestions") List<Suggestion> suggestions
  ) {
  }
}
