package org.enso.interpreter.bench.result;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;

public record Results(
    @JsonProperty("$schema") URI schema, Configuration configuration, Object ghActionRun, List<Result> results) {

  public static Results createEmpty(URI schemaUri) {
    var conf = Configuration.fromSystemProperties();
    return new Results(schemaUri, conf, null, new ArrayList<>());
  }
}
