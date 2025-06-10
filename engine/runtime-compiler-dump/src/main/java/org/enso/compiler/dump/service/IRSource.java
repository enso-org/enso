package org.enso.compiler.dump.service;

import java.net.URI;
import java.util.function.Function;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.IdentifiedLocation;

/** Information about an IR dumping. */
public record IRSource<AnIR extends IR>(
    AnIR ir,
    String name,
    String afterPass,
    URI loc,
    Function<IdentifiedLocation, Integer> lineMap) {}
