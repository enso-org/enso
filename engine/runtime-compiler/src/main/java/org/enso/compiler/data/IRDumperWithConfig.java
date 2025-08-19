package org.enso.compiler.data;

import org.enso.compiler.dump.service.IRDumper;

public record IRDumperWithConfig(IRDumper irDumper, IRDumperConfig config) {}
