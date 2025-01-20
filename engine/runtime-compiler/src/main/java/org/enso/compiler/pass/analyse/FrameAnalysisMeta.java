package org.enso.compiler.pass.analyse;

import org.enso.compiler.core.ir.ProcessingPass;

public sealed interface FrameAnalysisMeta extends ProcessingPass.Metadata
    permits FramePointer, FrameVariableNames {}
