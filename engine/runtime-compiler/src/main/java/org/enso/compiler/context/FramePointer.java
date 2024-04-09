package org.enso.compiler.context;

/**
 * A representation of a pointer into a stack frame at a given number of levels above the current.
 */
public record FramePointer(int parentLevel, int frameSlotIdx) {}
