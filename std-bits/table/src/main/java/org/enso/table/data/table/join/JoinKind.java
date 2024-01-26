package org.enso.table.data.table.join;

public enum JoinKind {
  INNER(true, false, false),
  FULL(true, true, true),
  LEFT_OUTER(true, true, false),
  RIGHT_OUTER(true, false, true),
  LEFT_ANTI(false, true, false),
  RIGHT_ANTI(false, false, true);

  public final boolean wantsCommon;
  public final boolean wantsLeftUnmatched;
  public final boolean wantsRightUnmatched;

  private JoinKind(boolean wantsCommon, boolean wantsLeftUnmatched, boolean wantsRightUnmatched) {
    this.wantsCommon = wantsCommon;
    this.wantsLeftUnmatched = wantsLeftUnmatched;
    this.wantsRightUnmatched = wantsRightUnmatched;
  }
}
