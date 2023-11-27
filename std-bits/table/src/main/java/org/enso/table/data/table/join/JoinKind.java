package org.enso.table.data.table.join;

public enum JoinKind {
  INNER,
  FULL,
  LEFT_OUTER,
  RIGHT_OUTER,
  LEFT_ANTI,
  RIGHT_ANTI;
  
  public static JoinResult.BuilderSettings makeSettings(JoinKind joinKind) {
    return switch (joinKind) {
      case INNER -> new JoinResult.BuilderSettings(true, false, false);
      case FULL -> new JoinResult.BuilderSettings(true, true, true);
      case LEFT_OUTER -> new JoinResult.BuilderSettings(true, true, false);
      case RIGHT_OUTER -> new JoinResult.BuilderSettings(true, false, true);
      case LEFT_ANTI -> new JoinResult.BuilderSettings(false, true, false);
      case RIGHT_ANTI -> new JoinResult.BuilderSettings(false, false, true);
    };
  }
}
