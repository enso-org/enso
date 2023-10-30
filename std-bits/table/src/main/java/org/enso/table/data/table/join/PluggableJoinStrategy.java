package org.enso.table.data.table.join;

/**
 * A {@link JoinStrategy} that can also be used within another join strategy to perform a join of sub-sets of indices,
 * stemming from already joining on other conditions.
 */
public interface PluggableJoinStrategy extends JoinStrategy {

  // TODO
}
