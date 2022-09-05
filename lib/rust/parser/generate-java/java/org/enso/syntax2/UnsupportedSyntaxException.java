package org.enso.syntax2;

public final class UnsupportedSyntaxException extends Exception {
    private final Tree tree;

    UnsupportedSyntaxException(Tree treeIn) {
        super("Tree contains unsupported syntax. Details are in an `Unsupported` node in the tree.");
        tree = treeIn;
    }

    public Tree getTree() {
        return tree;
    }
}
