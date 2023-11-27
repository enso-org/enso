# AST

The goals of the AST are to:

- Make expression edits as simple as possible.
- Support our synchronization strategy.

## Nodes

A node has an ExprId and other metadata, type-specific information, and a sequence of children. Children can be node references or strings. String children are the tokens of the frontend AST. For example, an identifier node has a single string child; a parser "macro" contains strings for its reserved words and and expression references for its general expressions.

Only nodes have ExprId assignments and thus are able to receive type info from the backend. Some things that are only tokens in `enso-parser` have nodes in the expression tree, e.g. operator symbols.

### Referential transparency

Nodes are not dependent on any auxiliary data structure. This allows them to be easily created and rearranged.

## The AST is abstract

`Tree` is concrete; this is important for preserving information such as exact whitespace characters, but it is not conducive to editing. The GUI's AST is abstract. Avoiding redundant information allows application logic to perform edits without needing to maintain unnecessary invariants. When ASTs are printed to text, any tokens needed to represent the structure are materialized (e.g. parentheses are inserted as needed.) During this printing process, the `Tree`s corresponding to preexisting AST nodes are consulted, in order to ensure that the concrete syntax of unmodified parts of the output matches the input. This is the only use of `Tree` in the frontend outside of the process of translating from `Tree` to frontend AST.

## AST graph

Nodes are stored in a map indexed by internal IDs.

The internal IDs are distinct from ExprIds because their consistency is driven by different concerns: E.g. when the frontend is modifying a value atomically, it does so by replacing a child expression [more on this in the synchronization documentation, to be written]; however the backend can reevaluate more efficiently if this operation doesn't change the ExprId, so the new node should have the same ExprID as the node it replaces.

## Executing edits

### Dirty-node overlay

There are two maps holding nodes: A primary map contains committed nodes. A dirty-node overlay map contains new or modified nodes. Application logic performs edits by inserting (or modifying) entries in the dirty-node map. When resolving a node reference, the overlay map is always checked first.

Dirty nodes are ephemeral; they can only be committed in the same frame that they are created. This avoids a tree-edit becoming outdated by remote changes; all conflict resolution is the responsibility of the Yjs CDRT.

### Print-parse roundtripping

It is important that the AST is always consistent with the AST that would result from parsing the corresponding source code. To ensure this, after executing the changes of an edit but before committing them, the modified subtree(s) of the AST are printed and reparsed.

AST-printing produces two outputs: Source code, and an ID map attaching identities to spans. This map is read when translating the parsed `Tree`s to AST nodes, and used to reattach IDs. In addition to spans it stores node-type information, which is used to disambiguate nodes that
contain the same code (i.e. when a node contains one other node as a child and has no other tokens or whitespace). Update operations on the map are not needed; the map is written once while printing, and read once while translating.

### Updating the committed-node map

The primary map is only updated in response to Yjs events. When an edit is completed, all the nodes in the overlay map are used to produce modifications to the Yjs data structures, and the overlay is cleared. By using the Yjs notifications even for local changes, we avoid needing separate tree-update mechanisms for local and remote changes.

[In the initial implementation, until the new AST is used for synchronization, local edits can be committed by simply moving dirty nodes into the committed node map. Remote edits can recreate the committed-node map by parsing the new code.]

###################

# Synchronization

Yjs representation of tree data

blocks are arrays
expressions are maps { value, metadata }
everything else is atomic (JSON/string)
parents & children
Y.Text leafs, some atomic changes

# Deltas

codemirror: deltas+attributes
raw text: if we save snapshots we can recover from edits much better...

###############

We should ensure when concurrent edits occur, the outcome is:

- The edits are merged in a way that is syntactically-consistent and widget-respecting.
  _Syntactically consistent_:
  Nearby structural edits should not be combined to yield shapes not present in the result of either edit--
  i.e. if two clients add an argument to a function call at the same time,
  the edits should not be merged to yield a call with two arguments.
  _Widget respecting_:
  When a node is edited by a widget, i.e. a dropdown, a number slider, etc.,
  the value edit should not merge with any concurrent edits--
  it should be atomically either observed or replaced by the other edit.
- If that is not possible, one of the edits is rejected.

With the right synchronized data model,
most edit conflicts will be naturally resolved in reasonable ways by the CRDT.

Synchronize node graph.

- Not text.
  - 6 months ago, the IDE was an enhanced text editor;
    it helped the user edit the top-level expression of an assignment,
    as text with associated information.
  - The paradigm changed in GUI1; we now edit a tree of widgets.
  - This is the same paradigm we want to use in GUI2.
  - For an IDE that operates on trees,
    using text as the canonical representation has a high complexity cost.
    - Text-based conflict resolution is syntax-breaking;
      tree-compatible text synchronization would increase the required complexity further.
- Sea of nodes resolves conflicts well.

Codemirror: Reparse, and use tree-delta.

- Although the editor operates on "text", that doesn't make it easier to use text as the sync representation:
  - We have to update the ID map for edits.
  - Synchronizing with a Y.Text gives us tree-breaking conflict resolution.

Translate syntax tree to GUI internal tree

- What the GUI cares about is quite different from the syntax.
  - We can abstract away many syntactic distinctions.
  - Perform analysis once upfront (alias analysis, qualified names, ...)
  - Code references in GUI tree: design for editability.
    - Nodes own strings for their _directly-owned_ content.
      - Location info and code-including-children references conflict with editability.
    - When we need document locations, they are derived by recursive node printing/recursive length-summing.
      - We can use a cache to make this cheap.
      - `line:col`-based locations are easy to invalidate and cheap to update for the kinds of edits we perform.
- Parser output is used only as an input to translation;
  node edits occur on the GUI tree.
  - (We should ensure print/parse/translate is a round-trip after any edit)
    - But this requirement is not introduced by translation;
      there are already many ways a `Tree` could be edited that wouldn't print/parse-roundtrip.
- Our lazy-deserialization model supports this efficiently:
  - If we ensure the lazy-tree objects never escape,
    the JIT should be able to avoid heap-allocating them.
  - There is other analysis we need to perform anyway (at least AA);
    if we do this during translation,
    translation doesn't require any additional pass over the data.
