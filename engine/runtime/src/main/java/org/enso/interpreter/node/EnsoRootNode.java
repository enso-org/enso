package org.enso.interpreter.node;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.FrameSlotKind;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;
import java.util.Objects;
import java.util.function.Supplier;
import org.enso.compiler.context.LocalScope;
import org.enso.compiler.core.ir.Location;
import org.enso.interpreter.EnsoLanguage;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.scope.ModuleScope;
import org.enso.interpreter.runtime.util.CachingSupplier;

/** A common base class for all kinds of root node in Enso. */
@NodeInfo(shortName = "Root", description = "A root node for Enso computations")
public abstract class EnsoRootNode extends RootNode {
  private final String name;
  private final int sourceStartIndex;
  private final int sourceLength;
  private final LocalScope localScope;
  private final ModuleScope moduleScope;
  private final CachingSupplier<Source> source;

  /**
   * Constructs the root node.
   *
   * @param language the language instance in which this will execute
   * @param localScope a reference to the construct local scope
   * @param moduleScope a reference to the construct module scope. May be {@code null}.
   * @param name the name of the construct
   * @param sourceSupplier a reference to the source code being executed. May be {@code null}.
   * @param location in the (to be supplied) source. May be {@code null}
   */
  protected EnsoRootNode(
      EnsoLanguage language,
      LocalScope localScope,
      ModuleScope moduleScope,
      String name,
      Supplier<Source> sourceSupplier,
      Location location) {
    super(language, buildFrameDescriptor(name, localScope));
    Objects.requireNonNull(language);
    Objects.requireNonNull(localScope);
    Objects.requireNonNull(moduleScope);
    this.name = name;
    this.localScope = localScope;
    this.moduleScope = moduleScope;
    this.source = sourceSupplier == null ? null : CachingSupplier.wrap(sourceSupplier);
    this.sourceStartIndex = location == null ? NO_SOURCE : location.start();
    this.sourceLength = location == null ? NO_SOURCE : location.length();
  }

  /**
   * Builds a {@link FrameDescriptor} from the alias analysis scope metadata for the local scope.
   * See [[AliasAnalysis.Graph.Scope.allDefinitions]].
   *
   * @return {@link FrameDescriptor} built from the variable definitions in the local localScope.
   */
  private static FrameDescriptor buildFrameDescriptor(String name, LocalScope localScope) {
    var descriptorBuilder = FrameDescriptor.newBuilder();
    descriptorBuilder.addSlot(FrameSlotKind.Object, LocalScope.monadicStateSlotName(), null);

    var allDefs = localScope.allSymbols(name, null);
    for (var definition : allDefs) {
      descriptorBuilder.addSlot(FrameSlotKind.Illegal, definition, null);
    }
    descriptorBuilder.defaultValue(DataflowError.UNINITIALIZED);
    var frameDescriptor = descriptorBuilder.build();
    return frameDescriptor;
  }

  /**
   * Gets a reference to the language context associated with this program.
   *
   * @return a reference to the language context
   */
  public EnsoContext getContext() {
    return EnsoContext.get(this);
  }

  /**
   * Creates a string representation of this node.
   *
   * @return a string representation of the node
   */
  @Override
  public String toString() {
    return this.name;
  }

  /**
   * Returns a language specific name of this node.
   *
   * @return a name of this node
   */
  @Override
  public String getName() {
    return this.name;
  }

  /**
   * Gets the source code represented by this node.
   *
   * @return a reference to the source code
   */
  @Override
  public SourceSection getSourceSection() {
    return findSourceSection(this, sourceStartIndex, sourceLength);
  }

  static final int NO_SOURCE = -1;

  @TruffleBoundary
  static SourceSection findSourceSection(final RootNode n, int sourceStartIndex, int sourceLength) {
    if (sourceStartIndex != NO_SOURCE && n instanceof EnsoRootNode rootNode) {
      if (rootNode.source == null) {
        return null;
      }
      var src = rootNode.source.get();
      assert src != null;
      var module = rootNode.getModuleScope().getModule();
      var ownedByModule = module.isModuleSource(src);
      if (ownedByModule) {
        // ask the module for a (patched) section
        return module.createSection(sourceStartIndex, sourceLength);
      } else {
        // ask the source itself
        return src.createSection(sourceStartIndex, sourceLength);
      }
    }
    return null;
  }

  /**
   * Gets the local scope this node expects to work with
   *
   * @return the local scope for this node
   */
  public LocalScope getLocalScope() {
    return localScope;
  }

  /**
   * Gets the module scope this node was defined with
   *
   * @return the module scope for this node
   */
  public ModuleScope getModuleScope() {
    return moduleScope;
  }

  /**
   * Marks this node as instrumentable by Truffle instruments API.
   *
   * @return {@code true}
   */
  @Override
  protected boolean isInstrumentable() {
    return true;
  }

  /**
   * Marks the node as cloneable for runtime splitting purposes.
   *
   * @return always true
   */
  @Override
  public boolean isCloningAllowed() {
    return true;
  }
}
