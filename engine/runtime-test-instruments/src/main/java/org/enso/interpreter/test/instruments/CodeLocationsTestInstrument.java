package org.enso.interpreter.test.instruments;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.EventBinding;
import com.oracle.truffle.api.instrumentation.EventContext;
import com.oracle.truffle.api.instrumentation.ExecutionEventListener;
import com.oracle.truffle.api.instrumentation.SourceFilter;
import com.oracle.truffle.api.instrumentation.SourceSectionFilter;
import com.oracle.truffle.api.instrumentation.TruffleInstrument;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.source.SourceSection;
import java.util.LinkedHashSet;
import java.util.Set;

/**
 * A debug instrument used to test code locations.
 *
 * <p>Allows to listen for a node with a given type at a given position, and later verify if such a
 * node was indeed encountered in the course of execution.
 */
@TruffleInstrument.Registration(
    id = CodeLocationsTestInstrument.INSTRUMENT_ID,
    services = CodeLocationsTestInstrument.class)
public class CodeLocationsTestInstrument extends TruffleInstrument {
  public static final String INSTRUMENT_ID = "locations-test";
  private Env env;

  /**
   * Initializes the instrument. Substitute for a constructor, called by the Truffle framework.
   *
   * @param env the instrumentation environment
   */
  @Override
  protected void onCreate(Env env) {
    env.registerService(this);
    this.env = env;
  }

  /**
   * An event listener implementing the behavior of verifying whether the currently executed node is
   * the one expected by the user.
   */
  public static class LocationsEventListener implements ExecutionEventListener {
    private boolean successful = false;
    private final int start;
    private final int diff;
    private final int length;
    private final int lengthDiff;
    private final Class<?> type;
    private final Set<SourceSection> close = new LinkedHashSet<>();

    private LocationsEventListener(int start, int diff, int length, int lengthDiff, Class<?> type) {
      this.start = start;
      this.diff = diff;
      this.length = length;
      this.lengthDiff = lengthDiff;
      this.type = type;
    }

    /**
     * Get the start location of the nodes expected by this listener.
     *
     * @return the start location for this listener
     */
    public int getStart() {
      return start;
    }

    /**
     * Get the source length of the nodes expected by this listener.
     *
     * @return the source length for this listener
     */
    public int getLength() {
      return length;
    }

    /**
     * Get the type of nodes expected by this listener.
     *
     * @return the node type for this listener
     */
    public Class<?> getType() {
      return type;
    }

    /**
     * Was a node with parameters specified for this listener encountered in the course of
     * execution?
     *
     * @return {@code true} if the requested node was observed, {@code false} otherwise
     */
    public boolean isSuccessful() {
      return successful;
    }

    /**
     * List collected {@link SourceSection} instances that were close to searched for location, but
     * not fully right.
     *
     * @return textual dump of those sections
     */
    public String dumpCloseSections() {
      var sb = new StringBuilder();
      for (var s : close) {
        sb.append("\n").append(s.toString());
      }
      return sb.toString();
    }

    /**
     * Checks if the node to be executed is the node this listener was created to observe.
     *
     * @param context current execution context
     * @param frame current execution frame
     */
    @Override
    public void onEnter(EventContext context, VirtualFrame frame) {
      if (successful) {
        return;
      }
      Node node = context.getInstrumentedNode();
      if (!type.isInstance(node)) {
        return;
      }
      SourceSection section = node.getSourceSection();
      if (section == null || !section.hasCharIndex()) {
        return;
      }
      if (Math.abs(section.getCharIndex() - start) <= diff
          && Math.abs(section.getCharLength() - length) <= lengthDiff) {
        successful = true;
      } else {
        close.add(section);
      }
    }

    @Override
    public void onReturnValue(EventContext context, VirtualFrame frame, Object result) {}

    @Override
    public void onReturnExceptional(
        EventContext context, VirtualFrame frame, Throwable exception) {}
  }

  /**
   * Attach a new listener to observe nodes with given parameters.
   *
   * @param sourceStart the source start location of the expected node
   * @param diff acceptable diff
   * @param length the source length of the expected node
   * @param type the type of the expected node
   * @return a reference to attached event listener
   */
  public EventBinding<LocationsEventListener> bindTo(
      int sourceStart, int diff, int length, int lengthDiff, Class<?> type) {
    var testSource = SourceFilter.newBuilder().sourceIs((t) -> t.getName().equals("Test")).build();
    return env.getInstrumenter()
        .attachExecutionEventListener(
            SourceSectionFilter.newBuilder()
                .sourceFilter(testSource)
                .indexIn(sourceStart, length)
                .build(),
            new LocationsEventListener(sourceStart, diff, length, lengthDiff, type));
  }
}
