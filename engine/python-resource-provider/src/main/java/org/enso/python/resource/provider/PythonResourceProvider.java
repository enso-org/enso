package org.enso.python.resource.provider;

import com.oracle.truffle.api.provider.InternalResourceProvider;
import java.nio.file.Path;

/**
 * A dummy {@link InternalResourceProvider} replacement for <a
 * href="https://github.com/oracle/graalpython/blob/763a4bdf04858dccaa5b82cc3b8bb9cba2a4ea68/graalpython/com.oracle.graal.python.resources/src/com/oracle/graal/python/resources/PythonResource.java#L4">GraalPython
 * resource provider</a>.
 *
 * <h2>How does Python resource extraction, and resolution work in Enso?</h2>
 *
 * <ul>
 *   <li>We unpack Python resources from {@code python-resource.jar} during build time, into a
 *       specific directory.
 *   <li>{@code python-resource.jar} is not present on the module path during runtime.
 *   <li>{@link InternalResourceProvider resource provider} for Python is normally located inside
 *       {@code python-resource.jar} file.
 *   <li>Without this {@link PythonResourceProvider} class, the Truffle engine would not be able to
 *       find the Python resources, even if this class is practically empty.
 * </ul>
 *
 * <p>Note that for this to work, we need to set {@code polyglot.engine.resourcePath.python} system
 * property to the directory with Python resources before {@link org.graalvm.polyglot.Context} is
 * created.
 *
 * @see com.oracle.truffle.api.TruffleLanguage.Env#getInternalResource(String)
 * @see org.graalvm.polyglot.Engine#copyResources(Path, String...)
 */
public final class PythonResourceProvider extends InternalResourceProvider {
  private static final String ERR_MSG =
      "Should not be called. Python resources should already be unpacked during " + "build time.";

  @Override
  protected String getComponentId() {
    return "python";
  }

  @Override
  protected String getResourceId() {
    return "python-home";
  }

  @Override
  protected Object createInternalResource() {
    throw new IllegalStateException(ERR_MSG);
  }
}
