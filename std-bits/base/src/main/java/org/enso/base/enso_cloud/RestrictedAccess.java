package org.enso.base.enso_cloud;

import java.util.List;
import java.util.Optional;

final class RestrictedAccess {
  record AccessLocation(String className, String method) {}

  /**
   * Checks the current stack trace to find the caller and checks if it is one of the allowed
   * locations.
   *
   * <p>This is a very rudimentary approach to the access control, and it is not very extensible, as
   * it requires updating std-base whenever a new library that needs access to secrets is added.
   * However, it seems like the best simple solution for now.
   *
   * <p>Later we may want to replace it with some other solution, e.g. a key that trusted libraries
   * will use to 'sign' their class name, proving that they can be trusted, without the need to
   * update std-base whenever a new library is added.
   */
  static void checkAccess(List<AccessLocation> allowedAccessLocations)
      throws EnsoSecretAccessDenied {
    var accessLocation =
        StackWalker.getInstance()
            .walk(
                (stackFrameStream -> {
                  Optional<StackWalker.StackFrame> firstClientFrame =
                      stackFrameStream.skip(2).findFirst();
                  if (firstClientFrame.isEmpty()) {
                    throw new IllegalStateException("Unable to find client frame.");
                  }

                  var frame = firstClientFrame.get();
                  return new AccessLocation(frame.getClassName(), frame.getMethodName());
                }));

    boolean isAllowed = allowedAccessLocations.contains(accessLocation);
    if (!isAllowed) {
      throw new EnsoSecretAccessDenied();
    }
  }
}
