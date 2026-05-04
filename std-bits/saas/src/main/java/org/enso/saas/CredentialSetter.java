package org.enso.saas;

import org.apache.commons.mail.DefaultAuthenticator;
import org.apache.commons.mail.HtmlEmail;
import org.enso.base.enso_cloud.EnsoHideableValue;
import org.enso.base.enso_cloud.ExternalLibrarySecretHelper;

/**
 * A helper class to set email credentials from secrets.
 *
 * <p>This class is allowed access to secrets. Extra care should be taken to ensure its result is
 * not leaked.
 */
public final class CredentialSetter {
  public static void setAuthenticator(
      HtmlEmail email, String username, EnsoHideableValue password) {
    email.setAuthenticator(new DefaultAuthenticator(username, unsafeResolveSecrets(password)));
  }

  /**
   * This function is allowed access to secrets. Extra care should be taken to ensure its result is
   * not leaked.
   */
  private static String unsafeResolveSecrets(EnsoHideableValue value) {
    return ExternalLibrarySecretHelper.resolveValue(value);
  }
}
