package org.enso.aws;

import software.amazon.awssdk.profiles.ProfileFileSupplier;

public class ProfileReader {
  public static final ProfileReader INSTANCE = new ProfileReader();

  private String[] profiles = null;

  private ProfileReader() {}

  public String[] getProfiles() {
    if (profiles == null) {
      var provider = ProfileFileSupplier.defaultSupplier();
      var profileFile = provider.get();
      profiles = profileFile.profiles().keySet().toArray(new String[0]);
    }

    return profiles;
  }
}
