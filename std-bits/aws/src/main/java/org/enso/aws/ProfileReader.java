package org.enso.aws;

import software.amazon.awssdk.profiles.ProfileFile;
import software.amazon.awssdk.profiles.ProfileFileSupplier;

public class ProfileReader {
  public static final ProfileReader INSTANCE = new ProfileReader();

  private ProfileFileSupplier provider = null;
  private ProfileFile profileFile = null;
  private String[] profiles = null;

  private ProfileReader() {}

  public String[] getProfiles() {
    if (provider == null) {
      provider = ProfileFileSupplier.defaultSupplier();
    }

    if (profiles == null || !provider.get().equals(profileFile)) {
      var profileFile = provider.get();
      profiles = profileFile.profiles().keySet().toArray(new String[0]);
    }

    return profiles;
  }
}
