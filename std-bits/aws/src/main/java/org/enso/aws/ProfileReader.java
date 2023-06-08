package org.enso.aws;

import software.amazon.awssdk.profiles.ProfileFileSupplier;

public class ProfileReader {
  public static final ProfileReader INSTANCE = new ProfileReader();

  private final String[] profiles;

  private ProfileReader() {

    var pvdr = ProfileFileSupplier.defaultSupplier();
    var profileFile = pvdr.get();
    profiles = profileFile.profiles().keySet().toArray(new String[0]);
  }

  public String[] getProfiles() {
    return profiles;
  }
}
