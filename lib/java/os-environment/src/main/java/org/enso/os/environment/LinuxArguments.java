package org.enso.os.environment;

final class LinuxArguments implements Arguments {
  static final LinuxArguments INSTANCE = new LinuxArguments();

  private LinuxArguments() {}

  @Override
  public String[] alterArgs(String[] originalArgs) {
    return originalArgs;
  }
}
