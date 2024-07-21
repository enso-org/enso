package org.enso.desktopenvironment;

final class TrashFactory {

  private static final class LazyTrash {
    static final Trash INSTANCE =
        switch (Platform.getOperatingSystem()) {
          case Platform.OS.LINUX -> new LinuxTrash();
          case Platform.OS.MACOS, Platform.OS.WINDOWS -> new JnaTrash();
        };
  }

  private TrashFactory() {}

  public static Trash getInstance() {
    return LazyTrash.INSTANCE;
  }
}
