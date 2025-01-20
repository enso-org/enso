package org.enso.shttp.cloud_mock;

import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.Deque;
import java.util.LinkedList;
import java.util.List;

public class AssetStore {
  static final String HOME_DIRECTORY_ID = "directory-27xJM00p8jWoL2qByTo6tQfciWC";
  static final String USERS_DIRECTORY_ID = "directory-27xJM00p8jWoL2qByTo6tQfciBB";
  static final String ROOT_DIRECTORY_ID = "directory-27xJM00p8jWoL2qByTo6tQfciAA";
  private final List<Secret> secrets = new LinkedList<>();

  final Directory rootDirectory;
  final Directory homeDirectory;

  public AssetStore() {
    rootDirectory =
        new Directory(ROOT_DIRECTORY_ID, "", null, currentTimeAsString(), new LinkedList<>());
    Directory usersDirectory =
        new Directory(
            USERS_DIRECTORY_ID,
            "Users",
            rootDirectory.id,
            currentTimeAsString(),
            new LinkedList<>());
    homeDirectory =
        new Directory(
            HOME_DIRECTORY_ID,
            "My test User 1",
            usersDirectory.id,
            currentTimeAsString(),
            new LinkedList<>());

    rootDirectory.children.add(usersDirectory);
    usersDirectory.children.add(homeDirectory);
  }

  String createSecret(String parentDirectoryId, String title, String value) {
    if (!parentDirectoryId.equals(HOME_DIRECTORY_ID)) {
      throw new IllegalArgumentException(
          "In Cloud Mock secrets can only be created in the root directory");
    }

    if (exists(parentDirectoryId, title)) {
      throw new IllegalArgumentException(
          "Secret with title " + title + " already exists in the directory");
    }

    String id = "secret-" + java.util.UUID.randomUUID().toString();
    secrets.add(new Secret(id, title, value, parentDirectoryId, currentTimeAsString()));
    return id;
  }

  boolean exists(String parentDirectoryId, String title) {
    return secrets.stream()
        .anyMatch(
            secret ->
                secret.title.equals(title) && secret.parentDirectoryId.equals(parentDirectoryId));
  }

  boolean deleteSecret(String id) {
    return secrets.removeIf(secret -> secret.id.equals(id));
  }

  Secret findSecretById(String id) {
    return secrets.stream().filter(secret -> secret.id.equals(id)).findFirst().orElse(null);
  }

  List<Secret> listAssets(String parentDirectoryId) {
    if (!parentDirectoryId.equals(HOME_DIRECTORY_ID)) {
      throw new IllegalArgumentException(
          "In Cloud Mock secrets can only be listed in the root directory");
    }

    return List.copyOf(secrets);
  }

  /**
   * We do not store the creation time as ZoneDateTime, because that causes trouble with
   * serialization to JSON - we'd need to add more dependencies to handle it. Instead, since this is
   * just a mock, we store as raw strings.
   */
  private String currentTimeAsString() {
    return ZonedDateTime.now()
        .withZoneSameInstant(ZoneId.of("UTC"))
        .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME);
  }

  Asset resolvePath(String[] path) {
    Deque<String> pathSegments = new LinkedList<>(Arrays.asList(path));
    Directory currentDirectory = rootDirectory;
    Asset currentAsset = currentDirectory.asAsset();

    while (!pathSegments.isEmpty()) {
      String nextSegment = pathSegments.poll();
      if (currentDirectory == null) {
        throw new IllegalArgumentException(
            "The path references a subdirectory of an asset that is not a directory");
      }

      var nextDirectory =
          currentDirectory.children.stream()
              .filter(directory -> directory.title.equals(nextSegment))
              .findFirst()
              .orElse(null);
      if (nextDirectory != null) {
        // Enter the subdirectory
        currentDirectory = nextDirectory;
        currentAsset = currentDirectory.asAsset();
      } else {
        // Otherwise, start looking for secrets
        final var currentDirectoryId = currentDirectory.id;
        var nextSecret =
            secrets.stream()
                .filter(
                    secret ->
                        secret.title.equals(nextSegment)
                            && secret.parentDirectoryId.equals(currentDirectoryId))
                .findFirst()
                .orElse(null);
        if (nextSecret != null) {
          // Found a secret, mark it for return.
          currentAsset = nextSecret.asAsset();
          // But if further path segments are encountered - we will crash.
          currentDirectory = null;
        } else {
          return null;
        }
      }
    }

    return currentAsset;
  }

  record Secret(
      String id, String title, String value, String parentDirectoryId, String modifiedAt) {
    Asset asAsset() {
      return new Asset(id, title, parentDirectoryId, modifiedAt);
    }
  }

  public record Asset(String id, String title, String parentId, String modifiedAt) {}

  record Directory(
      String id, String title, String parentId, String modifiedAt, LinkedList<Directory> children) {
    Asset asAsset() {
      return new Asset(id, title, parentId, modifiedAt);
    }
  }
}
