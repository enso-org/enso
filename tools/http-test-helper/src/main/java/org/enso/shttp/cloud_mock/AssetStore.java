package org.enso.shttp.cloud_mock;

import java.util.LinkedList;
import java.util.List;

public class AssetStore {
  static final String ROOT_DIRECTORY_ID = "directory-27xJM00p8jWoL2qByTo6tQfciWC";
  private final List<Secret> secrets = new LinkedList<>();

  String createSecret(String parentDirectoryId, String title, String value) {
    if (!parentDirectoryId.equals(ROOT_DIRECTORY_ID)) {
      throw new IllegalArgumentException(
          "In Cloud Mock secrets can only be created in the root directory");
    }

    if (exists(parentDirectoryId, title)) {
      throw new IllegalArgumentException(
          "Secret with title " + title + " already exists in the directory");
    }

    String id = "secret-" + java.util.UUID.randomUUID().toString();
    secrets.add(new Secret(id, title, value, parentDirectoryId));
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
    if (!parentDirectoryId.equals(ROOT_DIRECTORY_ID)) {
      throw new IllegalArgumentException(
          "In Cloud Mock secrets can only be listed in the root directory");
    }

    return List.copyOf(secrets);
  }

  Secret findAssetInRootByTitle(String subPath) {
    return secrets.stream()
        .filter(
            secret ->
                secret.title.equals(subPath) && secret.parentDirectoryId.equals(ROOT_DIRECTORY_ID))
        .findFirst()
        .orElse(null);
  }

  record Secret(String id, String title, String value, String parentDirectoryId) {
    Asset asAsset() {
      return new Asset(id, title, parentDirectoryId);
    }
  }

  public record Asset(String id, String title, String parentId) {}

  final Asset rootDirectory = new Asset(ROOT_DIRECTORY_ID, "", null);
}
