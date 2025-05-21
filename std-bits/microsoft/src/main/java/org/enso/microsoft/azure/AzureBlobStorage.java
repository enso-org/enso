package org.enso.microsoft.azure;

import com.azure.storage.blob.BlobServiceClientBuilder;
import org.slf4j.Logger;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public final class AzureBlobStorage {
  private static final Logger LOGGER = org.slf4j.LoggerFactory.getLogger(AzureBlobStorage.class);

  public static List<String> containers(AzureCredential credential, String storageAccountName, String prefix) {
    LOGGER.warn("Reading from Blob Storage: {}", storageAccountName);

    var client = new BlobServiceClientBuilder()
        .endpoint("https://" + storageAccountName + ".blob.core.windows.net/")
        .credential(CredentialHelper.toTokenCredential(credential))
        .buildClient();

    var result = new ArrayList<String>();
    for (var blobContainer : client.listBlobContainers()) {
      var containerName = blobContainer.getName();
      if ((Objects.equals(prefix, "")) || containerName.startsWith(prefix)) {
        result.add(containerName);
      }
    }
    return result;
  }
}
