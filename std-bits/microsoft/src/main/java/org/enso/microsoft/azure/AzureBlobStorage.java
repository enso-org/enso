package org.enso.microsoft.azure;

import com.azure.storage.blob.BlobServiceClientBuilder;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import org.slf4j.Logger;

public final class AzureBlobStorage {
  private static final Logger LOGGER = org.slf4j.LoggerFactory.getLogger(AzureBlobStorage.class);

  public static List<String> containers(
      AzureCredential credential, String storageAccountName, String prefix) {
    LOGGER.warn("Reading from Blob Storage: {}", storageAccountName);

    var client =
        new BlobServiceClientBuilder()
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

  public static Path getBlob(
      AzureCredential credential, String storageAccountName, String containerName, String blobName)
      throws IOException {
    LOGGER.trace("Reading from Blob Storage.");

    var client =
        new BlobServiceClientBuilder()
            .endpoint("https://" + storageAccountName + ".blob.core.windows.net/")
            .credential(CredentialHelper.toTokenCredential(credential))
            .buildClient();

    var blobContainer = client.getBlobContainerClient(containerName);
    var blob = blobContainer.getBlobClient(blobName);

    var tempFile = Files.createTempFile("enso-blob-", ".tmp");
    blob.downloadToFile(tempFile.toString());
    LOGGER.trace("Downloaded blob to: {}", tempFile);
    return tempFile;
  }
}
