package org.enso.microsoft.azure;

import com.azure.storage.blob.BlobServiceClient;
import com.azure.storage.blob.BlobServiceClientBuilder;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class AzureBlobStorage {
  private static final Logger LOGGER = LoggerFactory.getLogger(AzureBlobStorage.class);

  private static BlobServiceClient makeClient(
      AzureCredential credential, String storageAccountName) {
    var clientBuilder =
        new BlobServiceClientBuilder()
            .endpoint("https://" + storageAccountName + ".blob.core.windows.net/");

    if (credential instanceof AzureCredential.BlobStorageSASToken) {
      clientBuilder.sasToken(CredentialHelper.toSASToken(credential));
    } else {
      clientBuilder.credential(CredentialHelper.toTokenCredential(credential));
    }

    return clientBuilder.buildClient();
  }

  public static List<String> containers(
      AzureCredential credential, String storageAccountName, String prefix) {
    LOGGER.warn("Reading from Blob Storage: {}", storageAccountName);

    var client = makeClient(credential, storageAccountName);
    var containerList = client.listBlobContainers();

    var result = new ArrayList<String>();
    for (var blobContainer : containerList) {
      var containerName = blobContainer.getName();
      if ((Objects.equals(prefix, "")) || containerName.startsWith(prefix)) {
        result.add(containerName);
      }
    }
    return result;
  }

  public static List<String> listBlob(
      AzureCredential credential, String storageAccountName, String containerName, String prefix) {
    LOGGER.warn("List Blob Storage: {} : {}", storageAccountName);

    var client = makeClient(credential, storageAccountName);
    var blobContainer = client.getBlobContainerClient(containerName);
    if (!blobContainer.exists()) {
      throw new IllegalArgumentException("Container does not exist: " + containerName);
    }

    var blobList = blobContainer.listBlobs();

    var result = new ArrayList<String>();
    for (var blob : blobList) {
      var blobName = blob.getName();
      if ((Objects.equals(prefix, "")) || blobName.startsWith(prefix)) {
        result.add(blobName);
      }
    }
    return result;
  }

  public static String getBlob(
      AzureCredential credential, String storageAccountName, String containerName, String blobName)
      throws IOException {
    LOGGER.trace("Reading from Blob Storage.");

    var client = makeClient(credential, storageAccountName);
    var blobContainer = client.getBlobContainerClient(containerName);
    if (!blobContainer.exists()) {
      throw new IllegalArgumentException("Container does not exist: " + containerName);
    }

    var blob = blobContainer.getBlobClient(blobName);
    if (!blob.exists()) {
      throw new IllegalArgumentException("Blob does not exist: " + blobName);
    }

    var tempFile = Files.createTempFile("enso-blob-", blobName.replaceAll("[^A-Za-z0-9_.]", "_"));
    blob.downloadToFile(tempFile.toString(), true);
    LOGGER.trace("Downloaded blob to: {}", tempFile);
    return tempFile.toString();
  }
}
