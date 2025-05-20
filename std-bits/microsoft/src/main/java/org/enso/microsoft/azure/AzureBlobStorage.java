package org.enso.microsoft.azure;

import com.azure.core.credential.TokenCredential;
import com.azure.storage.blob.BlobServiceClientBuilder;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public final class AzureBlobStorage {
  public static List<String> containers(TokenCredential credential, String storageAccountName, String prefix) {
    var client = new BlobServiceClientBuilder()
        .endpoint("https://" + storageAccountName + ".blob.core.windows.net/")
        .credential(credential)
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
