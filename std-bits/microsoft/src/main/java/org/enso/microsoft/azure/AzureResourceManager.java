package org.enso.microsoft.azure;

import com.azure.core.management.AzureEnvironment;
import com.azure.core.management.profile.AzureProfile;
import com.azure.resourcemanager.storage.models.StorageAccount;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.enso.table.util.LeastRecentlyUsedCache;

public final class AzureResourceManager {
  private static com.azure.resourcemanager.AzureResourceManager.Authenticated getClient(
      AzureCredential credential, AzureEnvironment environment) {
    return com.azure.resourcemanager.AzureResourceManager.authenticate(
        CredentialHelper.toTokenCredential(credential), new AzureProfile(environment));
  }

  /**
   * Gets a list of Azure tenants associated with the provided credential and environment.
   *
   * @param credential the Azure credential.
   * @param environment the Azure environment.
   * @return a list of Azure tenants.
   */
  public static List<String> tenants(AzureCredential credential, AzureEnvironment environment) {
    var tenants = getClient(credential, environment).tenants();
    var result = new ArrayList<String>();
    for (var tenant : tenants.list()) {
      result.add(tenant.tenantId());
    }
    return result;
  }

  private static Map<String, List<AzureSubscription>> _subscriptionsCache;

  private static Map<String, List<AzureSubscription>> subscriptionsCache() {
    if (_subscriptionsCache == null) {
      _subscriptionsCache = new LeastRecentlyUsedCache<>(100);
    }
    return _subscriptionsCache;
  }

  /**
   * Represents an Azure subscription.
   *
   * @param id the subscription ID.
   * @param name the subscription name.
   */
  public record AzureSubscription(String id, String name) {}

  /**
   * Lists Azure subscriptions using the provided credential and environment.
   *
   * @param credential the Azure credential.
   * @param environment the Azure environment.
   */
  public static List<AzureSubscription> subscriptions(
      AzureCredential credential, AzureEnvironment environment) {
    var cacheKey = credential.uniqueId() + environment.toString();
    return subscriptionsCache()
        .computeIfAbsent(
            cacheKey,
            k -> {
              var subscriptions = getClient(credential, environment).subscriptions();
              var result = new ArrayList<AzureSubscription>();
              for (var subscription : subscriptions.list()) {
                result.add(
                    new AzureSubscription(
                        subscription.subscriptionId(), subscription.displayName()));
              }
              return result;
            });
  }

  /**
   * Represents an Azure storage account.
   *
   * @param credential the Azure credential used to access the storage account.
   * @param environment the Azure environment.
   * @param subscriptionId the Azure subscription (if null use the Default subscription).
   * @param inner the underlying storage account object.
   */
  public record AzureStorageAccount(
      AzureCredential credential,
      AzureEnvironment environment,
      String subscriptionId,
      StorageAccount inner) {
    public String id() {
      return inner.id();
    }

    public String name() {
      return inner.name();
    }

    public String resourceGroup() {
      return inner.resourceGroupName();
    }

    public String endpoints() {
      return inner.endPoints().toString();
    }
  }

  public static List<AzureStorageAccount> storageAccounts(
      AzureCredential credential, AzureEnvironment environment, String subscriptionId) {
    var client = getClient(credential, environment);
    var forSubs =
        subscriptionId == null
            ? client.withDefaultSubscription()
            : client.withSubscription(subscriptionId);

    var result = new ArrayList<AzureStorageAccount>();
    var storageAccounts = forSubs.storageAccounts();
    for (var account : storageAccounts.list()) {
      result.add(new AzureStorageAccount(credential, environment, subscriptionId, account));
    }
    return result;
  }
}
