package org.enso.microsoft.azure;

import com.azure.core.credential.TokenCredential;
import com.azure.core.management.AzureEnvironment;
import com.azure.core.management.profile.AzureProfile;

import java.util.ArrayList;
import java.util.List;

public final class AzureResourceManager {
  private static com.azure.resourcemanager.AzureResourceManager.Authenticated getClient(TokenCredential credential, AzureEnvironment environment) {
    var client = com.azure.resourcemanager.AzureResourceManager
        .authenticate(credential, new AzureProfile(environment));
    return client;
  }

  /**
   * Represents an Azure tenant.
   *
   * @param id the tenant ID (a GUID).
   * @param key ??
   */
  public record AzureTenant(String id, String key) {}

  /**
   * Gets a list of Azure tenants associated with the provided credential and environment.
   *
   * @param credential the Azure credential.
   * @param environment the Azure environment.
   * @return
   */
  public static List<AzureTenant> tenants(TokenCredential credential, AzureEnvironment environment) {
    var tenants = getClient(credential, environment).tenants();
    var result = new ArrayList<AzureTenant>();
    for (var tenant : tenants.list()) {
      result.add(new AzureTenant(tenant.tenantId(), tenant.key()));
    }
    return result;
  }

  /**
   * Represents an Azure subscription.
   *
   * @param id the subscription ID.
   * @param name the subscription name.
   */
  public record AzureSubscription(String id, String name) {}

  /**
   * Creates an Azure Resource Manager client using the provided credential and environment.
   *
   * @param credential the Azure credential.
   * @param environment the Azure environment.
   */
  public static List<AzureSubscription> subscriptions(TokenCredential credential, AzureEnvironment environment) {
    var subscriptions = getClient(credential, environment).subscriptions();
    var result = new ArrayList<AzureSubscription>();
    for (var subscription : subscriptions.list()) {
      result.add(new AzureSubscription(subscription.subscriptionId(), subscription.displayName()));
    }
    return result;
  }
}
