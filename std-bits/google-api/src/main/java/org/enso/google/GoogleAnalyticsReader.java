package org.enso.google;

import com.google.analytics.admin.v1beta.AnalyticsAdminServiceClient;
import com.google.analytics.admin.v1beta.AnalyticsAdminServiceSettings;
import com.google.analytics.admin.v1beta.ListAccountsRequest;
import com.google.analytics.admin.v1beta.ListPropertiesRequest;
import com.google.analytics.data.v1beta.BetaAnalyticsDataClient;
import com.google.analytics.data.v1beta.BetaAnalyticsDataSettings;
import com.google.analytics.data.v1beta.DateRange;
import com.google.analytics.data.v1beta.Dimension;
import com.google.analytics.data.v1beta.GetMetadataRequest;
import com.google.analytics.data.v1beta.Metadata;
import com.google.analytics.data.v1beta.Metric;
import com.google.analytics.data.v1beta.RunReportRequest;
import com.google.api.gax.core.CredentialsProvider;
import java.io.IOException;
import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;
import java.util.stream.IntStream;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.StringBuilder;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.Table;

public class GoogleAnalyticsReader {
  private static final Map<String, Metadata> metadataCache = new HashMap<>();

  public record AnalyticsAccount(
      String id, String displayName, boolean deleted, ZonedDateTime created, String regionCode) {}

  public record AnalyticsProperty(
      String id,
      String displayName,
      boolean deleted,
      ZonedDateTime created,
      String account,
      String currency,
      TimeZone timeZone) {}

  public record AnalyticDimension(
      String apiName, String displayName, String category, String description) {}

  private static AnalyticsAdminServiceClient createAdminClient(
      CredentialsProvider credentialsProvider) throws IOException {
    if (credentialsProvider == null) {
      // Default Credentials Path
      return AnalyticsAdminServiceClient.create();
    }

    var settings =
        AnalyticsAdminServiceSettings.newBuilder()
            .setCredentialsProvider(credentialsProvider)
            .build();
    return AnalyticsAdminServiceClient.create(settings);
  }

  private static BetaAnalyticsDataClient createDataClient(CredentialsProvider credentialsProvider)
      throws IOException {
    if (credentialsProvider == null) {
      // Default Credentials Path
      return BetaAnalyticsDataClient.create();
    }

    var settings =
        BetaAnalyticsDataSettings.newBuilder().setCredentialsProvider(credentialsProvider).build();
    return BetaAnalyticsDataClient.create(settings);
  }

  /** Lists all Google Analytics accounts. */
  public static AnalyticsAccount[] listAccounts(
      CredentialsProvider credentialsProvider, int limit, boolean includeDeleted)
      throws IOException {
    int pageSize = getPageSize(limit);

    var request =
        ListAccountsRequest.newBuilder()
            .setPageSize(pageSize)
            .setShowDeleted(includeDeleted)
            .build();

    try (var client = createAdminClient(credentialsProvider)) {
      var response = client.listAccounts(request);
      var output = new ArrayList<AnalyticsAccount>(pageSize);
      for (var page : response.iteratePages()) {
        for (var account : page.iterateAll()) {
          var ensoAccount =
              new AnalyticsAccount(
                  account.getName(),
                  account.getDisplayName(),
                  account.getDeleted(),
                  Instant.ofEpochSecond(
                          account.getCreateTime().getSeconds(), account.getCreateTime().getNanos())
                      .atZone(ZoneId.systemDefault()),
                  account.getRegionCode());

          output.add(ensoAccount);
          if (limit != 0 && output.size() == limit) {
            break;
          }
        }
      }

      return output.toArray(new AnalyticsAccount[0]);
    }
  }

  private static int getPageSize(int limit) {
    return (limit == 0 || limit > 1000) ? 1000 : limit;
  }

  /**
   * Lists all properties of a given account.
   *
   * @param credentialsProvider the credentials provider
   * @param parents the parent accounts or null for all properties (e.g. "accounts/123" for account
   *     with ID 123)
   * @param limit the maximum number of properties to return (0 for all properties, up to 1000)
   * @param includeDeleted whether to include deleted properties
   * @return an array of properties
   */
  public static AnalyticsProperty[] listProperties(
      CredentialsProvider credentialsProvider,
      AnalyticsAccount[] parents,
      int limit,
      boolean includeDeleted)
      throws IOException {
    if (parents == null) {
      parents = listAccounts(credentialsProvider, 0, false);
    }

    if (parents.length == 0) {
      return new AnalyticsProperty[0];
    }

    int pageSize = getPageSize(limit);

    var output = new ArrayList<AnalyticsProperty>(pageSize);
    try (var client = createAdminClient(credentialsProvider)) {
      for (var parent : parents) {
        var request =
            ListPropertiesRequest.newBuilder()
                .setPageSize(pageSize)
                .setShowDeleted(includeDeleted)
                .setFilter("parent: " + parent.id());

        var response = client.listProperties(request.build());
        for (var page : response.iteratePages()) {
          for (var property : page.iterateAll()) {
            var ensoProperty =
                new AnalyticsProperty(
                    property.getName(),
                    property.getDisplayName(),
                    property.hasDeleteTime(),
                    Instant.ofEpochSecond(
                            property.getCreateTime().getSeconds(),
                            property.getCreateTime().getNanos())
                        .atZone(ZoneId.systemDefault()),
                    property.getAccount(),
                    property.getCurrencyCode(),
                    TimeZone.getTimeZone(property.getTimeZone()));
            output.add(ensoProperty);
          }
        }
      }

      return output.toArray(new AnalyticsProperty[0]);
    }
  }

  /**
   * Lists all metrics available in a Google Analytics property.
   *
   * @param credentialsProvider the credentials provider (null for default credentials)
   * @param property the property to list metrics for
   * @return an array of metrics
   */
  public static AnalyticDimension[] listMetrics(
      CredentialsProvider credentialsProvider, AnalyticsProperty property) throws IOException {
    var metadata = getMetadata(credentialsProvider, property.id());
    return metadata.getMetricsList().stream()
        .map(
            metric ->
                new AnalyticDimension(
                    metric.getApiName(),
                    metric.getUiName(),
                    metric.getCategory(),
                    metric.getDescription()))
        .toArray(AnalyticDimension[]::new);
  }

  /**
   * Lists all dimensions available in Google Analytics.
   *
   * @return an array of dimensions
   */
  public static AnalyticDimension[] listDimensions(
      CredentialsProvider credentialsProvider, AnalyticsProperty property) throws IOException {
    var metadata = getMetadata(credentialsProvider, property.id());
    return metadata.getDimensionsList().stream()
        .map(
            dimension ->
                new AnalyticDimension(
                    dimension.getApiName(),
                    dimension.getUiName(),
                    dimension.getCategory(),
                    dimension.getDescription()))
        .toArray(AnalyticDimension[]::new);
  }

  /** Caches metadata requests for Google Analytics properties. */
  private static synchronized Metadata getMetadata(
      CredentialsProvider credentialsProvider, String propertyId) throws IOException {
    if (metadataCache.containsKey(propertyId)) {
      return metadataCache.get(propertyId);
    }

    var request = GetMetadataRequest.newBuilder().setName(propertyId + "/metadata").build();

    try (var client = createDataClient(credentialsProvider)) {
      var metadata = client.getMetadata(request);
      metadataCache.put(propertyId, metadata);
      return metadata;
    }
  }

  /** Clears the metadata cache. */
  public static void clearMetadataCache() {
    metadataCache.clear();
  }

  /**
   * Runs a report in Google Analytics.
   *
   * @param credentialsProvider the credentials provider
   * @param property the property to run the report on
   * @param startDate the start date of the report
   * @param endDate the end date of the report
   * @param dimensions the dimensions to include in the report
   * @param metrics the metrics to include in the report
   * @return a Table with the report data
   */
  public static Table runReport(
      CredentialsProvider credentialsProvider,
      AnalyticsProperty property,
      LocalDate startDate,
      LocalDate endDate,
      List<String> dimensions,
      List<String> metrics)
      throws IOException {
    var dateRange =
        DateRange.newBuilder()
            .setStartDate(startDate.format(DateTimeFormatter.ISO_LOCAL_DATE))
            .setEndDate(endDate.format(DateTimeFormatter.ISO_LOCAL_DATE))
            .build();

    var request =
        RunReportRequest.newBuilder()
            .setProperty(property.id())
            .addDateRanges(dateRange)
            .addAllDimensions(
                dimensions.stream().map(n -> Dimension.newBuilder().setName(n).build()).toList())
            .addAllMetrics(
                metrics.stream().map(n -> Metric.newBuilder().setName(n).build()).toList())
            .build();

    try (var client = createDataClient(credentialsProvider)) {
      var response = client.runReport(request);
      int rowCount = response.getRowCount();

      var builders = new Builder[dimensions.size() + metrics.size()];
      for (int i = 0; i < dimensions.size() + metrics.size(); i++) {
        builders[i] = new StringBuilder(rowCount, TextType.VARIABLE_LENGTH);
      }

      // Load the data
      for (int row = 0; row < rowCount; row++) {
        for (int col = 0; col < dimensions.size(); col++) {
          builders[col].append(response.getRows(row).getDimensionValues(col).getValue());
        }

        for (int col = 0; col < metrics.size(); col++) {
          builders[dimensions.size() + col].append(
              response.getRows(row).getMetricValues(col).getValue());
        }
      }

      // Convert to Java Table
      var columns =
          IntStream.range(0, builders.length)
              .mapToObj(
                  i ->
                      new Column(
                          i < dimensions.size()
                              ? dimensions.get(i)
                              : metrics.get(i - dimensions.size()),
                          builders[i].seal()))
              .toArray(Column[]::new);
      return new Table(columns);
    }
  }
}
