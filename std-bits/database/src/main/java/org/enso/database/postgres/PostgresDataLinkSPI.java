package org.enso.database.postgres;

import org.enso.base.enso_cloud.DataLinkSPI;

public final class PostgresDataLinkSPI extends DataLinkSPI {
  @Override
  protected String getModuleName() {
    return "Standard.Database.Connection.Data_Link.Postgres_Data_Link";
  }

  @Override
  protected String getTypeName() {
    return "Postgres_Data_Link";
  }

  @Override
  protected String getLinkTypeName() {
    return "Postgres_Connection";
  }
}
