package org.enso.database.postgres;

import org.enso.base.enso_cloud.DataLinkSPI;

<<<<<<< HEAD:std-bits/database/src/main/java/org/enso/database/postgres/PostgresDataLinkSPI.java
public final class PostgresDataLinkSPI extends DataLinkSPI {
=======
@org.openide.util.lookup.ServiceProvider(service = DataLinkSPI.class)
public final class PostgresDataLinkImpl extends DataLinkSPI {
>>>>>>> origin/develop:std-bits/database/src/main/java/org/enso/database/postgres/PostgresDataLinkImpl.java
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
