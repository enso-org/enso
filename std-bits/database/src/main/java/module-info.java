import org.enso.base.enso_cloud.DataLinkSPI;
import org.enso.base.file_format.FileFormatSPI;
import org.enso.database.DatabaseConnectionDetailsSPI;
import org.enso.database.postgres.PostgresConnectionDetailsSPI;
import org.enso.database.postgres.PostgresDataLinkSPI;
import org.enso.database.sqlite.SQLiteConnectionDetailsSPI;
import org.enso.database.sqlite.SQLiteFormatSPI;
import org.enso.database.sqlite.SQLiteInMemoryDetailsSPI;

module org.enso.std.database {
  requires java.logging;
  requires java.sql;
  requires org.enso.std.base;
  requires org.enso.common.polyglot.core.utils;
  requires org.graalvm.collections;
  requires org.graalvm.polyglot;
  requires com.fasterxml.jackson.databind;

  uses DatabaseConnectionDetailsSPI;
  uses java.sql.Driver;

  exports org.enso.database;

  provides DatabaseConnectionDetailsSPI with
      PostgresConnectionDetailsSPI,
      SQLiteConnectionDetailsSPI,
      SQLiteInMemoryDetailsSPI;
  provides DataLinkSPI with
      PostgresDataLinkSPI;
  provides FileFormatSPI with
      SQLiteFormatSPI;

  opens org.enso.database.dryrun;
}
