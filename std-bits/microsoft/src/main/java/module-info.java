import org.enso.database.DatabaseConnectionDetailsSPI;
import org.enso.microsoft.SQLServerConnectionDetailsSPI;

module org.enso.std.microsoft {
  requires org.enso.std.base;
  requires org.enso.std.database;
  requires com.microsoft.sqlserver.jdbc;

  provides DatabaseConnectionDetailsSPI with
      SQLServerConnectionDetailsSPI;
}
