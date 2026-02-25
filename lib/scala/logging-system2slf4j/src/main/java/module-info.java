import org.enso.logging.system2slf4j.SystemLoggerViaSlf4j;

module org.enso.logging.system2slf4j {
  requires org.slf4j;

  provides java.lang.System.LoggerFinder with
      SystemLoggerViaSlf4j;
}
