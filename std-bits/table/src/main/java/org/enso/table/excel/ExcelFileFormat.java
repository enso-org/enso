package org.enso.table.excel;

public enum ExcelFileFormat {
  XLS {
    @Override
    ExcelFormatStrategy createStrategy() {
      return new XlsFormatStrategy();
    }
  },
  XLSX {
    @Override
    ExcelFormatStrategy createStrategy() {
      return new XlsxFormatStrategy();
    }
  },
  XLSX_FALLBACK {
    @Override
    ExcelFormatStrategy createStrategy() {
      return new XlsxFormatStrategy();
    }
  };

  abstract ExcelFormatStrategy createStrategy();
}
