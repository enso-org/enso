package org.enso.table.excel;

public enum ExcelFileFormat {
  XLS {
    @Override
    public ExcelFormatStrategy createStrategy() {
      return new XlsFormatStrategy();
    }
  },
  XLSX {
    @Override
    public ExcelFormatStrategy createStrategy() {
      return new XlsxFormatStrategy();
    }
  },
  XLSX_FALLBACK {
    @Override
    public ExcelFormatStrategy createStrategy() {
      return new XlsxFormatStrategy();
    }
  };

  public abstract ExcelFormatStrategy createStrategy();
}
