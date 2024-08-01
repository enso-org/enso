package org.enso.tableau;

import org.enso.base.file_format.FileFormatSPI;

@org.openide.util.lookup.ServiceProvider(service = FileFormatSPI.class)
public class TableauFormatSPI extends FileFormatSPI {
  @Override
  protected String getModuleName() {
    return "Standard.Tableau.Tableau_Format";
  }

  @Override
  protected String getTypeName() {
    return "Tableau_Format";
  }

  @Override
  protected String getDataLinkFormatName() {
    return "tableau";
  }
}
