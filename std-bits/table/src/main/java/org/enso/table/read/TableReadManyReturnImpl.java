package org.enso.table.read;

import org.enso.base.read.ReadManyReturnSPI;

@org.openide.util.lookup.ServiceProvider(service = ReadManyReturnSPI.class)
public final class TableReadManyReturnImpl extends ReadManyReturnSPI {
  @Override
  protected String getModuleName() {
    return "Standard.Table.Return_As_Table";
  }

  @Override
  protected String getTypeName() {
    return "Return_As_Table";
  }
}
