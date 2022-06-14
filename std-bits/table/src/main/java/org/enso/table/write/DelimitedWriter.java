package org.enso.table.write;


import org.enso.table.data.table.Table;
import org.enso.table.formatting.DataFormatter;

import java.io.Writer;

public class DelimitedWriter {
    public static void write(Writer output, Table table, DataFormatter[] columnFormatters) {
        assert table.getColumns().length == columnFormatters.length;
    }
}
