import * as ag from 'ag-grid-enterprise'

/**
 * Work around (AG-16857)[https://github.com/ag-grid/ag-grid/issues/12910]. This is needed for cell selection by
 * click-drag to work within shadow roots in AG Grid versions including 35.2.0. It is expected to be fixed in the next
 * release. The bug is covered by the 'Copy/paste from Table Visualization' integration test.
 */
export function onCellMouseOver(event: ag.CellMouseOverEvent<unknown>) {
  const ev = event.event
  const grid = event.api
  if (ev instanceof MouseEvent && ev.buttons === 1) {
    const clickedCell = grid.getFocusedCell()
    if (clickedCell) {
      grid.clearCellSelection()
      grid.addCellRange({
        rowStartIndex: clickedCell.rowIndex,
        columnStart: clickedCell.column,
        rowEndIndex: event.rowIndex,
        columnEnd: event.column,
      })
    }
  }
}

ag.ModuleRegistry.registerModules([ag.GridStateModule])
