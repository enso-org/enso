/**
 * Status bar for table visualization.
 */
export class TableVizStatusBar {
  eGui!: HTMLElement

  /**
   * Initializes the statusbar with the provided parameters.
   * @param params The statusbar parameter: total row count.
   */
  init(params: { total: number; filtered: number }) {
    const showFilteredCount = params.filtered != null && params.filtered != params.total
    const display = showFilteredCount ? 'block' : 'none'
    this.eGui = document.createElement('div')
    this.eGui.innerHTML = `
            <div><b>Total Row Count:</b> ${params.total}</div>
            <div style="display:${display}"><b>Filtered Row Count:</b> ${params.filtered}</div>
          `
  }

  /**
   * Returns the statusbar DOM element.
   */
  getGui() {
    return this.eGui
  }
}
