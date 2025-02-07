/**
 * Custom tooltip for table visualization.
 */
export class TableVizStatusBar {
  eGui!: HTMLElement

  /**
   * Initializes the tooltip with the provided parameters.
   * @param params The tooltip parameters: the data quality metrics, total row count,
   * and a flag whether to show/hide the data quality indicators.
   */
  init(params: { total: number }) {
    this.eGui = document.createElement('div')
    this.eGui.innerHTML = `
            <div><b>Total Row Count:</b> ${params.total}</div>
        `
  }

  /**
   * Returns the tooltip DOM element.
   */
  getGui() {
    return this.eGui
  }
}
