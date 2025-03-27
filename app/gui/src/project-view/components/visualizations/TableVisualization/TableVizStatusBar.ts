/**
 * Status bar for table visualization.
 */
export class TableVizStatusBar {
  eGui!: HTMLElement

  /**
   * Initializes the statusbar with the provided parameters.
   * @param params The statusbar parameter: total row count.
   */
  init(params: { total: number }) {
    this.eGui = document.createElement('div')
    this.eGui.innerHTML = `
              <div><b>Total Row Count:</b> ${params.total}</div>
          `
  }

  /**
   * Returns the statusbar DOM element.
   */
  getGui() {
    return this.eGui
  }
}
