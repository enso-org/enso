import type { ITooltipComp, ITooltipParams } from '@ag-grid-community/core'
import type { DataQualityMetricValue } from '../TableVisualization.vue'

/**
 * Custom tooltip for table visualization.
 */
export class TableVisualisationTooltip implements ITooltipComp {
  eGui!: HTMLElement

  /**
   * Initializes the tooltip with the provided parameters.
   * @param params The tooltip parameters: the data quality metrics, total row count,
   * and a flag whether to show/hide the data quality indicators.
   */
  init(
    params: ITooltipParams & {
      dataQualityMetrics: DataQualityMetricValue[]
      total: number
      showDataQuality: boolean
    },
  ) {
    this.eGui = document.createElement('div')

    Object.assign(this.eGui.style, {
      backgroundColor: '#f5f5f5',
      border: '1px solid #c0c0c0',
      padding: '10px',
      boxShadow: '0px 4px 8px rgba(0, 0, 0, 0.15)',
      borderRadius: '4px',
      fontFamily: 'Arial, sans-serif',
      color: '#333',
    })

    const getPercentage = (value: number) => (value / params.total) * 100
    const createIndicator = (value: number) => {
      const color =
        value < 33 ? 'green'
        : value < 66 ? 'orange'
        : 'red'
      return `<div style="display: inline-block; width: 10px; height: 10px; border-radius: 50%; background-color: ${color}; margin-left: 5px;"></div>`
    }

    const getDataQualityTemplate = () => {
      let template = ''
      params.dataQualityMetrics.forEach((obj) => {
        const rendered =
          obj.displayType === 'Percentage' ?
            `${getPercentage(obj.value).toFixed(2)}% ${createIndicator(getPercentage(obj.value))}`
          : `${obj.value}`
        const metricTemplate =
          obj.name !== '' ?
            `<div>${obj.name}: ${rendered}</div>`
          : `<div>${rendered.replaceAll('\n', '<br/>')}</div>`
        template = template + metricTemplate
      })
      return template
    }

    this.eGui.innerHTML = `
            <div><b>Column value type:</b> ${params.value}</div>
            <div style="display: ${params.showDataQuality ? 'block' : 'none'};"">
                ${getDataQualityTemplate()}
            </div>
        `
  }

  /**
   * Returns the tooltip DOM element.
   */
  getGui() {
    return this.eGui
  }
}
