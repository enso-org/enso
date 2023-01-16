/** Simple Warning Visualization. */
class WarningsVisualization extends Visualization {
    static inputType = 'Any'
    static label = 'Warnings'

    constructor(data) {
        super(data)
        this.setPreprocessor('Standard.Visualization.Warnings', 'process_to_json_text')
    }

    onDataReceived(data) {
        while (this.dom.firstChild) {
            this.dom.removeChild(this.dom.lastChild)
        }

        const divElement = document.createElement('div')
        divElement.setAttributeNS(null, 'id', 'vis-tbl-view')
        divElement.setAttributeNS(null, 'class', 'scrollable')
        divElement.setAttributeNS(null, 'width', '100%')
        divElement.setAttributeNS(null, 'height', '100%')
        this.divElem = divElement
        this.dom.appendChild(divElement)
        this.updateDivSize()

        const parsedData = typeof data === 'string' ? JSON.parse(data) : data
        const prefix =
            '<ul style="font-family: DejaVuSansMonoBook, sans-serif; font-size: 12px; white-space: pre;">'
        const body =
            parsedData.length == ''
                ? '<li>There are no warnings.</li>'
                : parsedData.map(d => '<li>' + this.escapeHtml(d) + '</li>').join('')
        const suffix = '</ul>'
        divElement.innerHTML = prefix + body + suffix
    }

    updateDivSize() {
        if (this.divElem !== undefined) {
            const width = this.dom.getAttributeNS(null, 'width')
            const height = this.dom.getAttributeNS(null, 'height')
            const tblViewStyle = `width: ${width - 5}px; height: ${
                height - 5
            }px; overflow: auto; padding:10px 0 0 10px;`
            this.divElem.setAttributeNS(null, 'style', tblViewStyle)
            this.divElem.setAttributeNS(null, 'viewBox', '0 0 ' + width + ' ' + height)
        }
    }

    setSize(size) {
        this.dom.setAttributeNS(null, 'width', size[0])
        this.dom.setAttributeNS(null, 'height', size[1])
        this.updateDivSize()
    }

    escapeHtml(unsafe) {
        return unsafe
            .replaceAll('&', '&amp;')
            .replaceAll('<', '&lt;')
            .replaceAll('>', '&gt;')
            .replaceAll('"', '&quot;')
            .replaceAll("'", '&#039;')
    }
}

return WarningsVisualization
