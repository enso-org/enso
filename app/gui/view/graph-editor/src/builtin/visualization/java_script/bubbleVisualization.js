class BubbleVisualization extends Visualization {
    static inputType = 'Any'

    onDataReceived(data) {
        const xmlns = 'http://www.w3.org/2000/svg'
        while (this.dom.firstChild) {
            this.dom.removeChild(this.dom.lastChild)
        }
        const width = this.dom.getAttributeNS(null, 'width')
        const height = this.dom.getAttributeNS(null, 'height')

        const svgElem = document.createElementNS(xmlns, 'svg')
        svgElem.setAttributeNS(null, 'class', 'vis-svg')
        svgElem.setAttributeNS(null, 'viewBox', 0 + ' ' + 0 + ' ' + width + ' ' + height)
        svgElem.setAttributeNS(null, 'width', '100%')
        svgElem.setAttributeNS(null, 'height', '100%')
        svgElem.setAttributeNS(null, 'transform', 'matrix(1 0 0 -1 0 0)')

        this.dom.appendChild(svgElem)

        data.forEach(data => {
            const bubble = document.createElementNS(xmlns, 'circle')
            bubble.setAttributeNS(null, 'stroke', 'black')
            bubble.setAttributeNS(null, 'fill', 'red')
            bubble.setAttributeNS(null, 'r', data[2] || 0)
            bubble.setAttributeNS(null, 'cx', data[0] || 0)
            bubble.setAttributeNS(null, 'cy', data[1] || 0)
            svgElem.appendChild(bubble)
        })
    }

    setSize(size) {
        this.dom.setAttributeNS(null, 'width', size[0])
        this.dom.setAttributeNS(null, 'height', size[1])
    }
}

return BubbleVisualization
