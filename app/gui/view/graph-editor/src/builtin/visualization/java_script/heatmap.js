/** Heatmap Visualization. */
// TODO refactor this to avoid loading on startup. See issue #985 .
loadScript('https://d3js.org/d3.v4.min.js')
loadStyle('https://fonts.cdnfonts.com/css/dejavu-sans-mono')

/**
 * A d3.js heatmap visualization.
 */
class Heatmap extends Visualization {
    static inputType = 'Standard.Table.Data.Table.Table | Standard.Base.Data.Vector.Vector'
    static label = 'Heatmap'

    constructor(data) {
        super(data)
        this.setPreprocessor('Standard.Visualization.Table.Visualization', 'prepare_visualization')
    }

    onDataReceived(data) {
        const { parsedData, isUpdate } = this.parseData(data)
        if (!ok(parsedData)) {
            console.error('Heatmap got invalid data: ' + data.toString())
        }
        this.updateState(parsedData, isUpdate)

        if (!this.isInitialised()) {
            this.initCanvas()
        }

        this.initHeatmap()
    }

    parseData(data) {
        let parsedData
        if (typeof data === 'string' || data instanceof String) {
            parsedData = JSON.parse(data)
        } else {
            parsedData = data
        }
        const isUpdate = parsedData.update === 'diff'
        return { parsedData, isUpdate }
    }

    /**
     * Indicates whether this visualisation has been initialised.
     */
    isInitialised() {
        ok(this.svg)
    }

    /**
     * Update the internal data and plot settings with the ones from the new incoming data.
     * If no new settings/data have been provided the old ones will be kept.
     */
    updateState(data, isUpdate) {
        if (isUpdate) {
            this._dataValues = ok(data.data) ? data.data : this.data
        } else {
            this._dataValues = this.extractValues(data)
        }
    }

    extractValues(rawData) {
        /// Note this is a workaround for #1006, we allow raw arrays and JSON objects to be consumed.
        if (ok(rawData.data)) {
            return rawData.data
        } else if (Array.isArray(rawData)) {
            return rawData
        } else if (ok(rawData.json) && Array.isArray(rawData.json)) {
            return rawData.json
        }
        return []
    }

    /**
     * Return vales to plot.
     */
    data() {
        return this._dataValues || {}
    }

    /**
     * Return the layout measurements for the plot. This includes the outer dimensions of the
     * drawing area as well as the inner dimensions of the plotting area and the margins.
     */
    canvasDimensions() {
        const width = this.dom.getAttributeNS(null, 'width')
        const height = this.dom.getAttributeNS(null, 'height')
        const margin = { top: 20, right: 20, bottom: 20, left: 25 }
        return {
            inner: {
                width: width - margin.left - margin.right,
                height: height - margin.top - margin.bottom,
            },
            outer: { width, height },
            margin,
        }
    }

    /**
     * Creates HTML div element as container for plot.
     */
    createOuterContainerWithStyle(width, height) {
        const divElem = document.createElementNS(null, 'div')
        divElem.setAttributeNS(null, 'class', 'vis-heatmap')
        divElem.setAttributeNS(null, 'viewBox', 0 + ' ' + 0 + ' ' + width + ' ' + height)
        divElem.setAttributeNS(null, 'width', '100%')
        divElem.setAttributeNS(null, 'height', '100%')

        const addStyleToElem = (attr, stl) => {
            const style = document.createElement('style')
            style.innerText = attr + '{' + stl + '}'

            divElem.appendChild(style)
        }

        const darkStrokeColor = `rgba(255,255,255,0.7)`

        addStyleToElem('.dark-theme text', `fill: ${darkStrokeColor};`)

        return divElem
    }

    /**
     * Initialise the drawing svg and related properties, e.g., canvas size and margins.
     */
    initCanvas() {
        while (this.dom.firstChild) {
            this.dom.removeChild(this.dom.lastChild)
        }

        this.canvas = this.canvasDimensions()
        const container = this.createOuterContainerWithStyle(
            this.canvas.outer.width,
            this.canvas.outer.height
        )
        this.dom.appendChild(container)

        this.svg = d3
            .select(container)
            .append('svg')
            .attr('width', this.canvas.outer.width)
            .attr('height', this.canvas.outer.height)
            .append('g')
            .attr(
                'transform',
                'translate(' + this.canvas.margin.left + ',' + this.canvas.margin.top + ')'
            )
    }

    /**
     * Initialise the heatmap with the current data and settings.
     */
    initHeatmap() {
        let data = this.data()
        if (ok(data.length) && data.length === 2 && Array.isArray(data[1])) {
            let indices = Array.from(Array(data[1].length).keys())
            data = [data[0], indices, data[1]]
        } else if (!Array.isArray(data[0])) {
            let indices = Array.from(Array(data.length).keys())
            data = [indices, [], data]
        } else if (ok(data.length) && data.length === 1 && Array.isArray(data[0])) {
            let indices = Array.from(Array(data[0].length).keys())
            data = [indices, [], data[0]]
        }

        // Labels of row and columns
        let myGroups = d3.map(data[0], d => d).keys()
        let myVars = d3.map(data[1], d => d).keys()
        let labelStyle = 'font-family: DejaVuSansMonoBook; font-size: 10px;'
        let self = this

        // Build X scales and axis:
        let x = d3.scaleBand().range([0, this.canvas.inner.width]).domain(myGroups).padding(0.05)
        let xAxis = d3
            .axisBottom(x)
            .tickSize(0)
            .tickValues(
                myGroups.filter((d, i) => {
                    if (i === myGroups.length - 1) {
                        return 1
                    }
                    let divisor = (5 * self.canvas.outer.width) / 200
                    return !(i % Math.round(myGroups.length / divisor))
                })
            )

        this.svg
            .append('g')
            .attr('style', labelStyle)
            .attr('transform', 'translate(0,' + this.canvas.inner.height + ')')
            .call(xAxis)
            .select('.domain')
            .remove()

        // Build Y scales and axis:
        let y = d3.scaleBand().range([this.canvas.inner.height, 0]).domain(myVars).padding(0.05)
        let yAxis = d3
            .axisLeft(y)
            .tickSize(0)
            .tickValues(
                myVars.filter((d, i) => {
                    if (i === myVars.length - 1) {
                        return 1
                    }
                    let divisor = (9 * self.canvas.outer.height) / 200
                    return !(i % Math.round(myVars.length / divisor))
                })
            )
        this.svg.append('g').attr('style', labelStyle).call(yAxis).select('.domain').remove()

        // Build color scale
        let fill = d3
            .scaleSequential()
            .interpolator(d3.interpolateViridis)
            .domain([0, d3.max(data[2], d => d)])

        let indices = Array.from(Array(data[0].length).keys())

        this.svg
            .selectAll()
            .data(indices, d => data[0][d] + ':' + data[1][d])
            .enter()
            .append('rect')
            .attr('x', d => x(data[0][d]))
            .attr('y', d => y(data[1][d]))
            .attr('rx', 4)
            .attr('ry', 4)
            .attr('width', x.bandwidth())
            .attr('height', y.bandwidth())
            .style('fill', d => fill(data[2][d]))
            .style('stroke-width', 4)
            .style('stroke', 'none')
            .style('opacity', 0.8)
    }

    /**
     * Sets size of the main parent DOM object.
     */
    setSize(size) {
        while (this.dom.firstChild) {
            this.dom.removeChild(this.dom.lastChild)
        }

        this.dom.setAttributeNS(null, 'width', size[0])
        this.dom.setAttributeNS(null, 'height', size[1])
        this.initCanvas()
        this.initHeatmap()
    }
}

/**
 * Checks if `t` has defined type and is not null.
 */
function ok(t) {
    return t !== undefined && t !== null
}

return Heatmap
