/** ScatterPlot Visualization. */
// TODO refactor this to avoid loading on startup. See issue #985 .
loadScript('https://d3js.org/d3.v4.min.js')
loadStyle('https://fontlibrary.org/face/dejavu-sans-mono')

let shortcuts = {
    zoomIn: e => (e.ctrlKey || e.metaKey) && e.key === 'z',
    showAll: e => (e.ctrlKey || e.metaKey) && event.key === 'a',
}

const LABEL_STYLE = 'font-family: DejaVuSansMonoBook; font-size: 10px;'
const X_AXIS_LABEL_WIDTH = 30
const POINT_LABEL_PADDING_X = 7
const POINT_LABEL_PADDING_Y = 2
const ANIMATION_DURATION = 1000
const LINEAR_SCALE = 'linear'
const LOGARITHMIC_SCALE = 'logarithmic'
const VISIBLE_POINTS = 'visible'
const BUTTONS_HEIGHT = 25
const DEFAULT_LIMIT = 1024

/**
 * A d3.js ScatterPlot visualization.
 *
 * To zoom use scroll wheel.
 * To select click and swipe with LMB.
 * To deselect click outside of selection with LMB.
 * To pan click and swipe with RMB.
 * To zoom out click "Fit all" or use key combination "ctrl/cmd+a".
 * To zoom into selection click appropriate button or use key combination "ctrl/cmd+z".
 *
 * Data format (JSON):
 * {
 *  "axis":{
 *     "x":{"label":"x-axis label""scale":"linear"},
 *     "y":{"label":"y-axis label","scale":"logarithmic"},
 *  },
 *  "focus":{"x":1.7,"y":2.1,"zoom":3.0},
 *  "points":{"labels":"visible" | "invisible"},
 *  "data":[
 *     {"x":0.1,"y":0.7,"label":"foo","color":"FF0000","shape":"circle","size":0.2},
 *     ...
 *     {"x":0.4,"y":0.2,"label":"baz","color":"0000FF","shape":"square","size":0.3}
 *  ]
 * }
 */
class ScatterPlot extends Visualization {
    static inputType = 'Standard.Table.Data.Table.Table | Standard.Base.Data.Vector.Vector'
    static label = 'Scatter Plot'

    constructor(data) {
        super(data)
        this.bounds = null
        this.limit = DEFAULT_LIMIT
        this.updatePreprocessor()
        this.dataPoints = []
        this.axis = {
            x: { scale: LINEAR_SCALE },
            y: { scale: LINEAR_SCALE },
        }
        this.points = { labels: VISIBLE_POINTS }
    }

    updatePreprocessor() {
        let args = []
        if (this.bounds) {
            args.push('[' + this.bounds.join(',') + ']')
        } else {
            args.push('Nothing')
        }
        args.push(this.limit.toString())
        this.setPreprocessor('Standard.Visualization.Scatter_Plot', 'process_to_json_text', ...args)
    }

    /**
     * Presents a scatterplot visualization after receiving `data`.
     */
    onDataReceived(data) {
        while (this.dom.firstChild) {
            this.dom.removeChild(this.dom.lastChild)
        }
        let parsedData = this.parseData(data)
        this.updateState(parsedData)
        this.drawScatterplot()
    }

    canvasWidth() {
        return this.dom.getAttributeNS(null, 'width')
    }

    canvasHeight() {
        let height = this.dom.getAttributeNS(null, 'height')
        return height - BUTTONS_HEIGHT
    }

    updateState(parsedData) {
        this.axis = parsedData.axis ?? {
            x: { scale: LINEAR_SCALE },
            y: { scale: LINEAR_SCALE },
        }
        this.focus = parsedData.focus
        this.points = parsedData.points ?? { labels: 'visible' }
        this.dataPoints = this.extractValues(parsedData)
        this.dataPoints = this.dataPoints.filter(pt => isValidNumber(pt.x) && isValidNumber(pt.y))
        this.updateBox()
    }

    updateBox() {
        this.margin = this.getMargins(this.axis)
        this.boxWidth = this.canvasWidth() - this.margin.left - this.margin.right
        this.boxHeight = this.canvasHeight() - this.margin.top - this.margin.bottom
    }

    extractValues(data) {
        /// Note this is a workaround for #1006, we allow raw arrays and JSON objects to be consumed.
        if (Array.isArray(data)) {
            return this.arrayAsValues(data)
        }
        return data.data ?? []
    }

    /**
     * Return a array of values as valid scatterplot input.
     * Uses the values of the array as y-coordinate and the index of the value as its x-coordinate.
     */
    arrayAsValues(dataArray) {
        return dataArray.map((y, ix) => {
            return { x: ix, y }
        })
    }

    parseData(data) {
        let parsedData
        /// Note this is a workaround for #1006, we allow raw arrays and JSON objects to be consumed.
        if (typeof data === 'string' || data instanceof String) {
            parsedData = JSON.parse(data)
        } else {
            parsedData = data
        }
        return parsedData
    }

    drawScatterplot() {
        const divElem = this.createDivElem(this.canvasWidth(), this.canvasHeight())
        this.dom.appendChild(divElem)
        let svg = d3
            .select(divElem)
            .append('svg')
            .attr('width', this.canvasWidth())
            .attr('height', this.canvasHeight())
            .append('g')
            .attr('transform', 'translate(' + this.margin.left + ',' + this.margin.top + ')')

        let extremesAndDeltas = this.getExtremesAndDeltas(this.dataPoints)
        let scaleAndAxis = this.createAxes(
            this.axis,
            extremesAndDeltas,
            this.boxWidth,
            this.boxHeight,
            svg,
            focus
        )
        this.createLabels(this.axis, svg, this.boxWidth, this.margin, this.boxHeight)
        let scatter = this.createScatter(
            svg,
            this.boxWidth,
            this.boxHeight,
            this.points,
            this.dataPoints,
            scaleAndAxis
        )
        let zoom = this.addPanAndZoom(
            this.boxWidth,
            this.boxHeight,
            svg,
            this.margin,
            scaleAndAxis,
            scatter,
            this.points
        )

        // TODO [MM]: In task specification buttons were on top of the visualization, but because
        //            the visualization selector obfuscated them, they're now on the bottom.
        //            This should be fixed in (#898).
        this.createButtonFitAll(scatter, this.points, extremesAndDeltas, zoom, this.boxWidth)
        let selectedZoomBtn = this.createButtonScaleToPoints()
        this.addBrushing(this.boxWidth, this.boxHeight, scatter, selectedZoomBtn, this.points, zoom)
    }

    /**
     * Adds panning and zooming functionality to the visualization.
     */
    addPanAndZoom(boxWidth, boxHeight, svg, margin, scaleAndAxis, scatter, points) {
        const minScale = 0.5
        const maxScale = 20
        const rightButton = 2
        const midButton = 1
        const midButtonClicked = 4
        const scrollWheel = 0
        const extent = [minScale, maxScale]
        let startPos
        const zoom = d3
            .zoom()
            .filter(function () {
                switch (d3.event.type) {
                    case 'mousedown':
                        return d3.event.button === rightButton || d3.event.button === midButton
                    case 'wheel':
                        return d3.event.button === scrollWheel
                    default:
                        return false
                }
            })
            .wheelDelta(function () {
                const event = d3.event
                const minDelta = 0.002
                const medDelta = 0.05
                const maxDelta = 1
                const wheelSpeedMultiplier =
                    event.deltaMode === 1 ? medDelta : event.deltaMode ? maxDelta : minDelta
                return -event.deltaY * wheelSpeedMultiplier
            })
            .scaleExtent(extent)
            .extent([
                [0, 0],
                [boxWidth, boxHeight],
            ])
            .on('zoom', zoomed)
            .on('start', startZoom)

        const zoomElem = scatter
            .append('g')
            .attr('class', 'zoom')
            .attr('width', boxWidth)
            .attr('height', boxHeight)
            .style('fill', 'none')
            .call(zoom)

        let transformedScale = Object.assign({}, scaleAndAxis)
        let tempRmbScale = Object.assign({}, scaleAndAxis)
        const self = this

        /**
         * Helper function called on pan/scroll.
         */
        function zoomed() {
            function rescale(distanceScale) {
                transformedScale.xScale = distanceScale.rescaleX(transformedScale.xScale)
                transformedScale.yScale = distanceScale.rescaleY(transformedScale.yScale)
            }

            function getScaleForZoom(scale, focus) {
                return d3.zoomIdentity
                    .translate(focus.x - self.margin.left, focus.y - self.margin.top)
                    .scale(scale)
                    .translate(-focus.x + self.margin.left, -focus.y + self.margin.top)
            }

            if (d3.event.sourceEvent != null && d3.event.sourceEvent.buttons === rightButton) {
                transformedScale.xScale = tempRmbScale.xScale
                transformedScale.yScale = tempRmbScale.yScale
                const rmbDivider = 100.0
                const zoomAmount = rmbZoomValue(d3.event.sourceEvent) / rmbDivider
                const scale = Math.exp(zoomAmount)
                const distanceScale = getScaleForZoom(scale, startPos)
                rescale(distanceScale)
            } else if (d3.event.sourceEvent != null && d3.event.sourceEvent.type === 'wheel') {
                if (d3.event.sourceEvent.ctrlKey) {
                    const pinchDivider = 100.0
                    const zoomAmount = -d3.event.sourceEvent.deltaY / pinchDivider
                    const scale = Math.exp(zoomAmount)
                    const distanceScale = getScaleForZoom(scale, startPos)
                    rescale(distanceScale)
                } else {
                    const distanceScale = d3.zoomIdentity.translate(
                        -d3.event.sourceEvent.deltaX,
                        -d3.event.sourceEvent.deltaY
                    )
                    rescale(distanceScale)
                }
            } else if (
                d3.event.sourceEvent != null &&
                d3.event.sourceEvent.buttons === midButtonClicked
            ) {
                const movementFactor = 2
                const distanceScale = d3.zoomIdentity.translate(
                    d3.event.sourceEvent.movementX / movementFactor,
                    d3.event.sourceEvent.movementY / movementFactor
                )
                rescale(distanceScale)
            } else {
                rescale(d3.event.transform)
            }

            scaleAndAxis.xAxis.call(
                d3
                    .axisBottom(transformedScale.xScale)
                    .ticks((5 * self.dom.getAttributeNS(null, 'width')) / 200)
            )
            scaleAndAxis.yAxis.call(
                d3
                    .axisLeft(transformedScale.yScale)
                    .ticks((10 * self.dom.getAttributeNS(null, 'height')) / 200)
            )
            scatter
                .selectAll('path')
                .attr(
                    'transform',
                    d =>
                        'translate(' +
                        transformedScale.xScale(d.x) +
                        ',' +
                        transformedScale.yScale(d.y) +
                        ')'
                )

            if (points.labels === VISIBLE_POINTS) {
                scatter
                    .selectAll('text')
                    .attr('x', d => transformedScale.xScale(d.x) + POINT_LABEL_PADDING_X)
                    .attr('y', d => transformedScale.yScale(d.y) + POINT_LABEL_PADDING_Y)
            }
        }

        /**
         * Return the position of this event in local canvas coordinates.
         */
        function getPos(event) {
            if (event != null) {
                return { x: event.offsetX, y: event.offsetY }
            }
            return { x: 0, y: 0 }
        }

        /**
         * Return the zoom value computed from the initial right-mouse-button event to the current
         * right-mouse event.
         */
        function rmbZoomValue(event) {
            const end = getPos(event)
            const dX = end.x - startPos.x
            const dY = end.y - startPos.y
            return dX - dY
        }

        /**
         * Helper function called when starting to pan/scroll.
         */
        function startZoom() {
            startPos = getPos(d3.event.sourceEvent)
            tempRmbScale = Object.assign({}, transformedScale)
        }

        return { zoomElem, zoom, transformedScale }
    }

    /** Removing `pointer-events` handling from brush element, as we want it to be inherited. D3 inserts
     * `pointer-events: all` in the brush element and some of its children on brush creation and after brushing ends.
     * There is no documentation on that topic as far as we are aware, so this was observed and tested manually. */
    removePointerEventsAttrsFromBrush(brushElem) {
        brushElem.attr('pointer-events', null)
        brushElem.select(function () {
            for (const child of this.childNodes) {
                child.removeAttribute('pointer-events')
            }
        })
    }

    /**
     * Adds brushing functionality to the plot.
     *
     * Brush is a tool which enables user to select points, and zoom into selection via
     * keyboard shortcut or button event.
     */
    addBrushing(boxWidth, boxHeight, scatter, selectedZoomBtn, points, zoom) {
        let extent
        let brushClass = 'brush'

        let brush = d3
            .brush()
            .extent([
                [0, 0],
                [boxWidth, boxHeight],
            ])
            .on('start ' + brushClass, updateChart)

        // The brush element must be child of zoom element - this is only way we found to have both zoom and brush
        // events working at the same time. See https://stackoverflow.com/a/59757276 .
        let brushElem = zoom.zoomElem.append('g').attr('class', brushClass).call(brush)

        this.removePointerEventsAttrsFromBrush(brushElem)
        let self = this

        /**
         * Zooms into selected fragment of plot.
         *
         * Based on https://www.d3-graph-gallery.com/graph/interactivity_brush.html
         * Section "Brushing for zooming".
         */
        const zoomIn = () => {
            let xMin = zoom.transformedScale.xScale.invert(extent[0][0])
            let xMax = zoom.transformedScale.xScale.invert(extent[1][0])
            let yMin = zoom.transformedScale.yScale.invert(extent[1][1])
            let yMax = zoom.transformedScale.yScale.invert(extent[0][1])

            this.bounds = [xMin, yMin, xMax, yMax]
            this.updatePreprocessor()

            zoom.transformedScale.xScale.domain([xMin, xMax])
            zoom.transformedScale.yScale.domain([yMin, yMax])

            self.zoomingHelper(zoom.transformedScale, boxWidth, scatter, points)
        }

        const zoomInKeyEvent = event => {
            if (shortcuts.zoomIn(event)) {
                zoomIn()
                endBrushing()
            }
        }

        /**
         * Updates plot when brushing.
         */
        function updateChart() {
            let selectionEvent = d3.event.selection
            selectedZoomBtn.style.display = 'inline-block'
            selectedZoomBtn.addEventListener('click', zoomIn, true)
            document.addEventListener('keydown', zoomInKeyEvent, true)
            extent = selectionEvent
        }

        /**
         * Removes brush, keyboard event and zoom button when end event is captured.
         */
        const endBrushing = () => {
            brushElem.call(brush.move, null)
            selectedZoomBtn.style.display = 'none'
            selectedZoomBtn.removeEventListener('click', zoomIn, true)
            document.removeEventListener('keydown', zoomInKeyEvent, true)
            this.removePointerEventsAttrsFromBrush(brushElem)
        }

        let endEvents = ['click', 'auxclick', 'contextmenu', 'scroll']
        endEvents.forEach(e => document.addEventListener(e, endBrushing, false))
    }

    /**
     * Helper function for zooming in after the scale has been updated.
     */
    zoomingHelper(scaleAndAxis, boxWidth, scatter, points) {
        scaleAndAxis.xAxis
            .transition()
            .duration(ANIMATION_DURATION)
            .call(
                d3
                    .axisBottom(scaleAndAxis.xScale)
                    .ticks((5 * this.dom.getAttributeNS(null, 'width')) / 200)
            )
        scaleAndAxis.yAxis
            .transition()
            .duration(ANIMATION_DURATION)
            .call(
                d3
                    .axisLeft(scaleAndAxis.yScale)
                    .ticks((10 * this.dom.getAttributeNS(null, 'height')) / 200)
            )

        scatter
            .selectAll('path')
            .transition()
            .duration(ANIMATION_DURATION)
            .attr(
                'transform',
                d => 'translate(' + scaleAndAxis.xScale(d.x) + ',' + scaleAndAxis.yScale(d.y) + ')'
            )

        if (points.labels === VISIBLE_POINTS) {
            scatter
                .selectAll('text')
                .transition()
                .duration(ANIMATION_DURATION)
                .attr('x', d => scaleAndAxis.xScale(d.x) + POINT_LABEL_PADDING_X)
                .attr('y', d => scaleAndAxis.yScale(d.y) + POINT_LABEL_PADDING_Y)
        }
    }

    /**
     * Creates a plot object and populates it with given data.
     */
    createScatter(svg, boxWidth, boxHeight, points, dataPoints, scaleAndAxis) {
        let clip = svg
            .append('defs')
            .append('svg:clipPath')
            .attr('id', 'clip')
            .append('svg:rect')
            .attr('width', boxWidth)
            .attr('height', boxHeight)
            .attr('x', 0)
            .attr('y', 0)

        let symbol = d3.symbol()

        let scatter = svg.append('g').attr('clip-path', 'url(#clip)')

        let sizeScaleMultiplier = 100

        let color = this.theme.get('accent')
        let fillColor = `rgba(${color.red * 255},${color.green * 255},${color.blue * 255},0.8)`

        scatter
            .selectAll('dataPoint')
            .data(dataPoints)
            .enter()
            .append('path')
            .attr(
                'd',
                symbol.type(this.matchShape()).size(d => (d.size ?? 1.0) * sizeScaleMultiplier)
            )
            .attr(
                'transform',
                d => 'translate(' + scaleAndAxis.xScale(d.x) + ',' + scaleAndAxis.yScale(d.y) + ')'
            )
            .style('fill', d => d.color ?? fillColor)

        if (points.labels === VISIBLE_POINTS) {
            scatter
                .selectAll('dataPoint')
                .data(dataPoints)
                .enter()
                .append('text')
                .text(d => d.label)
                .attr('x', d => scaleAndAxis.xScale(d.x) + POINT_LABEL_PADDING_X)
                .attr('y', d => scaleAndAxis.yScale(d.y) + POINT_LABEL_PADDING_Y)
                .attr('style', LABEL_STYLE)
                .attr('fill', 'black')
        }

        return scatter
    }

    /**
     * Helper function to match d3 shape from string.
     */
    matchShape() {
        return d => {
            if (d.shape === 'cross') {
                return d3.symbolCross
            }
            if (d.shape === 'diamond') {
                return d3.symbolDiamond
            }
            if (d.shape === 'square') {
                return d3.symbolSquare
            }
            if (d.shape === 'star') {
                return d3.symbolStar
            }
            if (d.shape === 'triangle') {
                return d3.symbolTriangle
            }
            return d3.symbolCircle
        }
    }

    /**
     * Creates labels on axes if they're defined.
     */
    createLabels(axis, svg, boxWidth, margin, boxHeight) {
        let fontStyle = '10px DejaVuSansMonoBook'
        if (axis.x.label !== undefined) {
            let paddingY = 20
            svg.append('text')
                .attr('text-anchor', 'end')
                .attr('style', LABEL_STYLE)
                .attr('x', margin.left + this.getTextWidth(axis.x.label, fontStyle) / 2)
                .attr('y', boxHeight + margin.top + paddingY)
                .text(axis.x.label)
        }

        if (axis.y.label !== undefined) {
            let paddingY = 15
            svg.append('text')
                .attr('text-anchor', 'end')
                .attr('style', LABEL_STYLE)
                .attr('transform', 'rotate(-90)')
                .attr('y', -margin.left + paddingY)
                .attr(
                    'x',
                    -margin.top - boxHeight / 2 + this.getTextWidth(axis.y.label, fontStyle) / 2
                )
                .text(axis.y.label)
        }
    }

    /**
     * Helper function to get text width to make sure that labels on x axis wont overlap,
     * and keeps it readable.
     */
    getTextWidth(text, font) {
        const canvas = document.createElement('canvas')
        const context = canvas.getContext('2d')
        context.font = font
        const metrics = context.measureText('  ' + text)
        return metrics.width
    }

    /**
     * Construct either linear or logarithmic D3 scale.
     *
     * The scale kind is selected depending on update contents.
     *
     * @param axis Axis information as received in the visualization update.
     * @returns D3 scale.
     */
    axisD3Scale(axis) {
        switch (axis?.scale) {
            case LINEAR_SCALE:
                return d3.scaleLinear()
            case LOGARITHMIC_SCALE:
                return d3.scaleLog()
            default:
                return d3.scaleLinear()
        }
    }

    /**
     * Creates plot's axes.
     */
    createAxes(axis, extremesAndDeltas, boxWidth, boxHeight, svg, focus) {
        let { domainX, domainY } = this.getDomains(extremesAndDeltas, focus)

        let xScale = this.axisD3Scale(axis?.x)
        xScale.domain(domainX).range([0, boxWidth])
        let xAxis = svg
            .append('g')
            .attr('transform', 'translate(0,' + boxHeight + ')')
            .attr('style', LABEL_STYLE)
            .call(d3.axisBottom(xScale).ticks((5 * this.dom.getAttributeNS(null, 'width')) / 200))

        let yScale = this.axisD3Scale(axis?.y)
        yScale.domain(domainY).range([boxHeight, 0])
        let yAxis = svg
            .append('g')
            .attr('style', LABEL_STYLE)
            .call(d3.axisLeft(yScale).ticks((10 * this.dom.getAttributeNS(null, 'height')) / 200))
        return { xScale: xScale, yScale: yScale, xAxis: xAxis, yAxis: yAxis }
    }

    /**
     * Helper function calculating domains for x and y axes.
     *
     * Example:
     * Lets say we have a bunch of points. Those points will have some minimum and maximum value,
     * from which we can calculate the span of points on X and Y axis, hence domain, with added
     * padding to make sure points will fit nicely on the chart.
     */
    getDomains(extremesAndDeltas, focus) {
        let domainX = [
            extremesAndDeltas.xMin - extremesAndDeltas.paddingX,
            extremesAndDeltas.xMax + extremesAndDeltas.paddingX,
        ]
        let domainY = [
            extremesAndDeltas.yMin - extremesAndDeltas.paddingY,
            extremesAndDeltas.yMax + extremesAndDeltas.paddingY,
        ]

        if (focus !== undefined) {
            if (focus.x !== undefined && focus.y !== undefined && focus.zoom !== undefined) {
                let newPaddingX = extremesAndDeltas.dx * (1 / (2 * focus.zoom))
                let newPaddingY = extremesAndDeltas.dy * (1 / (2 * focus.zoom))
                domainX = [focus.x - newPaddingY, focus.x + newPaddingY]
                domainY = [focus.y - newPaddingY, focus.y + newPaddingY]
            }
        }
        return { domainX, domainY }
    }

    /**
     * Helper function calculating extreme values and paddings to make sure data will fit nicely.
     *
     * It traverses through data getting minimal and maximal values, and calculates padding based on
     * span calculated from above values, multiplied by 10% so that the plot is a little bit smaller
     * than the container.
     */
    getExtremesAndDeltas(dataPoints) {
        let xMin = dataPoints[0]?.x ?? 0
        let xMax = dataPoints[0]?.x ?? 0
        let yMin = dataPoints[0]?.y ?? 0
        let yMax = dataPoints[0]?.y ?? 0

        dataPoints.forEach(d => {
            if (d.x < xMin) {
                xMin = d.x
            }
            if (d.x > xMax) {
                xMax = d.x
            }
            if (d.y < yMin) {
                yMin = d.y
            }
            if (d.y > yMax) {
                yMax = d.y
            }
        })

        let dx = xMax - xMin
        let dy = yMax - yMin

        let paddingX = 0.1 * dx
        let paddingY = 0.1 * dy

        return { xMin, xMax, yMin, yMax, paddingX, paddingY, dx, dy }
    }

    /**
     * Helper function getting margins for plot's box.
     */
    getMargins(axis) {
        let x_label = axis?.x?.label
        let y_label = axis?.y?.label
        if (x_label === undefined && y_label === undefined) {
            return { top: 20, right: 20, bottom: 20, left: 45 }
        } else if (y_label === undefined) {
            return { top: 10, right: 20, bottom: 35, left: 35 }
        } else if (x_label === undefined) {
            return { top: 20, right: 10, bottom: 20, left: 55 }
        }
        return { top: 10, right: 10, bottom: 35, left: 55 }
    }

    /**
     * Creates HTML div element as container for plot.
     */
    createDivElem(width, height) {
        const divElem = document.createElementNS(null, 'div')
        divElem.setAttributeNS(null, 'class', 'vis-scatterplot')
        divElem.setAttributeNS(null, 'viewBox', 0 + ' ' + 0 + ' ' + width + ' ' + height)
        divElem.setAttributeNS(null, 'width', '100%')
        divElem.setAttributeNS(null, 'height', '100%')
        divElem.setAttributeNS(null, 'transform', 'matrix(1 0 0 -1 0 0)')

        const addStyleToElem = (attr, stl) => {
            let style = document.createElement('style')
            style.innerText = attr + '{' + stl + '}'

            divElem.appendChild(style)
        }

        let darkStrokeColor = `rgba(255,255,255,0.7)`
        let buttonLightColor = `#333`
        let darkBtnHoverColor = `rgba(255,255,255,0.5)`
        let darkSelectionFill = `#efefef`

        addStyleToElem('.selection', 'rx: 4px;stroke: transparent;')
        addStyleToElem(
            'button',
            `
            margin-left: 5px;
            margin-bottom: 5px;
            display: inline-block;
            padding: 2px 10px;
            outline: none;
            background-color: transparent;
            border: 1px solid ${buttonLightColor};
            color: ${buttonLightColor};
            border-radius: 14px;
            font-size: 10px;
            font-family: DejaVuSansMonoBook;
            vertical-align: top;
            transition: all 0.3s ease;
        `
        )
        addStyleToElem(
            'button:hover',
            `
            background-color: ${buttonLightColor};
            color: ${darkSelectionFill};
        `
        )

        addStyleToElem(
            '.dark-theme button',
            `
            border: 0;
            background-color: ${darkStrokeColor};
        `
        )
        addStyleToElem(
            '.dark-theme button:hover',
            `
            background-color: ${darkBtnHoverColor};
        `
        )
        addStyleToElem('.dark-theme .selection', `fill: ${darkSelectionFill}`)
        addStyleToElem('.dark-theme line', `stroke: ${darkStrokeColor};`)
        addStyleToElem('.dark-theme .domain', `stroke: ${darkStrokeColor};`)
        addStyleToElem('.dark-theme text', `fill: ${darkStrokeColor};`)

        return divElem
    }

    /**
     * Helper function for button creation.
     */
    createBtnHelper() {
        const btn = document.createElement('button')
        btn.setAttribute('width', '80px')
        btn.setAttribute('height', '20px')
        return btn
    }

    /**
     * Creates a button to fit all points on plot.
     */
    createButtonFitAll(scatter, points, extremesAndDeltas, zoom, boxWidth) {
        const btn = this.createBtnHelper()

        let text = document.createTextNode('Fit all')
        btn.appendChild(text)

        let self = this
        const unzoom = () => {
            zoom.zoomElem.transition().duration(0).call(zoom.zoom.transform, d3.zoomIdentity)

            let domainX = [
                extremesAndDeltas.xMin - extremesAndDeltas.paddingX,
                extremesAndDeltas.xMax + extremesAndDeltas.paddingX,
            ]
            let domainY = [
                extremesAndDeltas.yMin - extremesAndDeltas.paddingY,
                extremesAndDeltas.yMax + extremesAndDeltas.paddingY,
            ]

            zoom.transformedScale.xScale.domain(domainX)
            zoom.transformedScale.yScale.domain(domainY)

            self.zoomingHelper(zoom.transformedScale, boxWidth, scatter, points)

            self.bounds = null
            self.limit = DEFAULT_LIMIT
            self.updatePreprocessor()
        }

        document.addEventListener('keydown', e => {
            if (shortcuts.showAll(e)) {
                unzoom()
            }
        })

        btn.addEventListener('click', unzoom)
        this.dom.appendChild(btn)
    }

    /**
     * Creates a button to zoom into brushed fragment of plot.
     */
    createButtonScaleToPoints() {
        const btn = this.createBtnHelper()
        let text = document.createTextNode('Zoom to selected')
        btn.appendChild(text)
        btn.setAttribute('width', '120px')
        btn.style.display = 'none'
        this.dom.appendChild(btn)
        return btn
    }

    /**
     * Sets size of this DOM object.
     */
    setSize(size) {
        while (this.dom.firstChild) {
            this.dom.removeChild(this.dom.lastChild)
        }

        this.dom.setAttributeNS(null, 'width', size[0])
        this.dom.setAttributeNS(null, 'height', size[1])
        this.updateBox()
        this.drawScatterplot()
    }
}

return ScatterPlot
