/** SQL visualization. */

// =================
// === Constants ===
// =================

/** The qualified name of the Text type. */
const textType = 'Builtins.Main.Text'

/** The module prefix added for unknown SQL types. */
const customSqlTypePrefix = 'Standard.Database.Data.SQL.SQL_Type.'

/** Specifies opacity of interpolation background color. */
const interpolationBacgroundOpacity = 0.3

/** The CSS styles for the visualization. */
const visualizationStyle = `
    <style>
    .sql {
        font-family: DejaVuSansMonoBook, sans-serif;
        font-size: 12px;
        margin-left: 7px;
        margin-top: 5px;
    }
    .interpolation {
        border-radius: 6px;
        padding:1px 2px 1px 2px;
        display: inline;
    }
    .mismatch-parent {
        position: relative;
        display: inline-flex;
        justify-content: center;
    }
    .mismatch-mouse-area {
        display: inline;
        position: absolute;
        width: 150%;
        height: 150%;
        align-self: center;
        z-index: 0;
    }
    .mismatch {
        z-index: 1;
    }
    .modulepath {
        color: rgba(150, 150, 150, 0.9);
    }
    .tooltip {
        font-family: DejaVuSansMonoBook, sans-serif;
        font-size: 12px;
        opacity: 0;
        transition: opacity 0.2s;
        display: inline-block;
        white-space: nowrap;
        background-color: rgba(249, 249, 249, 1);
        box-shadow: 0 0 16px rgba(0, 0, 0, 0.16);
        text-align: left;
        border-radius: 6px;
        padding: 5px;
        position: absolute;
        z-index: 99999;
        pointer-events: none;
    }
    </style>
    `

// =============================
// === Script Initialisation ===
// =============================

loadScript('https://cdnjs.cloudflare.com/ajax/libs/sql-formatter/4.0.2/sql-formatter.min.js')

// ===========================
// === Table visualization ===
// ===========================

/**
 * A visualization that pretty-prints generated SQL code and displays type hints related to
 * interpolated query parameters.
 */
class SqlVisualization extends Visualization {
    // TODO Change the type below once #837 is done:
    // 'Standard.Database.Data.Table.Table | Standard.Database.Data.Column.Column'
    static inputType = 'Standard.Database.Data.Table.Table | Standard.Database.Data.Column.Column'
    static label = 'SQL Query'

    constructor(api) {
        super(api)
        this.setPreprocessor('Standard.Visualization.SQL.Visualization', 'prepare_visualization')
    }

    onDataReceived(data) {
        this.removeAllChildren()

        let parsedData = data
        if (typeof data === 'string') {
            parsedData = JSON.parse(data)
        }

        let visHtml = visualizationStyle
        if (parsedData.error !== undefined) {
            visHtml += '<pre class="sql">' + parsedData.error + '</pre>'
        } else {
            const params = parsedData.interpolations.map(param =>
                renderInterpolationParameter(this.theme, param)
            )

            let language = 'sql'
            if (parsedData.dialect === 'postgresql') {
                language = 'postgresql'
            }

            const formatted = sqlFormatter.format(parsedData.code, {
                params: params,
                language: language,
            })

            const codeRepresentation = '<pre class="sql">' + formatted + '</pre>'
            visHtml += codeRepresentation
        }

        const containers = this.createContainers()
        const parentContainer = containers.parent
        containers.scrollable.innerHTML = visHtml
        this.dom.appendChild(parentContainer)

        const tooltip = new Tooltip(parentContainer)
    }

    /**
     * Removes all children of this visualization's DOM.
     *
     * May be used to reset the visualization's content.
     */
    removeAllChildren() {
        while (this.dom.firstChild) {
            this.dom.removeChild(this.dom.lastChild)
        }
    }

    /**
     * Creates containers for the visualization.
     */
    createContainers() {
        const parentContainer = document.createElement('div')
        parentContainer.setAttributeNS(null, 'style', 'position: relative;')
        const width = this.dom.getAttributeNS(null, 'width')
        const height = this.dom.getAttributeNS(null, 'height')
        const scrollable = document.createElement('div')
        scrollable.setAttributeNS(null, 'id', 'vis-sql-view')
        scrollable.setAttributeNS(null, 'class', 'scrollable')
        scrollable.setAttributeNS(null, 'viewBox', '0 0 ' + width + ' ' + height)
        scrollable.setAttributeNS(null, 'width', '100%')
        scrollable.setAttributeNS(null, 'height', '100%')
        const viewStyle = `width: ${width - 5}px;
             height: ${height - 5}px;
             overflow: scroll;
             padding:2.5px;`
        scrollable.setAttributeNS(null, 'style', viewStyle)
        parentContainer.appendChild(scrollable)
        return {
            parent: parentContainer,
            scrollable: scrollable,
        }
    }

    setSize(size) {
        this.dom.setAttributeNS(null, 'width', size[0])
        this.dom.setAttributeNS(null, 'height', size[1])
    }
}

// === Handling Colors ===

/**
 * Renders a 4-element array representing a color into a CSS-compatible rgba string.
 */
function convertColorToRgba(color) {
    const r = 255 * color.red
    const g = 255 * color.green
    const b = 255 * color.blue
    const a = color.alpha
    return 'rgba(' + r + ',' + g + ',' + b + ',' + a + ')'
}

/** Replaces the alpha component of a color (represented as a 4-element array),
 * returning a new color.
 */
function replaceAlpha(color, newAlpha) {
    return {
        red: color.red,
        green: color.green,
        blue: color.blue,
        alpha: newAlpha,
    }
}

// === HTML Rendering Helpers ===

/**
 * Renders a HTML representation of a message to be displayed in a tooltip,
 * which explains a type mismatch.
 */
function renderTypeHintMessage(
    receivedTypeName,
    expectedTypeName,
    receivedTypeColor,
    expectedTypeColor
) {
    const received = new QualifiedTypeName(receivedTypeName)
    const expected = new QualifiedTypeName(expectedTypeName)

    let receivedPrefix = ''
    if (received.moduleName !== null) {
        receivedPrefix = '<span class="modulepath">' + received.moduleName + '.</span>'
    }
    const receivedStyledSpan = '<span style="color: ' + convertColorToRgba(receivedTypeColor) + '">'
    const receivedSuffix = receivedStyledSpan + received.name + '</span>'

    let expectedPrefix = ''
    if (expected.moduleName !== null) {
        expectedPrefix = '<span class="modulepath">' + expected.moduleName + '.</span>'
    }
    const expectedStyledSpan = '<span style="color: ' + convertColorToRgba(expectedTypeColor) + '">'
    const expectedSuffix = expectedStyledSpan + expected.name + '</span>'

    let message = 'Received ' + receivedPrefix + receivedSuffix + '<br>'
    message += 'Expected ' + expectedPrefix + expectedSuffix + '<br>'
    message += 'The database may perform an auto conversion.'
    return message
}

/**
 * Wraps a qualified type name.
 *
 * The `moduleName` field is the name of the module the type is from. It may be null if the original
 * type name did not contain a module name.
 * The `name` field is the simple name of the type itself.
 */
class QualifiedTypeName {
    /** Creates a QualifiedTypeName instance from a string representation. */
    constructor(typeName) {
        let ix = typeName.lastIndexOf('.')
        if (ix < 0) {
            this.moduleName = null
            this.name = typeName
        } else {
            this.moduleName = typeName.substr(0, ix)
            this.name = typeName.substr(ix + 1)
        }
    }
}

/**
 * Renders HTML for displaying an Enso parameter that is interpolated into the SQL code.
 */
function renderInterpolationParameter(theme, param) {
    const actualType = param.enso_type
    let value = param.value

    if (actualType === textType) {
        value = "'" + value.replaceAll("'", "''") + "'"
    }

    const actualTypeColor = theme.getColorForType(actualType)
    const fgColor = actualTypeColor
    let bgColor = replaceAlpha(fgColor, interpolationBacgroundOpacity)

    return renderRegularInterpolation(value, fgColor, bgColor)
}

/**
 * A helper that renders the HTML representation of a regular SQL interpolation.
 */
function renderRegularInterpolation(value, fgColor, bgColor) {
    let html =
        '<div class="interpolation" style="color:' +
        convertColorToRgba(fgColor) +
        ';background-color:' +
        convertColorToRgba(bgColor) +
        ';">'
    html += value
    html += '</div>'
    return html
}

// === Tooltip ===

/**
 * A hint tooltip that can be displayed above elements.
 */
class Tooltip {
    constructor(container) {
        this.tooltip = document.createElement('div')
        this.tooltip.setAttributeNS(null, 'class', 'tooltip')
        container.appendChild(this.tooltip)
        this.tooltipOwner = null
    }

    /**
     * Hides the tooltip.
     *
     * The actor parameter specifies who is initiating the hiding.
     * If this method is called but the tooltip has got a new owner in the meantime, the call is
     * ignored.
     */
    hide(actor) {
        if (this.tooltipOwner === null || this.tooltipOwner === actor) {
            this.tooltipOwner = null
            this.tooltip.style.opacity = 0
        }
    }

    /**
     * Shows the tooltip above the element represented by `actor`.
     *
     * Tooltip content is specified by the `message` which can include arbitrary HTML.
     */
    show(actor, message) {
        this.tooltipOwner = actor
        this.tooltip.innerHTML = message
        this.tooltip.style.opacity = 1

        const interpolantContainer = actor.parentElement
        const codeContainer = interpolantContainer.parentElement
        const scrollElement = codeContainer.parentElement

        const scrollOffsetX = scrollElement.scrollLeft
        const scrollOffsetY = scrollElement.scrollTop + scrollElement.offsetHeight

        const interpolantOffsetX = interpolantContainer.offsetLeft
        const interpolantOffsetY = interpolantContainer.offsetTop

        const centeringOffset = (interpolantContainer.offsetWidth - this.tooltip.offsetWidth) / 2
        const belowPadding = 3
        const belowOffset = interpolantContainer.offsetHeight + belowPadding

        const x = interpolantOffsetX - scrollOffsetX + centeringOffset
        const y = interpolantOffsetY - scrollOffsetY + belowOffset

        this.tooltip.style.transform = 'translate(' + x + 'px, ' + y + 'px)'
    }
}

return SqlVisualization
