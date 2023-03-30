/** Table visualization. */
loadScript('https://cdn.jsdelivr.net/npm/ag-grid-community/dist/ag-grid-community.min.js')
// Use the following line instead of the above one to use the enterprise version of ag-grid.
// loadScript('https://cdn.jsdelivr.net/npm/ag-grid-enterprise@29.1.0/dist/ag-grid-enterprise.min.js')

// ============================
// === Style Initialisation ===
// ============================

loadStyle('https://cdn.jsdelivr.net/npm/ag-grid-community/styles/ag-grid.css')
loadStyle('https://cdn.jsdelivr.net/npm/ag-grid-community/styles/ag-theme-alpine.css')
loadStyleFromString(scrollbarStyle)

// ===========================
// === Table visualization ===
// ===========================

class TableVisualization extends Visualization {
    // IMPORTANT: When updating this, also update the test in
    // test/Visualization_Tests/src/Default_Visualizations_Spec.enso:15 as this verifies that the
    // type names do not go out of sync. Should be removed once
    // https://github.com/enso-org/enso/issues/5195 is implemented.
    static inputType =
        'Standard.Table.Data.Table.Table | Standard.Table.Data.Column.Column | Standard.Base.Data.Vector.Vector | Standard.Base.Data.Array.Array | Standard.Base.Data.Map.Map | Any '
    static label = 'Table'

    constructor(data) {
        super(data)
        this.setPreprocessor('Standard.Visualization.Table.Visualization', 'prepare_visualization')
    }

    onDataReceived(data) {
        function hasExactlyKeys(keys, obj) {
            return Object.keys(obj).length === keys.length && keys.every(k => obj.hasOwnProperty(k))
        }

        function isObjectMatrix(data) {
            let isList = Array.isArray(data) && data[0]
            if (!isList || !(typeof data[0] === 'object')) return false
            let firstKeys = Object.keys(data[0])
            return data.every(obj => hasExactlyKeys(firstKeys, obj))
        }

        function isMatrix(data) {
            let isList = Array.isArray(data) && data[0]
            if (!isList) return false
            let firstIsArray = Array.isArray(data[0])
            if (!firstIsArray) return false
            let firstLen = data[0].length
            return data.every(d => d.length === firstLen)
        }

        function toRender(content) {
            if (content instanceof Array) {
                if (isMatrix(content)) {
                    return `[Vector ${content.length} rows x ${content[0].length} cols]`
                } else if (isObjectMatrix(content)) {
                    return `[Table ${content.length} rows x ${Object.keys(content[0]).length} cols]`
                } else {
                    return `[Vector ${content.length} items]`
                }
            }

            if (content instanceof Object) {
                const type = content.type
                if (type === 'BigInt') {
                    return BigInt(content.value)
                } else if (content['_display_text_']) {
                    return content['_display_text_']
                } else if (type === 'Date') {
                    return new Date(content.year, content.month - 1, content.day)
                        .toISOString()
                        .substring(0, 10)
                } else if (type === 'Time_Of_Day') {
                    const js_date = new Date(
                        0,
                        0,
                        1,
                        content.hour,
                        content.minute,
                        content.second,
                        content.nanosecond / 1000000
                    )
                    return (
                        js_date.toTimeString().substring(0, 8) +
                        (js_date.getMilliseconds() === 0 ? '' : '.' + js_date.getMilliseconds())
                    )
                } else if (type === 'Date_Time') {
                    const js_date = new Date(
                        content.year,
                        content.month - 1,
                        content.day,
                        content.hour,
                        content.minute,
                        content.second,
                        content.nanosecond / 1000000
                    )
                    return (
                        js_date.toISOString().substring(0, 10) +
                        ' ' +
                        js_date.toTimeString().substring(0, 8) +
                        (js_date.getMilliseconds() === 0 ? '' : '.' + js_date.getMilliseconds())
                    )
                } else {
                    return `{ ${type} Object }`
                }
            }

            return content
        }

        if (!this.tabElem) {
            while (this.dom.firstChild) {
                this.dom.removeChild(this.dom.lastChild)
            }

            const style =
                '.ag-theme-alpine { --ag-grid-size: 3px; --ag-list-item-height: 20px; display: inline; }'
            const styleElem = document.createElement('style')
            styleElem.innerHTML = style
            this.dom.appendChild(styleElem)

            const tabElem = document.createElement('div')
            tabElem.setAttributeNS(null, 'id', 'vis-tbl-view')
            tabElem.setAttributeNS(null, 'class', 'scrollable ag-theme-alpine')
            this.dom.appendChild(tabElem)
            this.tabElem = tabElem
            this.updateTableSize()

            this.agGridOptions = {
                rowData: [],
                columnDefs: [],
                defaultColDef: {
                    editable: false,
                    sortable: true,
                    filter: true,
                    resizable: true,
                    minWidth: 50,
                },
            }
            this.agGrid = new agGrid.Grid(tabElem, this.agGridOptions)
        }

        let parsedData = typeof data === 'string' ? JSON.parse(data) : data

        let columnDefs = []
        let rowData = []
        let dataTruncated = false

        if (parsedData.error !== undefined) {
            this.agGridOptions.api.setColumnDefs([
                {
                    field: 'error',
                    cellStyle: { 'white-space': 'normal' },
                },
            ])
            this.agGridOptions.api.setRowData([{ error: parsedData.error }])
        } else if (parsedData.json != null && isMatrix(parsedData.json)) {
            columnDefs = parsedData.json[0].map((_, i) => ({ field: i.toString() }))
            rowData = parsedData.json
            dataTruncated = parsedData.all_rows_count !== parsedData.json.length
        } else if (parsedData.json != null && isObjectMatrix(parsedData.json)) {
            let firstKeys = Object.keys(parsedData.json[0])
            columnDefs = firstKeys.map(field => ({ field }))
            rowData = parsedData.json.map(obj =>
                firstKeys.reduce((acc, key) => ({ ...acc, [key]: toRender(obj[key]) }), {})
            )
            dataTruncated = parsedData.all_rows_count !== parsedData.json.length
        } else if (parsedData.json != null && Array.isArray(parsedData.json)) {
            columnDefs = [{ field: '#', headerName: 'Row#' }, { field: 'value' }]
            rowData = parsedData.json.map((row, i) => ({ ['#']: i, value: toRender(row) }))
            dataTruncated = parsedData.all_rows_count !== parsedData.json.length
        } else if (parsedData.json != null) {
            columnDefs = [{ field: 'value' }]
            rowData = [{ value: toRender(parsedData.json) }]
        } else {
            const indices_header = (parsedData.indices_header ? parsedData.indices_header : []).map(
                h => {
                    const headerName = h === '#' ? 'Row#' : h
                    return { field: h, headerName: headerName }
                }
            )
            columnDefs = [...indices_header, ...parsedData.header.map(h => ({ field: h }))]

            const rows =
                parsedData.data && parsedData.data.length > 0
                    ? parsedData.data[0].length
                    : parsedData.indices && parsedData.indices.length > 0
                    ? parsedData.indices[0].length
                    : 0
            rowData = Array.apply(null, Array(rows)).map((_, i) => {
                const row = {}
                const shift = parsedData.indices ? parsedData.indices.length : 0
                columnDefs.map(
                    (h, j) =>
                        (row[h.field] = toRender(
                            j < shift ? parsedData.indices[j][i] : parsedData.data[j - shift][i]
                        ))
                )
                return row
            })

            dataTruncated = parsedData.all_rows_count !== rowData.length
        }

        // If the table contains more rows than an upper limit, the engine will send only some of all rows.
        // If data is truncated, we cannot rely on sorting/filtering so will disable.
        // A pinned row is added to tell the user the row count and that filter/sort is disabled.
        const col_span = '__COL_SPAN__'
        if (dataTruncated) {
            columnDefs[0].colSpan = p => p.data[col_span] || 1
        }
        this.agGridOptions.defaultColDef.filter = !dataTruncated
        this.agGridOptions.defaultColDef.sortable = !dataTruncated
        this.agGridOptions.api.setColumnDefs(columnDefs)
        if (dataTruncated) {
            const field = columnDefs[0].field
            const extraRow = {
                [field]: `Showing ${rowData.length} of ${parsedData.all_rows_count} rows. Sorting and filtering disabled.`,
                [col_span]: columnDefs.length,
            }
            this.agGridOptions.api.setPinnedTopRowData([extraRow])
        }
        this.agGridOptions.api.setRowData(rowData)
        this.agGridOptions.api.sizeColumnsToFit()
    }

    updateTableSize() {
        if (this.tabElem !== undefined) {
            const width = this.dom.getAttributeNS(null, 'width')
            const height = this.dom.getAttributeNS(null, 'height')
            const tblViewStyle = `width: ${width}px; height: ${height}px; overflow: scroll;`
            this.tabElem.setAttributeNS(null, 'style', tblViewStyle)
        }

        this.agGridOptions && this.agGridOptions.api.sizeColumnsToFit()
    }

    setSize(size) {
        this.dom.setAttributeNS(null, 'width', size[0])
        this.dom.setAttributeNS(null, 'height', size[1])
        this.updateTableSize()
    }
}

return TableVisualization
