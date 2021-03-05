/** Table visualization. */

// ============================
// === Style Initialisation ===
// ============================

loadStyleFromString(scrollbarStyle)

// ===========================
// === Table visualization ===
// ===========================

class TableVisualization extends Visualization {
    static inputType = 'Any'
    static label = 'Table'

    onDataReceived(data) {
        if (!this.isInit) {
            this.setPreprocessor(
                'x -> (Json.from_pairs [["header", x.columns.map .name], ["data", x.columns.map .to_vector . map (x -> x.take_start 2000) ]]).to_text '
            )
            this.isInit = true
        }

        function tableOf(content, level) {
            let open = '<table class="level' + level + '">'
            return open + content + '</table>'
        }

        function hasExactlyKeys(keys, obj) {
            return Object.keys(obj).length === keys.length && keys.every(k => obj.hasOwnProperty(k))
        }

        function getAtNestedKey(data, key) {
            let res = data
            key.forEach(k => (res = res[k]))
            return res
        }

        function repNestedKey(key) {
            return key.join('.')
        }

        function generateNestings(data, key) {
            let first = getAtNestedKey(data[0], key)
            if (!(first instanceof Object)) return [key]
            let firstKeys = Object.keys(first)
            let isNestable = data.every(obj => hasExactlyKeys(firstKeys, getAtNestedKey(obj, key)))
            if (isNestable) {
                let withNests = firstKeys.map(k => key.concat([k]))
                let furtherNestings = withNests.map(k => generateNestings(data, k))
                return [].concat.apply([], furtherNestings)
            } else {
                return [key]
            }
        }

        function isObjectMatrix(data) {
            let isList = Array.isArray(data) && data[0]
            if (!isList || !(typeof data[0] === 'object')) return false
            let firstKeys = Object.keys(data[0])
            return data.every(obj => hasExactlyKeys(firstKeys, obj))
        }

        function genObjectMatrix(data, level) {
            let result = '<tr><th></th>'
            let keys = Object.keys(data[0])
            let nests = [].concat.apply(
                [],
                keys.map(k => generateNestings(data, [k]))
            )
            nests.forEach(key => {
                result += '<th>' + repNestedKey(key) + '</th>'
            })
            result += '</tr>'
            data.forEach((row, ix) => {
                result += '<tr><th>' + ix + '</th>'
                nests.forEach(k => {
                    result += toTableCell(getAtNestedKey(row, k), level)
                })
                result += '</tr>'
            })
            return tableOf(result, level)
        }

        function isMatrix(data) {
            let isList = Array.isArray(data) && data[0]
            if (!isList) return false
            let firstIsArray = Array.isArray(data[0])
            if (!firstIsArray) return false
            let firstLen = data[0].length
            return data.every(d => d.length === firstLen)
        }

        function genMatrix(data, level, header) {
            let result = '<tr><th></th>'
            if (header) {
                header.forEach((elt, ix) => {
                    result += '<th>' + elt + '</th>'
                })
            } else {
                data[0].forEach((elt, ix) => {
                    result += '<th>' + ix + '</th>'
                })
            }
            result += '</tr>'
            const table = []

            data.forEach((d, i) => {
                d.forEach((elem, idx) => {
                    table[idx] = table[idx] || []
                    table[idx].push(elem)
                })
            })

            table.forEach((row, ix) => {
                result += '<tr><th>' + ix + '</th>'
                row.forEach(d => {
                    result += toTableCell(d, level)
                })
                result += '</tr>'
            })
            return tableOf(result, level)
        }

        function genGenericTable(data, level) {
            let result = ''
            data.forEach((point, ix) => {
                result += '<tr><th>' + ix + '</th>' + toTableCell(point, level) + '</tr>'
            })
            return tableOf(result, level)
        }

        function genRowObjectTable(data, level) {
            let keys = Object.keys(data)
            let result = '<tr>'
            keys.forEach(key => {
                result += '<th>' + key + '</th>'
            })
            result += '</tr><tr>'
            keys.forEach(key => {
                result += toTableCell(data[key], level)
            })
            result += '</tr>'
            return tableOf(result, level)
        }

        function toTableCell(data, level) {
            if (Array.isArray(data)) {
                return '<td>' + genTable(data, level + 1) + '</td>'
            } else if (data instanceof Object) {
                return '<td>' + genRowObjectTable(data, level + 1) + '</td>'
            } else {
                if (data === undefined || data === null) data = ''
                let res = data.toString()
                return '<td class="plaintext">' + (res === '' ? 'N/A' : res) + '</td>'
            }
        }

        function genTable(data, level, header) {
            if (isMatrix(data)) {
                return genMatrix(data, level, header)
            } else if (isObjectMatrix(data)) {
                return genObjectMatrix(data, level)
            } else {
                return genGenericTable(data, level)
            }
        }

        while (this.dom.firstChild) {
            this.dom.removeChild(this.dom.lastChild)
        }

        const style_dark = `
        <style>
        table {
            font-family: DejaVuSansMonoBook, sans-serif;
            font-size: 12px;
        }
        
        td {
            color: rgba(255, 255, 255, 0.9);
            padding: 0;
        }
        
        td.plaintext,
        th {
            padding: 5px;
        }
        
        th,
        td {
            border: 1px solid transparent;
            background-clip: padding-box;
        }
        
        th {
            color: rgba(255, 255, 255, 0.7);
            font-weight: 400;
        }
        
        td,
        th {
            background-color: rgba(255, 255, 255, 0.03);
        }
        </style>
        `

        const style_light = `
        <style>
        table {
            font-family: DejaVuSansMonoBook, sans-serif;
            font-size: 12px;
        }

        td {
            color: rgba(0, 0, 0, 0.7);
            padding: 0;
        }

        td.plaintext,
            th {
            padding: 5px;
        }

        th,
            td {
            border: 1px solid transparent;
            background-clip: padding-box;
        }

        th {
            color: rgba(0, 0, 0, 0.9);
            font-weight: 400;
        }

        td,
        th {
            background-color: rgba(0, 0, 0, 0.025);
        }
        </style>`

        const width = this.dom.getAttributeNS(null, 'width')
        const height = this.dom.getAttributeNS(null, 'height')
        const tabElem = document.createElement('div')
        tabElem.setAttributeNS(null, 'id', 'vis-tbl-view')
        tabElem.setAttributeNS(null, 'class', 'scrollable')
        tabElem.setAttributeNS(null, 'viewBox', '0 0 ' + width + ' ' + height)
        tabElem.setAttributeNS(null, 'width', '100%')
        tabElem.setAttributeNS(null, 'height', '100%')
        const tblViewStyle = `width: ${width - 10}px;
             height: ${height - 10}px;
             overflow: scroll;
             padding:2.5px;`
        tabElem.setAttributeNS(null, 'style', tblViewStyle)
        this.dom.appendChild(tabElem)

        let parsedData = data
        if (typeof data === 'string') {
            parsedData = JSON.parse(data)
        }

        let style = style_light
        if (document.getElementById('root').classList.contains('dark-theme')) {
            style = style_dark
        }
        const table = genTable(parsedData.data || parsedData, 0, parsedData.header)
        tabElem.innerHTML = style + table
    }

    setSize(size) {
        this.dom.setAttributeNS(null, 'width', size[0])
        this.dom.setAttributeNS(null, 'height', size[1])
    }
}

return TableVisualization
