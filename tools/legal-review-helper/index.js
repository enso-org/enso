const reviewRoot = '../../target'
const settingsRoot = '../../tools/legal-review'

const express = require('express')
const app = express()
const open = require('open')
const fs = require('fs')
const path = require('path')

// The home page that lists available reports.
app.get('/', function (req, res) {
    let html = '<h1>Report review</h1>'
    const files = fs.readdirSync(reviewRoot)
    const reports = files
        .map(f => f.match(/^(.*)-report.html$/))
        .filter(m => m != null)
        .map(m => m[1])
    if (reports.length == 0) {
        html +=
            'No reports found. ' +
            'Run <pre style="display:inline">enso / gatherLicenses</pre> first.'
    } else {
        html += 'Select report:'
        html += '<ul>'
        reports.forEach(report => {
            html += '<li><a href="/report/' + report + '">' + report + '</a></li>'
        })
        html += '</ul>'
    }
    res.send(html)
})

// Serves the injection script.
app.use('/static', express.static('static'))

// Serves contents of the given report, injecting the review-mode script.
app.get('/report/:report', function (req, res) {
    const report = req.params['report']
    console.log('Opening report for ', report)
    fs.readFile(path.join(reviewRoot, report + '-report.html'), 'utf-8', (err, data) => {
        const injection =
            '<script src="/static/inject.js"></script>' +
            '<script>var reportName = "' +
            report +
            '";</script>'
        if (err) {
            res.status(400).send(err)
        } else {
            const injected = data.replace('</head>', injection + '</head>')
            res.send(injected)
        }
    })
})

// Appends a line to the setting file.
function addLine(report, package, file, line) {
    const dir = path.join(settingsRoot, report, package)
    const location = path.join(dir, file)
    console.log('Adding ' + line + ' to ' + location)
    fs.mkdirSync(dir, {
        recursive: true,
    })
    fs.appendFileSync(location, line + '\n')
}

// Removes a line from the setting file.
function removeLine(report, package, file, line) {
    const location = path.join(settingsRoot, report, package, file)
    console.log('Removing ' + line + ' from ' + location)
    const lines = fs
        .readFileSync(location, 'utf-8')
        .split(/\r?\n/)
        .filter(x => x.length > 0)
    const toRemove = lines.filter(x => x == line)
    const others = lines.filter(x => x != line)
    if (toRemove.length == 0) {
        throw (
            'Line ' +
            line +
            ' was not present in the file. ' +
            'Are you sure the report is up to date?'
        )
    } else {
        var newContent = others.join('\n') + '\n'
        if (others.length == 0) {
            newContent = ''
        }
        fs.writeFileSync(location, newContent)
    }
}

// Handles the requests to add or remove lines.
app.use(express.urlencoded({ extended: true }))
app.post('/modify/:report', function (req, res) {
    const report = req.params['report']
    const package = req.body['package']
    const action = req.body['action']
    const file = req.body['file']
    let line = req.body['line']
    const encodedLine = req.body['encoded_line']
    if (encodedLine !== undefined) {
        line = Buffer.from(encodedLine, 'base64').toString()
    }

    try {
        if (action == 'add') {
            addLine(report, package, file, line)
        } else if (action == 'remove') {
            removeLine(report, package, file, line)
        } else {
            throw 'Unknown action'
        }
        res.send('OK')
    } catch (error) {
        console.error(error)
        res.status(500).send(error)
    }
})

// Handles the request to rename a package directory.
app.post('/rename-package/:report', function (req, res) {
    const report = req.params['report']
    const from = req.body['from']
    const to = req.body['to']

    try {
        const fromPath = path.join(settingsRoot, report, from)
        const toPath = path.join(settingsRoot, report, to)

        if (!fs.existsSync(fromPath)) {
            throw 'The source directory ' + fromPath + ' does not exist...'
        }

        if (fs.existsSync(toPath)) {
            const isTargetEmpty = fs.readdirSync(toPath).length == 0
            if (!isTargetEmpty) {
                throw (
                    'The target directory ' +
                    toPath +
                    ' already exists. Please merge the directories manually using your preferred file explorer.'
                )
            }

            fs.rmdirSync(toPath)
        }

        fs.renameSync(fromPath, toPath)
        res.send('OK')
    } catch (error) {
        console.error(error)
        res.status(500).send(error)
    }
})

app.post('/override-custom-license/:report', function (req, res) {
    const report = req.params['report']
    const package = req.body['package']
    const file = req.body['file']

    try {
        const dir = path.join(settingsRoot, report, package)

        const defaultAndCustom = path.join(dir, 'default-and-custom-license')
        if (fs.existsSync(defaultAndCustom)) {
            fs.unlinkSync(defaultAndCustom)
        }

        const fileName = file.split('/').pop()

        const custom = path.join(dir, 'custom-license')
        fs.writeFileSync(custom, fileName + '\n')
        res.send('OK')
    } catch (error) {
        console.error(error)
        res.status(500).send(error)
    }
})

app.post('/revert-to-default-license/:report', function (req, res) {
    const report = req.params['report']
    const package = req.body['package']
    // TODO
})

app.post('/use-both-custom-and-default-license/:report', function (req, res) {
    const report = req.params['report']
    const package = req.body['package']
    // TODO
})

app.get('/get-known-license-texts', function (req, res) {
    const knownLicenses = fs.readdirSync(path.join(settingsRoot, 'license-texts'))
    res.send(JSON.stringify(knownLicenses))
})

app.post('/mark-license-as-reviewed/:report', function (req, res) {
    const report = req.params['report']
    const licenseName = req.body['licenseName']
    const licenseTextPath = 'tools/legal-review/license-texts/' + req.body['licenseTextPath']
    const location = path.join(settingsRoot, report, 'reviewed-licenses', licenseName)

    console.log(licenseTextPath + ' -> ' + location)

    if (fs.existsSync(location)) {
        res.status(400).send('License already marked as reviewed')
        return
    }

    fs.writeFileSync(location, licenseTextPath)
    res.send('Marked ' + licenseName + ' as reviewed: ' + licenseTextPath)
})

/*
 * Listens on a random free port, opens a browser with the home page and waits
 * for a newline to terminate.
 */
const server = app.listen(0, () => {
    const port = server.address().port
    console.log('Listening on at ', 'http://localhost:' + port + '/')
    open('http://localhost:' + port + '/')

    console.log('Press ENTER to stop the server.')
    process.stdin.on('data', function (chunk) {
        if (chunk.indexOf('\n') >= 0) {
            console.log('Good bye')
            process.exit(0)
        }
    })
})
