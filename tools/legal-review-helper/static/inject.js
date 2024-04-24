/** Sets a status text in bottom left part of the screen. */
function setStatus(text, color) {
    var status = $('#status')
    status.html(text)
    if (color === undefined) {
        color = 'white'
    }
    status.css('background-color', color)
}

/** Creates a handler that will request to add or remove a line from a file. */
function makeHandler(elem, data, file, action) {
    return function (ev) {
        data['file'] = file
        data['action'] = action
        setStatus('Sending review...')
        $.post('/modify/' + reportName, data, function (response) {
            $(elem).html(
                '<span style="color:gray">Modified, if you want to ' +
                    'change this value, regenerate the report first</span>'
            )
            var tab = $(elem).closest('div').parent()
            var title = tab.children('h4')
            tab.accordion('option', 'active', false)
            var info = 'added ' + file
            if (action == 'remove') {
                info = 'undone review'
            }
            var newTitle =
                '<span style="text-decoration: line-through;">' +
                title.html() +
                '</span><br>' +
                info
            title.html(newTitle)
            title.find('span').css('color', 'gray')
            setStatus('Review for ' + data['package'] + ' sent.')
        }).fail(function (err) {
            setStatus('Failed to send review: ' + JSON.stringify(err), 'red')
        })
    }
}

$(function () {
    $('body').prepend(
        '<div style="color:orange">This review helper tool does not regenerate the ' +
            'report - any changes that are applied using this tool will not be visible after ' +
            'refreshing the page, until you regenerate the report by running ' +
            '`openLegalReviewReport` command again.</div>'
    )
    $('body').append(
        '<div id="status" ' + 'style="position: fixed;left:4pt;bottom:4pt">' + 'Loading...</div>'
    )

    initializeFileButtons()
    initializeCopyrightButtons()
    initializePackageRenameButtons()

    setStatus('Initialized')
})

function initializeCopyrightButtons() {
    var copys = $('.copyright-ui')
    copyrightMap = {
        Ignore: 'copyright-ignore',
        KeepWithContext: 'copyright-keep-context',
        Keep: 'copyright-keep',
    }

    copys.each(function (index) {
        var package = $(this).data('package')
        var encodedContent = $(this).data('content')
        var status = $(this).data('status')
        var contexts = parseInt($(this).data('contexts'))
        var data = {
            encoded_line: encodedContent,
            package: package,
        }
        if (status == 'NotReviewed') {
            var buttons =
                '<button class="ignore">Ignore</button>' +
                '<button class="keep">Keep single line</button>' +
                '<button class="keepctx">Keep with context</button>'
            $(this).html(buttons)
            $(this)
                .children('.ignore')
                .on('click', makeHandler(this, data, 'copyright-ignore', 'add'))
            $(this)
                .children('.keep')
                .on('click', makeHandler(this, data, 'copyright-keep', 'add'))
            if (contexts == 1) {
                $(this)
                    .children('.keepctx')
                    .on('click', makeHandler(this, data, 'copyright-keep-context', 'add'))
            } else {
                $(this).children('.keepctx').attr('disabled', true)
            }
        } else if (status != 'Added') {
            $(this).html('<button>Undo review</button>')
            $(this)
                .children('button')
                .on('click', makeHandler(this, data, copyrightMap[status], 'remove'))
        } else {
            $(this).html('<button disabled>This notice was added manually</button>')
        }
    })
}

function initializeFileButtons() {
    var files = $('.file-ui')
    filesMap = {
        Ignore: 'files-ignore',
        Keep: 'files-keep',
    }

    files.each(function (index) {
        var package = $(this).data('package')
        var filename = $(this).data('filename')
        var status = $(this).data('status')
        var data = {
            line: filename,
            package: package,
        }
        if (status == 'NotReviewed') {
            var buttons =
                '<button class="ignore">Ignore</button>' + '<button class="keep">Keep</button>'
            $(this).html(buttons)
            $(this)
                .children('.ignore')
                .on('click', makeHandler(this, data, 'files-ignore', 'add'))
            $(this)
                .children('.keep')
                .on('click', makeHandler(this, data, 'files-keep', 'add'))
        } else if (status != 'Added') {
            $(this).html('<button>Undo review</button>')
            $(this)
                .children('button')
                .on('click', makeHandler(this, data, filesMap[status], 'remove'))
        } else {
            $(this).html('<button disabled>This file was added manually</button>')
        }
    })
}

function initializePackageRenameButtons() {
    $('.rename-dependency-config').each(function (index) {
        const entry = $(this)
        var button = ' <button class="auto-rename">Auto-Rename</button>'
        entry.append(button)
        entry.children('.auto-rename').on('click', function (ev) {
            const button = $(this)
            button.prop('disabled', true)
            var data = {
                from: entry.data('from'),
                to: entry.data('to'),
            }
            setStatus('Renaming ' + data['from'] + ' to ' + data['to'] + '...')
            $.post('/rename-package/' + reportName, data, function (response) {
                const message =
                    'Package renamed to ' +
                    data['to'] +
                    '. To see the changes, regenerate the report.'
                entry.html(message)
                entry.css('color', 'gray')
                setStatus(message)
            }).fail(function (err) {
                setStatus(
                    'Failed to rename package ' +
                        data['from'] +
                        ' to ' +
                        data['to'] +
                        ': ' +
                        JSON.stringify(err),
                    'red'
                )
            })
        })
    })
}
