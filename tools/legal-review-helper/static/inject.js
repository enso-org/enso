/** Sets a status text in bottom left part of the screen. */
function setStatus(text, color) {
    const status = $('#status')
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
            const tab = $(elem).closest('div').parent()
            const title = tab.children('h4')
            tab.accordion('option', 'active', false)
            const info = 'added ' + file
            if (action == 'remove') {
                info = 'undone review'
            }
            const newTitle =
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
    initializeLicenseReviewButton()
    initializeCustomCopyrightButton()
    initializeUnexpectedEntryButton()

    setStatus('Initialized')
})

function initializeCopyrightButtons() {
    const copys = $('.copyright-ui')
    copyrightMap = {
        Ignore: 'copyright-ignore',
        KeepWithContext: 'copyright-keep-context',
        Keep: 'copyright-keep',
    }

    copys.each(function (index) {
        const package = $(this).data('package')
        const encodedContent = $(this).data('content')
        const status = $(this).data('status')
        const contexts = parseInt($(this).data('contexts'))
        const data = {
            encoded_line: encodedContent,
            package: package,
        }
        if (status == 'NotReviewed') {
            const buttons =
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
    const files = $('.file-ui')
    filesMap = {
        Ignore: 'files-ignore',
        Keep: 'files-keep',
    }

    files.each(function (index) {
        const package = $(this).data('package')
        const filename = $(this).data('filename')
        const status = $(this).data('status')
        const data = {
            line: filename,
            package: package,
        }
        if (status == 'NotReviewed') {
            const buttons =
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

        // TODO button to set as license
    })
}

function initializePackageRenameButtons() {
    $('.rename-dependency-config').each(function (index) {
        const entry = $(this)
        const button = ' <button class="auto-rename">Auto-Rename</button>'
        entry.append(button)
        entry.children('.auto-rename').on('click', function (ev) {
            const button = $(this)
            button.prop('disabled', true)
            const data = {
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

function initializeLicenseReviewButton() {
    $('.license-not-reviewed').each(function (index) {
        const entry = $(this)
        const licenseName = entry.data('name')
        const button = ' <button class="mark-reviewed">Mark license as reviewed</button>'
        entry.append(button)
        entry.children('.mark-reviewed').on('click', function (ev) {
            const confirmed = confirm(
                'PLEASE verify that the license ' +
                    licenseName +
                    " is compatible with the project's way of distribution and license. Click OK if the license has passed the verification, otherwise please Cancel."
            )
            if (confirmed) {
                setStatus('Marking license ' + licenseName + ' as reviewed...')
                const data = {
                    name: licenseName,
                }
                $.post('/mark-license-reviewed/' + reportName, data, function (response) {
                    const message =
                        'License ' +
                        licenseName +
                        ' marked as reviewed. Regenerate the report to see the changes.'
                    entry.html(message)
                    entry.css('color', 'gray')
                    setStatus(message)
                }).fail(function (err) {
                    setStatus(
                        'Failed to mark license ' +
                            licenseName +
                            ' as reviewed: ' +
                            JSON.stringify(err),
                        'red'
                    )
                })
            }
        })
    })
}

function initializeCustomCopyrightButton() {
    $('.add-custom-copyright-notice').each(function (index) {})
}

function initializeUnexpectedEntryButton() {
    $('.unexpected-entry-in-file').each(function (index) {
        const fileName = $(this).data('filename')
        const package = $(this).data('package')
        const encodedContent = $(this).data('content')
        const data = {
            encoded_line: encodedContent,
            package: package,
        }
        const button = ' <button>Remove entry</button>'
        $(this).append(button)
        $(this)
            .children('button')
            .on('click', makeHandler(this, data, fileName, 'remove'))
    })
}
