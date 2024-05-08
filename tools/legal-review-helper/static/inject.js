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
            $(this).html('<button class="undo-review">Undo review</button>')
            $(this)
                .children('.undo-review')
                .on('click', makeHandler(this, data, filesMap[status], 'remove'))
        } else {
            $(this).html('<button disabled>This file was added manually</button>')
        }

        addLicenseOverrideButton($(this), package, filename)
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
            let formHtml = ''
            formHtml += '<div class="dialog"><form>'
            formHtml += '<label for="license-text">Select license text: </label>'
            formHtml += '<select name="license-text" id="license-text" disabled></select>'
            formHtml += '<button type="button" class="reload-licenses">🔃</button>'
            entry.append(formHtml)

            let dialogRef = null
            function onSubmit() {
                const licenseTextPath = dialogRef.find('#license-text').val()
                if (!licenseTextPath) {
                    alert('Please select a license text first.')
                    return
                }

                const data = {
                    licenseName: licenseName,
                    licenseTextPath: licenseTextPath,
                }
                $.post('/mark-license-as-reviewed/' + reportName, data, function (response) {
                    dialogRef.dialog('close')
                    entry.html('License reviewed. Regenerate the report to see the changes.')
                    setStatus(response)
                }).fail(function (err) {
                    dialogRef.dialog('close')
                    setStatus('Failed to mark license as reviewed: ' + JSON.stringify(err), 'red')
                })
            }

            function refreshLicenses() {
                $.get('/get-known-license-texts', function (response) {
                    const knownLicenses = JSON.parse(response)
                    const select = dialogRef.find('#license-text')
                    select.empty()

                    if (knownLicenses.length == 0) {
                        select.prop('disabled', true)
                        select.append($('<option></option>').attr('value', '').text(''))
                        select
                            .parent()
                            .append(
                                '<p style="color: red;">No license texts found. Add a file to /tools/legal-review/license-texts directory and reload.</p>'
                            )
                    } else {
                        select.append($('<option></option>').attr('value', '').text(''))
                        knownLicenses.forEach(license => {
                            const option = $('<option></option>')
                                .attr('value', license)
                                .text(license)
                            select.append(option)
                        })
                        select.prop('disabled', false)
                    }
                }).fail(function (err) {
                    setStatus('Failed to fetch known license texts: ' + JSON.stringify(err), 'red')
                })
            }

            dialogRef = entry.children('.dialog').dialog({
                autoOpen: true,
                modal: true,
                width: 800,
                buttons: {
                    'Mark license as reviewed and associated with selected license text': onSubmit,
                },
            })

            dialogRef.find('.reload-licenses').on('click', function (ev) {
                ev.preventDefault()
                refreshLicenses()
            })

            refreshLicenses()
        })
    })
}

function initializeCustomCopyrightButton() {
    $('.add-custom-copyright-notice').each(function (index) {
        $(this).css('display', 'block')
        resetCustomCopyrightButton($(this))
    })
}

function resetCustomCopyrightButton(injectionLocation) {
    const button =
        ' <button class="add-custom-notice" style="font-size: 10pt;" title="Add custom notice">+</button>'
    injectionLocation.html(button)
    injectionLocation.children('.add-custom-notice').on('click', function (ev) {
        // Replace the button with a text area
        const code =
            "Custom notice content:<br><textarea class='custom-notice-content'></textarea><br><button class='submit-custom-notice'>Submit</button>"
        injectionLocation.html(code)

        injectionLocation.children('.submit-custom-notice').on('click', function (ev) {
            const notice = injectionLocation.children('.custom-notice-content').val()
            try {
                // TODO support UTF-8 characters?
                const encoded = btoa(notice)
                const data = {
                    package: injectionLocation.data('package'),
                    action: 'add',
                    file: 'copyright-add',
                    line: encoded,
                }
                setStatus('Adding custom notice...')
                $.post('/modify/' + reportName, data, function (response) {
                    injectionLocation.css('color', 'gray')
                    setStatus('Custom notice added. Regenerate the report to see the changes.')
                    // TODO add note to parent as well to make it clearer
                    resetCustomCopyrightButton(injectionLocation)
                }).fail(function (err) {
                    setStatus('Failed to add custom notice: ' + JSON.stringify(err), 'red')
                })
            } catch (e) {
                setStatus('Failed to encode custom notice: ' + e, 'red')
            }
        })
    })
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
        const button = '<button>Remove entry</button> '
        $(this).prepend(button)
        $(this)
            .children('button')
            .on('click', makeHandler(this, data, fileName, 'remove'))
    })
}

function addLicenseOverrideButton(entry, package, filename) {
    const button =
        ' <button class="license-override" style="font-size: 7pt" title="Sets this file as the MAIN license to use for this dependency, instead of the default inferred license text.">Set as custom-license</button>'
    entry.append(button)
    entry.children('.license-override').on('click', function (ev) {
        const button = $(this)
        button.prop('disabled', true)
        const data = {
            package: package,
            file: filename,
        }
        setStatus('Overriding license for ' + package + '...')
        $.post('/override-custom-license/' + reportName, data, function (response) {
            const message = 'License overridden. Regenerate the report to see the changes.'
            entry.append(message)
            entry.css('color', 'gray')
            setStatus(message)
        }).fail(function (err) {
            setStatus(
                'Failed to override license for ' + package + ': ' + JSON.stringify(err),
                'red'
            )
        })
    })
}
