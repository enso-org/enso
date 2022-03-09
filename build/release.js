/// Package release utilities. Especially, utilities to load `CHANGELOG.md`, extract the newest
/// entry, and use it to generate package version and description.

const fss = require('fs')
const path = require('path')
const paths = require('./paths')
const semver = require('semver')
const yaml = require('js-yaml')

// =================
// === Constants ===
// =================

const config = yaml.load(fss.readFileSync(path.join(paths.gui.root, 'config.yaml'), 'utf-8'))

const CHANGELOG_FILE_NAME = 'CHANGELOG.md'
const CHANGELOG_FILE = path.join(paths.root, CHANGELOG_FILE_NAME)
const ENGINE_VERSION = config.engineVersionSupported

// ===============
// === Version ===
// ===============

class NextReleaseVersion {
    /// Version used for config files when building the package with "next version" in changelog.
    toString() {
        return '0.0.0'
    }

    isPrerelease() {
        return true
    }
}

class Version {
    constructor(major, minor, patch, tag, tagVersion, rcTag, rcTagVersion) {
        this.major = major
        this.minor = minor
        this.patch = patch
        this.tag = tag
        this.tagVersion = parseInt(tagVersion)
        this.rcTag = rcTag
        this.rcTagVersion = rcTagVersion
    }

    lt(that) {
        if (this.major < that.major) {
            return true
        }
        if (this.minor < that.minor) {
            return true
        }
        if (this.patch < that.patch) {
            return true
        }
        if (this.tag === 'alpha' && that.tag === 'beta') {
            return true
        }
        if (this.tag === 'alpha' && that.tag === 'rc') {
            return true
        }
        if (this.tag === 'beta' && that.tag === 'rc') {
            return true
        }
        if (this.tagVersion < that.tagVersion) {
            return true
        }
        if (this.rcTagVersion < that.rcTagVersion) {
            return true
        }
        return false
    }

    isPrerelease() {
        if (this.tag) {
            return true
        } else {
            return false
        }
    }

    toString() {
        let suffix = ''
        if (this.tag) {
            suffix = `-${this.tag}.${this.tagVersion}`
            if (this.rcTag) {
                suffix += `.${this.rcTag}.${this.rcTagVersion}`
            }
        }
        return `${this.major}.${this.minor}.${this.patch}${suffix}`
    }
}

// ======================
// === ChangelogEntry ===
// ======================

class ChangelogEntry {
    constructor(version, body) {
        this.version = version
        this.body = body
    }

    assert_is_newest_version_defined() {
        if (this.version instanceof NextReleaseVersion) {
            throw `The newest entry in CHANGELOG.md does not have version assigned.`
        }
    }

    assert_is_unstable() {
        this.assert_is_newest_version_defined()
        if (!this.isPrerelease()) {
            throw 'Assertion failed. The version is stable.'
        }
    }

    assert_is_stable() {
        this.assert_is_newest_version_defined()
        if (this.isPrerelease()) {
            throw 'Assertion failed. The version is unstable.'
        }
    }

    isPrerelease() {
        return this.version.isPrerelease()
    }
}

// =================
// === Changelog ===
// =================

class Changelog {
    constructor() {
        this.entries = changelogEntries()
    }

    newestEntry() {
        return this.entries[0]
    }

    currentVersion() {
        return this.newestEntry().version
    }
}

function changelogSections() {
    let text = '\n' + fss.readFileSync(CHANGELOG_FILE, 'utf8')
    let chunks = text.split(/\r?\n#(?!#)/)
    return chunks.filter(s => s != '')
}

function changelogEntries() {
    let sections = changelogSections()
    let entries = []
    let firstSection = true
    for (let section of sections) {
        let splitPoint = section.indexOf('\n')
        let header = section.substring(0, splitPoint)
        let body = section.substring(splitPoint).trim()
        if (firstSection && header.startsWith(' Next Release')) {
            let version = new NextReleaseVersion()
            entries.push(new ChangelogEntry(version, body))
        } else {
            let headerReg =
                /^ Enso (?<major>[0-9]+)\.(?<minor>[0-9]+)\.(?<patch>[0-9]+)(-(?<tag>alpha|beta|rc)\.(?<tagVersion>[0-9]+))?(.(?<rcTag>rc)\.(?<rcTagVersion>[0-9]+))? \((?<year>[0-9][0-9][0-9][0-9])-(?<month>[0-9][0-9])-(?<day>[0-9][0-9])\)/
            let match = header.match(headerReg)
            if (!match) {
                throw `Improper changelog entry header: '${header}'. See the 'CHANGELOG_TEMPLATE.md' for details.`
            }
            let grps = match.groups
            let version = new Version(
                grps.major,
                grps.minor,
                grps.patch,
                grps.tag,
                grps.tagVersion,
                grps.rcTag,
                grps.rcTagVersion
            )
            entries.push(new ChangelogEntry(version, body))
        }
        firstSection = false
    }

    let firstEntry = true
    let lastVersion = null
    for (let entry of entries) {
        if (!(firstEntry && entry.version instanceof NextReleaseVersion)) {
            if (lastVersion !== null) {
                if (!entry.version.lt(lastVersion)) {
                    let v1 = entry.version.toString()
                    let v2 = lastVersion.toString()
                    throw `Versions are not properly ordered in the changelog (${v1} >= ${v2}).`
                }
            }
            lastVersion = entry.version
        }
        firstEntry = false
    }
    return entries
}

function changelog() {
    return new Changelog()
}

function currentVersion() {
    return changelog().currentVersion()
}

// ===============
// === Exports ===
// ===============

module.exports = { ENGINE_VERSION, Version, NextReleaseVersion, changelog, currentVersion }
