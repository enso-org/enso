/** @file
 * This file generates the product logo as SVG and then converts it to set of PNGs, MacOS ICNS, and
 * Windows ICO formats.
 */

import * as childProcess from 'node:child_process'
import * as fs from 'node:fs/promises'
import * as fsSync from 'node:fs'
import * as os from 'node:os'
import * as path from 'node:path'
import * as url from 'node:url'

import sharp from 'sharp'
import toIco from 'to-ico'

// =================
// === Constants ===
// =================

const DEFAULT_SIZE = 64
const INNER_SIZE = 56
const BORDER_MAX = 10
const BORDER_WIDTH = 7
/** How much bigger the left atom is compared to the right atom. */
const LEFT_ATOM_ENLARGEMENT = 4
/** The atoms are spaced one radius apart, so the centers are spaced three radii apart. */
const ATOM_SPACING_FACTOR = 3
const MACOS_DPI = 144

// ============
// === Logo ===
// ============

class Logo {
    constructor(size = DEFAULT_SIZE, compatibleMode = true) {
        this.xsize = size
        this.size = DEFAULT_SIZE
        this.compatibleMode = compatibleMode
        this.borderMax = BORDER_MAX
        this.borderSpread = 2
        this.borderWidth = BORDER_WIDTH
        this.topRadius = this.size / 2
        this.borderOffset = this.borderWidth - this.borderSpread
        this.innerRadius = this.topRadius - this.borderWidth - this.borderOffset
        this.atomRadius = this.innerRadius / 2
        this.atomDiff = 0
        this.d = LEFT_ATOM_ENLARGEMENT
        this.scale1 = INNER_SIZE / this.size
        this.scale = this.xsize / this.size
        this.tx = (this.size - INNER_SIZE) / 2
        if (this.compatibleMode) {
            this.ref = 'xlink:href'
        } else {
            this.ref = 'href'
        }
        this.defs = ''
    }

    generate() {
        return `
<svg version="1.1" baseProfile="full" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" height="${
            this.xsize
        }" width="${this.xsize}" viewBox="0 0 ${this.xsize} ${this.xsize}">
    <defs>
        <circle id="innerCircle" cx="32" cy="32" r="${this.innerRadius}"/>
        <circle id="leftAtom"    cx="${
            this.borderWidth + this.borderOffset + this.atomRadius + this.atomDiff - this.d
        }" cy="32" r="${this.atomRadius + this.atomDiff + this.d}"/>
        <circle id="rightAtom"   cx="${
            this.borderWidth +
            this.borderOffset +
            ATOM_SPACING_FACTOR * this.atomRadius +
            this.atomDiff
        }" cy="32" r="${this.atomRadius - this.atomDiff}"/>
        <mask id="innerCircleMask">
            <use ${this.ref}="#innerCircle" fill="white"/>
        </mask>

        <rect id="bg" width="64" height="64" fill="white"/>
            <mask id="bgmask">
            <use ${this.ref}="#bg"/>
            <circle cx="32" cy="32" r="${this.size / 2 - this.borderWidth}" fill="black"/>
        </mask>

        <mask id="mainShapeMask">
            <use ${this.ref}="#bg"/>
            <use ${this.ref}="#leftAtom" fill="black"/>
            <rect cy="32" width="64" height="32" fill="black"/>
        </mask>

        <g id="border">
            <circle cx="32" cy="32" r="32" mask="url(#bgmask)"/>
        </g>

        <g id="front">
            <use ${this.ref}="#innerCircle" mask="url(#mainShapeMask)"/>
            <use ${this.ref}="#rightAtom"/>
        </g>

        <g id="logo">
            <use ${this.ref}="#border"/>
            <use ${this.ref}="#front" transform="rotate(35 32 32)"/>
        </g>

        <g id="logo-scaled">
            <g transform="translate(${this.tx} ${this.tx}) scale(${this.scale1})">
                <use ${this.ref}="#logo" fill="#fafafa"/>
            </g>
        </g>

        <g id="background">
            <circle id="innerCircle" cx="32" cy="32" r="31" fill="#24292f"/>
        </g>

        <g id="final">
            <g> <use ${this.ref}="#background"/> </g>
            <g> <use ${this.ref}="#logo-scaled"/> </g>
        </g>

        ${this.defs}

    </defs>
    ${this.main()}
</svg>
`
    }

    main() {
        return `<g transform="scale(${this.scale})"> <use ${this.ref}="#final"/> </g>`
    }
}

/**
 * Generate icons.
 * @param {string} outputDir - The directory in which the icons will be placed.
 */
async function genIcons(outputDir) {
    // eslint-disable-next-line @typescript-eslint/no-magic-numbers
    let sizes = [16, 32, 64, 128, 256, 512, 1024]
    // eslint-disable-next-line @typescript-eslint/no-magic-numbers
    let winSizes = [16, 32, 64, 128, 256]

    const donePath = path.join(outputDir, 'init')
    if (fsSync.existsSync(donePath)) {
        console.log(`The ${donePath} file exists. Icons will not be regenerated.`)
        return
    } else {
        console.log(`Generating icons to ${outputDir}`)
        console.log('Generating SVG icons.')
        await fs.mkdir(path.resolve(outputDir, 'svg'), { recursive: true })
        await fs.mkdir(path.resolve(outputDir, 'png'), { recursive: true })
        for (const size of sizes) {
            let name = `icon_${size}x${size}.svg`
            let logo = new Logo(size, true).generate()
            await fs.writeFile(`${outputDir}/svg/${name}`, logo)
        }

        /// Please note that this function converts the SVG to PNG
        /// AND KEEPS THE METADATA INFORMATION ABOUT DPI OF 144.
        /// It is required to properly display png images on MacOS.
        /// There is currently no other way in `sharp` to do it.
        console.log('Generating PNG icons.')
        for (const size of sizes) {
            let inName = `icon_${size}x${size}.svg`
            let outName = `icon_${size}x${size}.png`
            await sharp(`${outputDir}/svg/${inName}`, { density: MACOS_DPI })
                .png()
                .resize({
                    width: size,
                    kernel: sharp.kernel.mitchell,
                })
                .toFile(`${outputDir}/png/${outName}`)
        }

        for (const size of sizes.slice(1)) {
            let size2 = size / 2
            let inName = `icon_${size}x${size}.svg`
            let outName = `icon_${size2}x${size2}@2x.png`
            await sharp(`${outputDir}/svg/${inName}`, { density: MACOS_DPI })
                .png()
                .resize({
                    width: size,
                    kernel: sharp.kernel.mitchell,
                })
                .toFile(`${outputDir}/png/${outName}`)
        }

        if (os.platform() === 'darwin') {
            console.log('Generating ICNS.')
            childProcess.execSync(`cp -R ${outputDir}/png ${outputDir}/png.iconset`)
            childProcess.execSync(
                `iconutil --convert icns --output ${outputDir}/icon.icns ${outputDir}/png.iconset`
            )
        }

        console.log('Generating ICO.')
        let files = []
        for (const size of winSizes) {
            let inName = `icon_${size}x${size}.png`
            let data = await fs.readFile(`${outputDir}/png/${inName}`)
            files.push(data)
        }
        const icoBuffer = await toIco(files)
        fsSync.writeFileSync(`${outputDir}/icon.ico`, icoBuffer)

        let handle = await fs.open(donePath, 'w')
        await handle.close()
        return
    }
}

/** Main entry function. */
async function main() {
    const outputDir = process.env.ENSO_BUILD_ICONS ?? process.argv[2]
    if (!outputDir) {
        const script = process.env.npm_package_name ?? url.fileURLToPath(import.meta.url)
        throw Error(
            `Script '${script}' invocation needs to be given an output path either through command line argument or 'ENSO_BUILD_ICONS' environment variable.`
        )
    } else {
        await genIcons(outputDir)
        return
    }
}

await main()
