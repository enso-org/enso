/// This file generates the product logo as SVG and then converts it to set of PNGs, MacOS ICNS, and
/// Windows ICO formats.

class Logo {
    constructor(size = 64, compatibleMode = true) {
        this.xsize = size
        this.size = 64
        this.compatibleMode = compatibleMode
        this.borderMax = 10
        this.borderSpread = 2
        this.init()
    }

    init() {
        var scaleStop = 128
        var scaleLog = Math.log2(scaleStop)
        this.borderWidth = 7
        this.topRadius = 32
        this.borderOffset = this.borderWidth - this.borderSpread
        this.innerRadius = this.topRadius - this.borderWidth - this.borderOffset
        this.atomRadius = this.innerRadius / 2
        this.atomDiff = 0
        this.d = 4
        let innerSize = 56
        this.scale1 = innerSize / 64
        this.scale = this.xsize / 64
        this.tx = (64 - innerSize) / 2
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
            this.borderWidth + this.borderOffset + 3 * this.atomRadius + this.atomDiff
        }" cy="32" r="${this.atomRadius - this.atomDiff}"/>
        <mask id="innerCircleMask">
            <use ${this.ref}="#innerCircle" fill="white"/>
        </mask>

        <rect id="bg" width="64" height="64" fill="white"/>
            <mask id="bgmask">
            <use ${this.ref}="#bg"/>
            <circle cx="32" cy="32" r="${32 - this.borderWidth}" fill="black"/>
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

class AppLogo extends Logo {
    constructor(size, compatibleMode) {
        super(size, compatibleMode)
        this.init()
    }
}

fastGenerate =
    cons =>
    (...args) =>
        new cons(...args).generate()

exports.generateMinimalWhiteLogo = fastGenerate(AppLogo)

const fss = require('fs')
const fs = fss.promises
const exec = require('child_process').exec
const spawn = require('child_process').spawn
const toIco = require('to-ico')
const sharp = require('sharp')
const path = require('path')

const thisPath = path.resolve(__dirname)
const root = path.resolve(thisPath, '..', '..', '..', '..', '..')
const distPath = path.resolve(root, 'dist', 'icons')
const donePath = path.resolve(distPath, 'init')

async function genIcons() {
    let sizes = [16, 32, 64, 128, 256, 512, 1024]
    let win_sizes = [16, 32, 64, 128, 256]

    if (fss.existsSync(donePath)) {
        console.log(`The ${donePath} file exists. Icons will not be regenerated.`)
        return
    }

    console.log('Generating SVG icons.')
    await fs.mkdir(path.resolve(distPath, 'svg'), { recursive: true })
    await fs.mkdir(path.resolve(distPath, 'png'), { recursive: true })
    for (let size of sizes) {
        let name = `icon_${size}x${size}.svg`
        await fs.writeFile(`${distPath}/svg/${name}`, exports.generateMinimalWhiteLogo(size, true))
    }

    /// Please note that this function converts the SVG to PNG
    /// AND KEEPS THE METADATA INFORMATION ABOUT DPI OF 144.
    /// It is required to properly display png images on MacOS.
    /// There is currently no other way in `sharp` to do it.
    console.log('Generating PNG icons.')
    for (let size of sizes) {
        let inName = `icon_${size}x${size}.svg`
        let outName = `icon_${size}x${size}.png`
        await sharp(`${distPath}/svg/${inName}`, { density: 144 })
            .png()
            .resize({
                width: size,
                kernel: sharp.kernel.mitchell,
            })
            .toFile(`${distPath}/png/${outName}`)
    }

    for (let size of sizes.slice(1)) {
        let size2 = size / 2
        let inName = `icon_${size}x${size}.svg`
        let outName = `icon_${size2}x${size2}@2x.png`
        await sharp(`${distPath}/svg/${inName}`, { density: 144 })
            .png()
            .resize({
                width: size,
                kernel: sharp.kernel.mitchell,
            })
            .toFile(`${distPath}/png/${outName}`)
    }

    console.log('Generating ICNS.')
    exec(`cp -R ${distPath}/png ${distPath}/png.iconset`)
    exec(`iconutil --convert icns --output ${distPath}/icon.icns ${distPath}/png.iconset`)

    console.log('Generating ICO.')
    let files = []
    for (let size of win_sizes) {
        let inName = `icon_${size}x${size}.png`
        let data = await fs.readFile(`${distPath}/png/${inName}`)
        files.push(data)
    }
    toIco(files).then(buf => {
        fss.writeFileSync(`${distPath}/icon.ico`, buf)
    })

    let handle = await fs.open(donePath, 'w')
    await handle.close()
}

genIcons()
