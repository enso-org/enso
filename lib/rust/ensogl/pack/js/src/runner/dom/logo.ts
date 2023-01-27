/** @file Enso SVG logo generator. */

// ==============
// === Config ===
// ==============

/** Logo generator configuration. The fields affect the way the logo is drawn. */
interface Config {
    borderOffset?: number
    borderWidth?: number
    color?: string
    compatibleMode?: boolean
    rotation?: number
    shapeErosion?: number
    shapeSpikeCutoff?: number
    showBorder?: boolean
    size?: number
}

// ============
// === Logo ===
// ============

/** The logo generator. */
export class Logo {
    borderOffset = 6
    borderWidth = 8
    color = '#FF0000'
    compatibleMode = true
    rotation = 0
    shapeErosion = -4
    shapeSpikeCutoff = 4
    showBorder = true
    size = 64

    ref: string
    innerCircleRadius: number
    shapeSpikeCutoffScaled: number
    innerRadius: number

    constructor(config?: Config) {
        this.borderOffset = config?.borderOffset ?? this.borderOffset
        this.borderWidth = config?.borderWidth ?? this.borderWidth
        this.color = config?.color ?? this.color
        this.compatibleMode = config?.compatibleMode ?? this.compatibleMode
        this.rotation = config?.rotation ?? this.rotation
        this.shapeErosion = config?.shapeErosion ?? this.shapeErosion
        this.shapeSpikeCutoff = config?.shapeSpikeCutoff ?? this.shapeSpikeCutoff
        this.showBorder = config?.showBorder ?? this.showBorder
        this.size = config?.size ?? this.size

        this.innerRadius = this.size / 2 - this.borderWidth - this.borderOffset
        this.innerCircleRadius = this.innerRadius / 2
        this.shapeSpikeCutoffScaled = (this.shapeSpikeCutoff * this.size) / 64
        if (this.compatibleMode) {
            this.ref = 'xlink:href'
        } else {
            this.ref = 'href'
        }
    }

    /** Generates the SVG code for the logo. */
    generate(): string {
        const id = (name: string) => {
            return `id="ensoLogo${name}"`
        }
        const idRef = (name: string) => {
            return `${ref}="#ensoLogo${name}"`
        }
        const mask = (name: string) => {
            return `mask="url(#ensoLogo${name})"`
        }
        const border = this.showBorder ? `<use ${this.ref}="#border"/>` : ''
        const size2 = this.size / 2
        const circleX =
            this.borderWidth + this.borderOffset + this.innerCircleRadius + this.shapeErosion
        const lCircleX = circleX - this.shapeSpikeCutoffScaled
        const lCircleR = Math.max(
            0,
            this.innerCircleRadius + this.shapeErosion + this.shapeSpikeCutoffScaled
        )
        const rCircleX = circleX + 2 * this.innerCircleRadius
        const rCircleR = Math.max(0, this.innerCircleRadius - this.shapeErosion)
        const ref = this.ref
        const xmlns = 'xmlns="http://www.w3.org/2000/svg"'
        const viewBox = `viewBox="0 0 ${this.size} ${this.size}"`
        const rotation = this.rotation + 35
        return `
        <svg ${xmlns} height="${this.size}" width="${this.size}" ${viewBox}>
            <defs>
                <circle ${id('InnerCircle')} cx="${size2}" cy="${size2}" r="${this.innerRadius}"/>
                <circle ${id('LeftCircle')} cx="${lCircleX}" cy="${size2}" r="${lCircleR}"/>
                <circle ${id('RightCircle')} cx="${rCircleX}" cy="${size2}" r="${rCircleR}"/>
                <rect ${id('Bg')} width="${this.size}" height="${this.size}" fill="white"/>
                <mask ${id('BgMask')}>
                  <use ${idRef('Bg')}/>
                  <circle cx="${size2}" cy="${size2}" r="${size2 - this.borderWidth}" fill="black"/>
                </mask>
                <mask ${id('MainShapeMask')}>
                  <use ${idRef('Bg')}/>
                  <use ${idRef('LeftCircle')} fill="black"/>
                  <rect width="${this.size}" height="${size2}" fill="black"/>
                </mask>
                <g ${id('Border')}>
                  <circle cx="${size2}" cy="${size2}" r="${size2}" ${mask('BgMask')}/>
                </g>
                <g ${id('InnerShape')}>
                  <use ${idRef('InnerCircle')} ${mask('MainShapeMask')}/>
                  <use ${idRef('RightCircle')}/>
                </g>
                
            </defs>
            <g id="ensoLogo" fill="${this.color}">
                ${border}
                <use ${idRef('InnerShape')} transform="rotate(${rotation} ${size2} ${size2})"/>
            </g>
        </svg>`
    }
}
