/** @file Files loader. Displays a loading spinner and reports the current download progress. */

import * as animation from 'runner/animation'
import * as html_utils from 'runner/dom/dom'
import * as math from 'runner/math'
import * as svg from 'runner/dom/svg'
import { Options } from 'runner/config'
import { Logo } from 'runner/dom/logo'
import { logger } from 'runner/log'

// =========================
// === ProgressIndicator ===
// =========================

const loaderColor = '#3c3c3c'
const ghostColor = '#00000020'
const topLayerIndex = '1000'

/** Visual representation of the loader. */
export class ProgressIndicator {
    dom: HTMLDivElement
    track: HTMLElement
    indicator: HTMLElement
    progressIndicatorMask: HTMLElement
    loaderTrackEndPoint: HTMLElement
    logo: HTMLElement
    center: HTMLElement
    initialized: Promise<void>
    destroyed: boolean
    ringInnerRadius: number
    ringWidth: number

    animatedValue = 0
    targetValue = 0
    minProgressSize = 0.1

    constructor(cfg: Options) {
        this.ringInnerRadius = 48
        this.ringWidth = 12

        this.dom = html_utils.newTopLevelDiv()
        this.dom.id = 'loader'
        this.dom.style.position = 'fixed'
        this.dom.style.top = '0'
        this.dom.style.left = '0'
        this.dom.style.zIndex = topLayerIndex
        this.dom.style.background = 'white'
        this.dom.style.opacity = '1'

        const center = document.createElement('div')
        center.style.width = '100%'
        center.style.height = '100%'
        center.style.display = 'flex'
        center.style.justifyContent = 'center'
        center.style.alignItems = 'center'
        this.dom.appendChild(center)
        this.center = center

        const outerRadius = this.ringInnerRadius + this.ringWidth
        const size = outerRadius * 2

        const progressBarSvg = this.initSvg()
        const logo = document.createElement('div')
        const progressBar = document.createElement('div')
        progressBar.style.position = 'absolute'
        logo.innerHTML = new Logo({
            size,
            color: loaderColor,
            showBorder: false,
            borderWidth: this.ringWidth,
            borderOffset: 8,
        }).generate()
        this.logo = logo
        progressBar.innerHTML = progressBarSvg
        center.appendChild(progressBar)
        center.appendChild(logo)

        // @ts-expect-error
        this.track = document.getElementById('loaderTrack')
        // @ts-expect-error
        this.indicator = document.getElementById('loaderIndicator')
        // @ts-expect-error
        this.progressIndicatorMask = document.getElementById('progressIndicatorMask')
        // @ts-expect-error
        this.loaderTrackEndPoint = document.getElementById('loaderTrackEndPoint')

        this.set(0)
        this.setIndicatorOpacity(0)

        if (cfg.groups.loader.options.spinner.value) {
            this.initialized = Promise.all([
                this.animateShow(),
                this.animateShowLogo(),
                this.animateProgress(),
            ]).then(() => {})
        } else {
            this.initialized = new Promise(resolve => {
                resolve()
            })
        }
        this.animateRotation()
        this.destroyed = false
    }

    /** Initialize the SVG view. */
    initSvg(): string {
        const outerRadius = this.ringInnerRadius + this.ringWidth
        const ringCenterRadius = this.ringInnerRadius + this.ringWidth / 2
        const size = outerRadius * 2

        return svg.newSvg(
            size,
            size,
            `
            <defs>
                <g id="progressBar">
                    <use href="#loaderRing" mask="url(#loaderTrackArcMask)" />
                    <circle r="${this.ringWidth / 2}" id="loaderTrackEndPoint" />
                    <circle r="${this.ringWidth / 2}" cy="-${ringCenterRadius}" />
                </g>
                <g id="loaderRing">
                    <circle r="${outerRadius}" mask="url(#loaderTrackRingMask)"/>
                </g>
                <mask id="loaderTrackArcMask">
                    <path fill="white" id="progressIndicatorMask"/>
                </mask>
                <mask id="loaderTrackRingMask">
                    <circle fill="white" r="${outerRadius}"/>
                    <circle fill="black" r="${this.ringInnerRadius}"/>
                </mask>


            </defs>
            <g>
                <g transform="translate(${size / 2},${size / 2})" id="loaderIndicator" opacity="1">
                    <use href="#loaderRing" fill="${ghostColor}" />
                    <g transform="rotate(0,0,0)" id="loaderTrack">
                        <use xlink:href="#progressBar" fill="${loaderColor}"></use>
                    </g>
                </g>
            </g>
        `
        )
    }

    /** Destroy the component. Remove it from the stage and destroy attached callbacks. */
    destroy() {
        const self = this
        void this.initialized.then(() => {
            void this.animateHide().then(() => {
                const parent = self.dom.parentNode
                if (parent) {
                    parent.removeChild(self.dom)
                }
                self.destroyed = true
            })
        })
    }

    set(value: number) {
        this.targetValue = value
    }

    displayProgress(value: number) {
        const minAngle = 0
        const maxAngle = 359
        const outerRadius = this.ringInnerRadius + this.ringWidth
        const size = outerRadius * 2
        const clampedValue = Math.min(Math.max(value, 0), 1)
        const ringCenterRadius = this.ringInnerRadius + this.ringWidth / 2
        const angleSpan = maxAngle - minAngle
        const maskAngle = maxAngle - ((1 - clampedValue) * angleSpan - minAngle)
        const cornerPos = math.polarToCartesian(ringCenterRadius, maskAngle)
        this.progressIndicatorMask.setAttribute('d', svg.arc(size, maskAngle))
        this.loaderTrackEndPoint.setAttribute('cx', `${cornerPos.x}`)
        this.loaderTrackEndPoint.setAttribute('cy', `${cornerPos.y}`)
    }

    /** Set the opacity of the loader. */
    setIndicatorOpacity(val: number) {
        this.center.style.opacity = `${val}`
    }

    /** Set the opacity of the loader. */
    setOpacity(val: number) {
        this.dom.style.opacity = `${val}`
    }

    /** Set the rotation of the loader (angles). */
    setRotation(val: number) {
        this.track.setAttribute('transform', `rotate(${val},0,0)`)
    }

    /** Start show animation. It is used after the loader is created. */
    animateShow(): Promise<void> {
        const self = this
        const startTime = window.performance.now()
        return new Promise(function (resolve) {
            const step = (time: DOMHighResTimeStamp) => {
                const opacitySampler = Math.min((time - startTime) / (1000 * 1), 1)
                self.setIndicatorOpacity(animation.easeInOutQuad(opacitySampler))
                if (opacitySampler < 1) {
                    window.requestAnimationFrame(step)
                } else {
                    resolve()
                }
            }
            window.requestAnimationFrame(step)
        })
    }

    /** Start the progress bar animation. The progress bar grows smoothly even if the data is
     * received in chunks. */
    animateProgress(): Promise<void> {
        const self = this
        let lastTime = window.performance.now()
        self.displayProgress(self.minProgressSize)
        return new Promise(function (resolve) {
            const step = (time: DOMHighResTimeStamp) => {
                const timeDiff = time - lastTime
                lastTime = time
                if (self.animatedValue < self.targetValue) {
                    self.animatedValue = Math.min(
                        self.targetValue,
                        self.animatedValue + timeDiff / 1000
                    )
                    self.displayProgress(
                        self.minProgressSize + (1 - self.minProgressSize) * self.animatedValue
                    )
                }
                if (self.animatedValue < 1) {
                    window.requestAnimationFrame(step)
                } else {
                    resolve()
                }
            }
            window.requestAnimationFrame(step)
        })
    }

    /** Start the logo show animation. */
    animateShowLogo(): Promise<void> {
        const self = this
        const startTime = window.performance.now()
        const outerRadius = this.ringInnerRadius + this.ringWidth
        const size = outerRadius * 2
        return new Promise(function (resolve) {
            const step = (time: DOMHighResTimeStamp) => {
                const opacitySampler = Math.min((time - startTime) / (1000 * 2), 1)
                const anim = animation.elasticInOut
                self.logo.innerHTML = new Logo({
                    size,
                    color: loaderColor,
                    showBorder: false,
                    borderWidth: self.ringWidth,
                    shapeSpikeCutoff: 1 + 6 * anim.amplitude(1).period(1)(opacitySampler),
                    rotation: 100 - 100 * anim.amplitude(0.5).period(1)(opacitySampler),
                    borderOffset: 20 * (1 - anim.amplitude(1).period(0.4)(opacitySampler)) + 8,
                    shapeErosion: 30 * (1 - anim.amplitude(1).period(0.4)(opacitySampler)) - 4,
                }).generate()
                if (opacitySampler < 1) {
                    window.requestAnimationFrame(step)
                } else {
                    resolve()
                }
            }
            window.requestAnimationFrame(step)
        })
    }

    /** Start the logo hide animation. */
    animateHide(): Promise<void> {
        const self = this
        const startTime = window.performance.now()
        return new Promise(function (resolve) {
            const step = (time: DOMHighResTimeStamp) => {
                const opacitySampler = 1 - Math.min((time - startTime) / (1000 * 0.3), 1)
                self.setOpacity(animation.easeInOutQuad(opacitySampler))
                if (opacitySampler > 0) {
                    window.requestAnimationFrame(step)
                } else {
                    resolve()
                }
            }
            window.requestAnimationFrame(step)
        })
    }

    /** Start the spinning animation. */
    animateRotation() {
        const indicator = this
        let rotation = 0
        const step = (time: DOMHighResTimeStamp) => {
            indicator.setRotation(rotation)
            rotation = time / 6
            if (!indicator.destroyed) {
                window.requestAnimationFrame(step)
            }
        }
        window.requestAnimationFrame(step)
    }
}

// ==============
// === Loader ===
// ==============

/** The main loader class. It connects to the provided fetch responses and tracks their status. */
export class Loader {
    indicator: ProgressIndicator
    totalBytes: number
    receivedBytes: number
    downloadSpeed: number
    lastReceiveTime: number
    initialized: Promise<void>
    capProgressAt: number
    done: Promise<void>
    doneResolve: null | ((value: void | PromiseLike<void>) => void) = null
    constructor(cfg: Options) {
        this.indicator = new ProgressIndicator(cfg)
        this.totalBytes = 0
        this.receivedBytes = 0
        this.downloadSpeed = 0
        this.lastReceiveTime = performance.now()
        this.initialized = this.indicator.initialized
        this.capProgressAt = cfg.groups.loader.options.downloadToInitRatio.value

        this.done = new Promise(resolve => {
            this.doneResolve = resolve
        })
    }

    /** Load the provided resources. */
    load(resources: Response[]) {
        const loaderError = (msg: string) => console.error(`Loader error. ${msg}`)
        let missingContentLength = false
        for (const resource of resources) {
            const contentLength = resource.headers.get('content-length')
            if (contentLength) {
                this.totalBytes += parseInt(contentLength)
            } else {
                missingContentLength = true
            }
            const body = resource.clone().body
            if (body) {
                body.pipeTo(this.inputStream()).catch(err => logger.error(err))
            } else {
                loaderError('Cannot read the response body.')
            }
        }

        if (missingContentLength || Number.isNaN(this.totalBytes)) {
            loaderError("Server is not configured to send the 'Content-Length' metadata.")
            this.totalBytes = 0
        }
    }

    /** The current loading progress [0..1]. */
    value() {
        if (this.totalBytes == 0) {
            return 0
        } else {
            return this.receivedBytes / this.totalBytes
        }
    }

    /** Check whether the loader finished downloading all assets. */
    isDone() {
        return this.receivedBytes == this.totalBytes
    }

    /** Run the hide animation and then remove the loader DOM element. */
    destroy() {
        this.indicator.destroy()
    }

    /** Callback run on every new received byte stream. */
    onReceive(newBytes: number) {
        this.receivedBytes += newBytes
        const time = performance.now()
        const timeDiff = time - this.lastReceiveTime
        if (timeDiff > 0) {
            this.downloadSpeed = newBytes / timeDiff
            this.lastReceiveTime = time

            const percent = this.showPercentageValue()
            const speed = this.showDownloadSpeed()
            const received = this.showReceivedBytes()
            console.log(`${percent}% (${received}) (${speed}).`)

            const indicatorProgress = this.value() * this.capProgressAt
            this.indicator.set(indicatorProgress)
        }
        if (this.isDone()) {
            this.indicator.set(1)
            if (this.doneResolve) {
                this.doneResolve()
            }
        }
    }

    /** Download percentage value. */
    showPercentageValue() {
        return Math.round(100 * this.value())
    }

    /** Download total size value. */
    showTotalBytes() {
        return `${math.formatMb(this.totalBytes)} MB`
    }

    /** Download received bytes value. */
    showReceivedBytes() {
        return `${math.formatMb(this.receivedBytes)} MB`
    }

    /** Download speed value. */
    showDownloadSpeed() {
        return `${math.formatMb(1000 * this.downloadSpeed)} MB/s`
    }

    /** Internal function for attaching new fetch responses. */
    inputStream(): WritableStream<Uint8Array> {
        const loader = this
        return new WritableStream({
            write(t) {
                loader.onReceive(t.length)
            },
        })
    }
}
