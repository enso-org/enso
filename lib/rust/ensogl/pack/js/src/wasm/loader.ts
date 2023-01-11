import * as animation from 'animation'
import * as html_utils from 'dom/dom'
import * as math from 'math'
import * as svg from 'dom/svg'
import { Config } from 'config/config'
import { Logo } from 'dom/logo'
import { easeInOutElastic, easeOutElastic, elasticOut } from 'animation'

// =========================
// === ProgressIndicator ===
// =========================

const loaderColor = '#3c3c3c'
const ghostColor = '#00000020'
const topLayerIndex = '1000'

/// Visual representation of the loader.
class ProgressIndicator {
    dom: HTMLDivElement
    track: HTMLElement
    indicator: HTMLElement
    progressIndicatorMask: HTMLElement
    loaderTrackEndPoint: HTMLElement
    logo: HTMLElement
    center: HTMLElement
    initialized: Promise<void[]>
    destroyed: boolean
    ringInnerRadius: number
    ringWidth: number

    value: number

    constructor(cfg: Config) {
        this.value = 0

        this.ringInnerRadius = 48
        this.ringWidth = 12

        this.dom = html_utils.new_top_level_div()
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

        const progress_bar_svg = this.init_svg()
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
        progressBar.innerHTML = progress_bar_svg
        center.appendChild(progressBar)
        center.appendChild(logo)

        //@ts-ignore
        this.track = document.getElementById('loaderTrack')
        //@ts-ignore
        this.indicator = document.getElementById('loaderIndicator')
        //@ts-ignore
        this.progressIndicatorMask = document.getElementById('progressIndicatorMask')
        //@ts-ignore
        this.loaderTrackEndPoint = document.getElementById('loaderTrackEndPoint')

        this.set(0)
        this.setIndicatorOpacity(0)

        if (cfg.useLoader) {
            this.initialized = Promise.all<void>([this.animateShow(), this.animateShowLogo()])
        } else {
            this.initialized = new Promise(resolve => {
                resolve([])
            })
        }
        this.animate_rotation()
        this.destroyed = false
    }

    /// Initializes the SVG view.
    init_svg(): string {
        const outerRadius = this.ringInnerRadius + this.ringWidth
        const ringCenterRadius = this.ringInnerRadius + this.ringWidth / 2
        const size = outerRadius * 2

        return svg.new_svg(
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

    /// Destroys the component. Removes it from the stage and destroys attached callbacks.
    destroy() {
        const self = this
        this.initialized.then(() => {
            this.animateHide().then(() => {
                const parent = self.dom.parentNode
                if (parent) {
                    parent.removeChild(self.dom)
                }
                self.destroyed = true
            })
        })
    }

    set(value: number) {
        const minAngle = 0
        const maxAngle = 359
        const outerRadius = this.ringInnerRadius + this.ringWidth
        const size = outerRadius * 2
        const clampedValue = Math.min(Math.max(value, 0), 1)
        const ringCenterRadius = this.ringInnerRadius + this.ringWidth / 2
        const angleSpan = maxAngle - minAngle
        const maskAngle = maxAngle - ((1 - clampedValue) * angleSpan - minAngle)
        const cornerPos = math.polar_to_cartesian(ringCenterRadius, maskAngle)
        this.progressIndicatorMask.setAttribute('d', svg.arc(size, maskAngle))
        this.loaderTrackEndPoint.setAttribute('cx', `${cornerPos.x}`)
        this.loaderTrackEndPoint.setAttribute('cy', `${cornerPos.y}`)
    }

    /// Set the opacity of the loader.
    setIndicatorOpacity(val: number) {
        this.center.style.opacity = `${val}`
    }

    /// Set the opacity of the loader.
    setOpacity(val: number) {
        this.dom.style.opacity = `${val}`
    }

    /// Set the rotation of the loader (angles).
    set_rotation(val: number) {
        this.track.setAttribute('transform', `rotate(${val},0,0)`)
    }

    /// Start show animation. It is used after the loader is created.
    animateShow(): Promise<void> {
        const self = this
        const startTime = window.performance.now()
        return new Promise(function (resolve) {
            function showStep(time: DOMHighResTimeStamp) {
                const opacitySampler = Math.min((time - startTime) / (1000 * 1), 1)
                self.setIndicatorOpacity(animation.ease_in_out_quad(opacitySampler))
                if (opacitySampler < 1) {
                    window.requestAnimationFrame(showStep)
                } else {
                    resolve()
                }
            }
            window.requestAnimationFrame(showStep)
        })
    }

    animateShowLogo(): Promise<void> {
        const self = this
        const startTime = window.performance.now()
        const outerRadius = this.ringInnerRadius + this.ringWidth
        const size = outerRadius * 2
        return new Promise(function (resolve) {
            function showStep(time: DOMHighResTimeStamp) {
                const opacitySampler = Math.min((time - startTime) / (1000 * 2), 1)
                self.logo.innerHTML = new Logo({
                    size,
                    color: loaderColor,
                    showBorder: false,
                    borderWidth: self.ringWidth,
                    shapeSpikeCutoff:
                        1 + 6 * animation.elasticInOut.amplitude(1).period(1)(opacitySampler),
                    rotation:
                        100 - 100 * animation.elasticInOut.amplitude(0.5).period(1)(opacitySampler),
                    borderOffset:
                        20 * (1 - animation.elasticInOut.amplitude(1).period(0.4)(opacitySampler)) +
                        8,
                    shapeErosion:
                        30 * (1 - animation.elasticInOut.amplitude(1).period(0.4)(opacitySampler)) -
                        4,
                }).generate()
                if (opacitySampler < 1) {
                    window.requestAnimationFrame(showStep)
                } else {
                    resolve()
                }
            }
            window.requestAnimationFrame(showStep)
        })
    }

    animateHide(): Promise<void> {
        const self = this
        const startTime = window.performance.now()
        return new Promise(function (resolve) {
            function hideStep(time: DOMHighResTimeStamp) {
                const opacitySampler = 1 - Math.min((time - startTime) / (1000 * 0.3), 1)
                self.setOpacity(animation.ease_in_out_quad(opacitySampler))
                if (opacitySampler > 0) {
                    window.requestAnimationFrame(hideStep)
                } else {
                    resolve()
                }
            }
            window.requestAnimationFrame(hideStep)
        })
    }

    /// Start the spinning animation.
    animate_rotation() {
        const indicator = this
        let rotation = 0
        function rotate_step(time: DOMHighResTimeStamp) {
            indicator.set_rotation(rotation)
            rotation = time / 6
            if (!indicator.destroyed) {
                window.requestAnimationFrame(rotate_step)
            }
        }
        window.requestAnimationFrame(rotate_step)
    }
}

// ==============
// === Loader ===
// ==============

/// The main loader class. It connects to the provided fetch responses and tracks their status.
export class Loader {
    indicator: ProgressIndicator
    total_bytes: number
    received_bytes: number
    download_speed: number
    lastReceiveTime: number
    initialized: Promise<void[]>
    capProgressAt: number
    done: Promise<void>
    done_resolve: null | ((value: void | PromiseLike<void>) => void) = null
    constructor(resources: Response[], cfg: Config) {
        this.indicator = new ProgressIndicator(cfg)
        this.total_bytes = 0
        this.received_bytes = 0
        this.download_speed = 0
        this.lastReceiveTime = performance.now()
        this.initialized = this.indicator.initialized
        this.capProgressAt = cfg.loaderDownloadToInitRatio.value

        this.done = new Promise(resolve => {
            this.done_resolve = resolve
        })

        let missing_content_length = false
        for (const resource of resources) {
            const content_length = resource.headers.get('content-length')
            if (content_length) {
                this.total_bytes += parseInt(content_length)
            } else {
                missing_content_length = true
            }
            const body = resource.clone().body
            if (body) {
                body.pipeTo(this.input_stream())
            } else {
                // FIXME: error
            }
        }

        if (missing_content_length || Number.isNaN(this.total_bytes)) {
            console.error(
                "Loader error. Server is not configured to send the 'Content-Length' metadata."
            )
            this.total_bytes = 0
        }
    }

    /// The current loading progress [0..1].
    value() {
        if (this.total_bytes == 0) {
            return 0.3
        } else {
            return this.received_bytes / this.total_bytes
        }
    }

    /// Returns true if the loader finished.
    is_done() {
        return this.received_bytes == this.total_bytes
    }

    /// Removes the loader with it's dom element.
    destroy() {
        this.indicator.destroy()
    }

    /// Callback run on every new received byte stream.
    on_receive(new_bytes: number) {
        this.received_bytes += new_bytes
        const time = performance.now()
        const timeDiff = time - this.lastReceiveTime
        if (timeDiff > 0) {
            this.download_speed = new_bytes / timeDiff
            this.lastReceiveTime = time

            const percent = this.show_percentage_value()
            const speed = this.show_download_speed()
            const received = this.show_received_bytes()
            console.log(`${percent}% (${received}) (${speed}).`)

            const indicator_progress = this.value() * this.capProgressAt
            this.indicator.set(indicator_progress)
        }
        if (this.is_done()) {
            if (this.done_resolve) {
                this.done_resolve()
            }
        }
    }

    /// Download percentage value.
    show_percentage_value() {
        return Math.round(100 * this.value())
    }

    /// Download total size value.
    show_total_bytes() {
        return `${math.format_mb(this.total_bytes)} MB`
    }

    /// Download received bytes value.
    show_received_bytes() {
        return `${math.format_mb(this.received_bytes)} MB`
    }

    /// Download speed value.
    show_download_speed() {
        return `${math.format_mb(1000 * this.download_speed)} MB/s`
    }

    /// Internal function for attaching new fetch responses.
    input_stream() {
        const loader = this
        return new WritableStream({
            write(t) {
                loader.on_receive(t.length)
            },
        })
    }
}
