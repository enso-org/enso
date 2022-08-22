import * as animation from './animation.js'
import * as html_utils from './html_utils.js'
import * as math from './math.js'
import * as svg from './svg.js'

// =========================
// === ProgressIndicator ===
// =========================

let bg_color = 'rgb(249,250,251)'
let loader_color = '#303030'
let top_layer_index = 1000

/// Visual representation of the loader.
export class ProgressIndicator {
    constructor(cfg) {
        this.dom = html_utils.new_top_level_div()
        this.dom.id = 'loader'
        this.dom.style.position = 'fixed'
        this.dom.style.top = 0
        this.dom.style.left = 0
        // In the Cloud UI, all layers are stacked, and the progress
        // indicator must be placed at the top layer.
        this.dom.style.zIndex = top_layer_index

        let center = document.createElement('div')
        center.style.width = '100%'
        center.style.height = '100%'
        center.style.display = 'flex'
        center.style.justifyContent = 'center'
        center.style.alignItems = 'center'
        this.dom.appendChild(center)

        let progress_bar_svg = this.init_svg()
        let progress_bar = document.createElement('div')
        progress_bar.innerHTML = progress_bar_svg
        center.appendChild(progress_bar)

        this.progress_indicator = document.getElementById('progress_indicator')
        this.progress_indicator_mask = document.getElementById('progress_indicator_mask')
        this.progress_indicator_corner = document.getElementById('progress_indicator_corner')

        this.set(0)
        this.set_opacity(0)

        if (cfg.use_loader) {
            this.initialized = this.animate_show()
        } else {
            this.initialized = new Promise(resolve => {
                resolve()
            })
        }
        this.animate_rotation()
        this.destroyed = false
    }

    /// Initializes the SVG view.
    init_svg() {
        let width = 128
        let height = 128
        let alpha = 0.9
        let inner_radius = 48
        let outer_radius = 60
        let mid_radius = (inner_radius + outer_radius) / 2
        let bar_width = outer_radius - inner_radius

        return svg.new_svg(
            width,
            height,
            `
            <defs>
                <g id="progress_bar">
                    <circle fill="${loader_color}" r="${outer_radius}"                               />
                    <circle fill="${bg_color}"     r="${inner_radius}"                               />
                    <path   fill="${bg_color}"     opacity="${alpha}" id="progress_indicator_mask"   />
                    <circle fill="${loader_color}" r="${
                bar_width / 2
            }" id="progress_indicator_corner" />
                    <circle fill="${loader_color}" r="${
                bar_width / 2
            }" cy="-${mid_radius}"            />
                </g>
            </defs>
            <g transform="translate(${width / 2},${height / 2})">
                <g transform="rotate(0,0,0)" id="progress_indicator">
                    <use xlink:href="#progress_bar"></use>
                </g>
            </g>
        `
        )
    }

    /// Destroys the component. Removes it from the stage and destroys attached callbacks.
    destroy() {
        html_utils.remove_node(this.dom)
        this.destroyed = true
    }

    /// Set the value of the loader [0..1].
    set(value) {
        let min_angle = 0
        let max_angle = 359
        let angle_span = max_angle - min_angle
        let mask_angle = (1 - value) * angle_span - min_angle
        let corner_pos = math.polar_to_cartesian(54, -mask_angle)
        this.progress_indicator_mask.setAttribute('d', svg.arc(128, -mask_angle))
        this.progress_indicator_corner.setAttribute('cx', corner_pos.x)
        this.progress_indicator_corner.setAttribute('cy', corner_pos.y)
    }

    /// Set the opacity of the loader.
    set_opacity(val) {
        this.progress_indicator.setAttribute('opacity', val)
    }

    /// Set the rotation of the loader (angles).
    set_rotation(val) {
        this.progress_indicator.setAttribute('transform', `rotate(${val},0,0)`)
    }

    /// Start show animation. It is used after the loader is created.
    animate_show() {
        let indicator = this
        return new Promise(function (resolve, reject) {
            let alpha = 0
            function show_step() {
                if (alpha > 1) {
                    alpha = 1
                }
                indicator.set_opacity(animation.ease_in_out_quad(alpha))
                alpha += 0.02
                if (alpha < 1) {
                    window.requestAnimationFrame(show_step)
                } else {
                    resolve()
                }
            }
            window.requestAnimationFrame(show_step)
        })
    }

    /// Start the spinning animation.
    animate_rotation() {
        let indicator = this
        let rotation = 0
        function rotate_step(timestamp) {
            indicator.set_rotation(rotation)
            rotation += 6
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
    constructor(resources, cfg) {
        this.indicator = new ProgressIndicator(cfg)
        this.total_bytes = 0
        this.received_bytes = 0
        this.download_speed = 0
        this.last_receive_time = performance.now()
        this.initialized = this.indicator.initialized
        this.cap_progress_at = 0.3

        let self = this
        this.done_resolve = null
        this.done = new Promise(resolve => {
            self.done_resolve = resolve
        })

        for (let resource of resources) {
            this.total_bytes += parseInt(resource.headers.get('Content-Length'))
            resource.clone().body.pipeTo(this.input_stream())
        }

        if (Number.isNaN(this.total_bytes)) {
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
    on_receive(new_bytes) {
        this.received_bytes += new_bytes
        let time = performance.now()
        let time_diff = time - this.last_receive_time
        this.download_speed = new_bytes / time_diff
        this.last_receive_time = time

        let percent = this.show_percentage_value()
        let speed = this.show_download_speed()
        let received = this.show_received_bytes()
        console.log(`${percent}% (${received}) (${speed}).`)

        let indicator_progress = this.value() * this.cap_progress_at
        this.indicator.set(indicator_progress)
        if (this.is_done()) {
            this.done_resolve()
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
        let loader = this
        return new WritableStream({
            write(t) {
                loader.on_receive(t.length)
            },
        })
    }
}
