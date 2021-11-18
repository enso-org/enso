export class Visualization {
    /**
     * Class constructor.
     *
     * The argument is IDE's implementation detail.
     *
     * When subclassing and using custom constructor, the argument must be passed
     * to this constructor and visualization implementation is not allowed to
     * perform any other operation on it.
     *
     * @param api an internal IDE structure. Should be passed intact by a subclass
     *            constructor (if present).
     */
    constructor(api) {
        // These go before `api` assignment so the `undefined` is not emitted to IDE.
        // First we will give deriving type a chance to overwrite them, then IDE will
        // invoke `__emitPreprocessorChange__()` on this.
        this.__preprocessorCode__ = null
        this.__preprocessorModule__ = null

        this.dom = api.root()
        this.theme = api.theme()
        this.__api__ = api
    }

    /**
     * Notify IDE that preprocessor settings have changed.
     * @private
     */
    __emitPreprocessorChange__() {
        this.__api__.emit_preprocessor_change(
            this.__preprocessorCode__,
            this.__preprocessorModule__
        )
    }

    /**
     * Get the current preprocessor code. See {@link setPreprocessorCode} for
     * more information about purpose of setting preprocessor code.
     *
     * @returns {string} Preprocessor code or `null` if no code was set.
     */
    getPreprocessorCode() {
        return this.__preprocessorCode__
    }

    /**
     * Set new preprocessor code.
     *
     * When visualization is attached to a node, each time a new value is produced from node,
     * the preprocessor shall be invoked with it. Result such call shall be serialized and
     * transported to visualization by invoking onDataReceived(data) method.
     *
     * Typically the preprocessor is a lambda, like the example below:
     *
     * `x -> x.to_default_visualization_data`
     *
     *
     * The code by default runs in the context of the current project's `Main` module.
     * If other context is needed (e.g. due to required import or other module-specific
     * context dependency), the {@link setPreprocessorModule} should be used to provide
     * the module's name.
     *
     * Please refer to [documentation]{@link https://dev.enso.org/docs/ide/product/visualizations.html#lazy-visualizations}
     * for details.
     *
     * @param {string} code text code in Enso. It must be invokable with one argument and return
     *                 JSON-compatible result. For example:
     *                 <pre><code>x -> x.to_default_visualization_data</code></pre>
     */
    setPreprocessorCode(code) {
        if (code !== this.__preprocessorCode__) {
            this.__preprocessorCode__ = code
            this.__emitPreprocessorChange__()
        }
    }

    /**
     * Get the current preprocessor's context module.
     *
     * See the [setter documentation]{@link setPreprocessorModule} for more information.
     *
     * @returns {string} Qualified name to preprocessor's context module.
     */
    getPreprocessorModule() {
        return this.__preprocessorModule__
    }

    /**
     * Set preprocessor's context module.
     *
     * [Preprocessor code]{@link setPreprocessorCode} is executed in the context of
     * certain Enso module. This decides what symbols are visible and available to
     * preprocessor, as everything that preprocessor uses must defined or imported
     * in the context module.
     *
     * If never set, Engine will use the current project's `Main` module as the context.
     *
     * @param module
     */
    setPreprocessorModule(module) {
        if (module !== this.__preprocessorModule__) {
            this.__preprocessorModule__ = module
            this.__emitPreprocessorChange__()
        } else {
            console.error('skipping, as', module, ' === ', this.__preprocessorModule__)
        }
    }

    /**
     * Set both preprocessor's code and context module.
     *
     * This is like calling both {@link setPreprocessorModule} and
     * {@link setPreprocessorCode}, however may be more efficient, as it will emit
     * only one update request.
     *
     * During the visualization construction phase no partial updates are emitted,
     * so using this method gives no additional benefit.
     *
     * @param code preprocessor code to be set.
     * @param module context module for the preprocessor execution.
     */
    setPreprocessor(code, module) {
        if (code !== this.__preprocessorCode__ || code !== this.__preprocessorModule__) {
            this.__preprocessorCode__ = code
            this.__preprocessorModule__ = module
            this.__emitPreprocessorChange__()
        }
    }
}

export function __Visualization__() {
    return Visualization
}
