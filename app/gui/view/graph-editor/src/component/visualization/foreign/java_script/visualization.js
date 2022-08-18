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
        this.__preprocessorModule__ = null
        this.__preprocessorMethod__ = null

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
            this.__preprocessorModule__,
            this.__preprocessorMethod__
        )
    }

    /**
     * Get the current preprocessor method. See {@link setPreprocessor} for
     * more information about purpose of setting the preprocessor.
     *
     * @returns {string} Preprocessor method or `null` if no method was set.
     */
    getPreprocessorMethod() {
        return this.__preprocessorMethod__
    }

    /**
     * Get the current preprocessor's context module.
     *
     * See the [setter documentation]{@link setPreprocessor} for more information.
     *
     * @returns {string} Qualified name to preprocessor's context module.
     */
    getPreprocessorModule() {
        return this.__preprocessorModule__
    }

    /**
     * Set the preprocessor.
     *
     * Sets the preprocessor method by providing the method pointer consisting of a
     * method name and a module name defining the preprocessor method.
     *
     * @param module module containing the preprocessor method.
     * @param method the preprocessor method name. The method must be invocable
                     with one argument and return JSON-compatible result.
     */
    setPreprocessor(module, method) {
        if (module !== this.__preprocessorModule__ || method !== this.__preprocessorMethod__) {
            this.__preprocessorModule__ = module
            this.__preprocessorMethod__ = method
            this.__emitPreprocessorChange__()
        }
    }
}

export function __Visualization__() {
    return Visualization
}
