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
        this.__preprocessorArguments__ = null

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
            this.__preprocessorMethod__,
            this.__preprocessorArguments__
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
     * Get the current preprocessor's arguments.
     *
     * See the [setter documentation]{@link setPreprocessor} for more information.
     *
     * @returns {Array} The list of preprocessor arguments or `null` if none were set.
     */
    getPreprocessorArguments() {
        return this.__preprocessorArguments__
    }

    /**
     * Set the arguments of the preprocessor function.
     *
     * Arguments should be strings representing valid Enso expressions that can
     * be evaluated in the preprocessor module. See the
     * [setter documentation]{@link setPreprocessor} for more information.
     *
     * @param arguments the arguments passed to the preprocessor function.
     */
    setPreprocessorArguments(...args) {
        if (args !== this.__preprocessorArguments__) {
            this.__preprocessorArguments__ = args
            this.__emitPreprocessorChange__()
        }
    }

    /**
     * Set the preprocessor.
     *
     * Sets the preprocessor method by providing the method pointer consisting of a
     * method name and a module name defining the preprocessor method.
     *
     * An example of setting a preprocessor without arguments.
     * @example
     * this.setPreprocessor('Standard.Visualization.Preprocessor', 'default_preprocessor')
     *
     * The arguments should be strings representing Enso expressions. These
     * expressions will be evaluated in the context of preprocessor module and
     * passed to the visualization function in the same order.
     *
     * For example, given the `prepare_visualization` Enso method defined as
     * @example
     * prepare_visualization data param_1=10 param_2=True param_3=Nothing = ...
     *
     * the `setPreprocessor` method call can look like
     * @example
     * this.setPreprocessor('Foo.Bar.Baz', 'prepare_visualization', '42', 'False')
     *
     * First argument of the visualization function is always the node value
     * (`data`), followed by the configuration parameters. In this example
     * `setPreprocessor` call passes the number `42` as `param_1` and boolean
     * `False` as `param_2`. `param_3` has been left unchanged with the default
     * value `Nothing` since it was not specified in the example
     * `setPreprocessor` call.
     *
     * @param module The qualified module containing the preprocessor method.
     * @param method The preprocessor method name. The method must be invocable
     *               with one argument and return JSON-compatible result.
     * @param arguments Positional arguments passed to the preprocessor function.
     *                  Arguments should be strings representing valid Enso
     *                  expressions that can be evaluated in the preprocessor module.
     */
    setPreprocessor(module, method, ...args) {
        if (
            module !== this.__preprocessorModule__ ||
            method !== this.__preprocessorMethod__ ||
            args !== this.__preprocessorArguments__
        ) {
            this.__preprocessorModule__ = module
            this.__preprocessorMethod__ = method
            this.__preprocessorArguments__ = args
            this.__emitPreprocessorChange__()
        }
    }
}

export function __Visualization__() {
    return Visualization
}
