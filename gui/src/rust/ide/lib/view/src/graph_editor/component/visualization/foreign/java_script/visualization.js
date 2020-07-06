export class Visualization {
    constructor(api) {
        this.dom     = api.root();
        this.__api__ = api;
    }
    setPreprocessor(code) {
        this.__api__.emit_preprocessor_change(code)
    }
}

export function __Visualization__() {
    return Visualization
}
