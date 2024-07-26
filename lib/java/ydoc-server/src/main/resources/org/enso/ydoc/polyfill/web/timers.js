(function (jvm) {

    globalThis.setInterval = function(func, delay, ...args) {
        return jvm('set-interval', func, delay, args);
    };

    globalThis.clearInterval = function(intervalID) {
        jvm('clear-interval', intervalID);
    };

    globalThis.setTimeout = function(func, delay, ...args) {
        return jvm('set-timeout', func, delay, args);
    };

    globalThis.clearTimeout = function(timeoutID) {
        jvm('clear-timeout', timeoutID);
    };

})
