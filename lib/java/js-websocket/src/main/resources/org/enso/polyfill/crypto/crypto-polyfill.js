(function (jvm) {

    var SubtleCrypto = (function () {

        function SubtleCrypto() { }

        SubtleCrypto.prototype.encrypt = function () {
            debugger;
        };

        SubtleCrypto.prototype.decrypt = function () {
            debugger;
        };

        return SubtleCrypto;
    }());

    var Crypto = (function () {

        function Crypto() {
            this.subtle_ = new SubtleCrypto();
        }

        Object.defineProperty(Crypto.prototype, 'subtle', {
            get: function () {
                return this.subtle_;
            },
            enumerable: false,
            configurable: true
        });

        Crypto.prototype.randomUUID = function () {
            return jvm('random-uuid');
        };

        Crypto.prototype.getRandomValues = function (typedArray) {
            for (let i = 0; i < typedArray.length; i++) {
                typedArray[i] = Math.floor(Math.random() * Number.MAX_SAFE_INTEGER);
            }
            return typedArray;
        };

        return Crypto;
    }());

    globalThis.crypto = new Crypto();

})
