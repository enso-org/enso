(function (jvm) {

    class CryptoSubtle {}

    class Crypto {

        #subtle = new CryptoSubtle();

        get subtle() {
            return this.#subtle;
        }

        randomUUID() {
            return jvm('random-uuid');
        }

        getRandomValues(typedArray) {
            for (let i = 0; i < typedArray.length; i++) {
                typedArray[i] = Math.floor(Math.random() * Number.MAX_SAFE_INTEGER);
            }
            return typedArray;
        }
    }

    globalThis.crypto = new Crypto();

})
