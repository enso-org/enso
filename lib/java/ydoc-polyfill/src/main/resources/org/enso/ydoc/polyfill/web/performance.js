(function (jvm) {

    class Performance {

        now() {
            return jvm('now');
        }
    }

    globalThis.performance = new Performance();

})
