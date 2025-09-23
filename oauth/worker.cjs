// worker.js
    const { parentPort, workerData } = require('worker_threads');

    // Perform a CPU-intensive task
    function calculateSum(limit) {
        let sum = 0;
        for (let i = 0; i <= limit; i++) {
            sum += i;
        }
        return sum;
    }

    const result = calculateSum(workerData.limit);

    // Send the result back to the main thread
    parentPort.postMessage(result);
