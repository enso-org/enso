    // main.js
    import open from "open";
    import { Worker, isMainThread, parentPort, workerData } from 'worker_threads';
    //const { Worker, isMainThread, parentPort, workerData } = require('worker_threads');
    //const open = require('open');

    if (isMainThread) {
        console.log('Main thread started.');

        // Create a new Worker instance
        const worker = new Worker('./worker.cjs', {
            workerData: { limit: 1000000000 } // Data to pass to the worker
        });

        open('https://cnn.com/');

        // Listen for messages from the worker thread
        worker.on('message', (result) => {
            console.log('Result from worker:', result);
        });

        // Listen for errors from the worker thread
        worker.on('error', (err) => {
            console.error('Worker error:', err);
        });

        // Listen for the worker thread to exit
        worker.on('exit', (code) => {
            if (code !== 0) {
                console.error(`Worker stopped with exit code ${code}`);
            } else {
                console.log('Worker finished successfully.');
            }
        });

        console.log('Main thread continues its work...');

    } else {
        // This block will be executed if this file is run as a worker
        // (though in this example, it's not designed to be)
        console.log('This code should not be executed in a worker context for this example.');
    }
