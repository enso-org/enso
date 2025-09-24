    // main.js
    import open from "open";
    import { Worker, isMainThread, parentPort, workerData } from 'worker_threads';
    //const { Worker, isMainThread, parentPort, workerData } = require('worker_threads');
    //const open = require('open');


    function make_get(baseUrl, parameters) {
      const url = new URL(baseUrl);
      for (const key in parameters) {
        url.searchParams.append(key, parameters[key]);
      }

      return url.href;
    }

    if (isMainThread) {
        console.log('Main thread started.');

        // Create a new Worker instance
        const worker = new Worker('./worker.cjs', {
            workerData: { limit: 1000000000 } // Data to pass to the worker
        });

        open(make_get('https://login.microsoftonline.com/557a086b-ff83-4d39-a7d4-3cedd3e30b8c/oauth2/v2.0/authorize', {
          'client_id': '225e3188-e3ec-4613-b8a5-4e0efac1694a',
          'response_type': 'code',
          'redirect_uri': 'https://ensoanalytics.com/msoauthtest',
          'response_mode': 'query',
          'scope': 'https://graph.microsoft.com/mail.read',
          'state': '12345'
        }));

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
