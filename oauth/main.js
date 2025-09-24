    // main.js
    import open from "open";
    import { Worker, isMainThread, parentPort, workerData } from 'worker_threads';
    import https from 'https';
    //const { Worker, isMainThread, parentPort, workerData } = require('worker_threads');
    //const open = require('open');

    const client_secret = process.env.OAUTH_CLIENT_SECRET;

    function make_get(baseUrl, parameters) {
      const url = new URL(baseUrl);
      for (const key in parameters) {
        url.searchParams.append(key, parameters[key]);
      }

      return url.href;
    }

    function get_auth_token(auth_code) {
      const data = JSON.stringify({
        'client_id': '087cad1c-ab83-476d-bb1b-47ed1c5be4ef',
        'scope': 'openid offline_access https://graph.microsoft.com/mail.read',
        'code': auth_code,
        'redirect_uri': 'https://ensoanalytics.com/msoauthtest',
        grant_type: 'authorization_code',
        'client_secret': client_secret,
      });

      const options = {
        hostname: 'login.microsoftonline.com',
        path: '/59c2b5a8-8575-4ce0-9ff5-be8f2b34ad63/oauth2/v2.0/token',
        port: 443, // 80 for HTTP
        method: 'POST',
        headers: {
          'Content-Type': 'application/x-www-form-urlencoded'
        }
      };

      const req = https.request(options, (res) => {
        console.log(`auth token request statusCode: ${res.statusCode}`);

        res.on('data', (d) => {
          process.stdout.write(d);
          console.log(d);
        });
      });

      req.on('error', (error) => {
        console.error(error);
      });

      req.write(data);
      req.end();
    }

    if (isMainThread) {
        console.log('Main thread started.');

        // Create a new Worker instance
        const worker = new Worker('./worker.cjs', {
            workerData: { limit: 1000000000 } // Data to pass to the worker
        });

        console.log("Starting oauth");
        open(make_get('https://login.microsoftonline.com/557a086b-ff83-4d39-a7d4-3cedd3e30b8c/oauth2/v2.0/authorize', {
          'client_id': '225e3188-e3ec-4613-b8a5-4e0efac1694a',
          'response_type': 'code',
          //'redirect_uri': 'https://ensoanalytics.com/msoauthtest',
          'redirect_uri': 'http://localhost:3000',
          'response_mode': 'query',
          'scope': 'https://graph.microsoft.com/mail.read',
          'state': '12345'
        }));

        // Listen for messages from the worker thread
        worker.on('message', (result) => {
            console.log('Result from worker:', result);
            const { auth_code } = result;
            console.log('auth_code ' + auth_code);
            const auth_token = get_auth_token(auth_code);
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
