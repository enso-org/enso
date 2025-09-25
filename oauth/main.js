// main.js
import open from "open";
import { Worker, isMainThread, parentPort, workerData } from 'worker_threads';
import https from 'https';
import axios from 'axios';
//const { Worker, isMainThread, parentPort, workerData } = require('worker_threads');
//const open = require('open');

(async () => {
    const client_secret = process.env.OAUTH_CLIENT_SECRET;

    function make_get(baseUrl, parameters) {
      const url = new URL(baseUrl);
      for (const key in parameters) {
        url.searchParams.append(key, parameters[key]);
      }

      return url.href;
    }

    async function get_access_token(auth_code) {
      const tokenEndpoint = 'https://login.microsoftonline.com/557a086b-ff83-4d39-a7d4-3cedd3e30b8c/oauth2/v2.0/token';
      //const tokenEndpoint = 'https://login.microsoftonline.com/59c2b5a8-8575-4ce0-9ff5-be8f2b34ad63/oauth2/v2.0/token';

      const data = new URLSearchParams();

      data.append('client_id', '225e3188-e3ec-4613-b8a5-4e0efac1694a');
      //data.append('client_id', '087cad1c-ab83-476d-bb1b-47ed1c5be4ef');
      data.append('scope', 'openid offline_access https://graph.microsoft.com/mail.read');
      data.append('code', auth_code);
      data.append('redirect_uri', 'http://localhost:3000');
      data.append('grant_type', 'authorization_code');
      data.append('client_secret', client_secret);

      const result = await axios.post(tokenEndpoint, data, {
          headers: {
              'Content-Type': 'application/x-www-form-urlencoded'
          }
      });
      return result.data.access_token;
    }

    async function get_authentication_code(worker) {
        return new Promise((resolve, reject) => {
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
          worker.on('message', async (result) => {
              //console.log('Result from worker:', result);
              const { auth_code } = result;
              //console.log('* auth_code ' + auth_code);
              resolve(auth_code);
          });

          // Listen for errors from the worker thread
          worker.on('error', (err) => {
              console.error('Worker error:', err);
              reject(err);
          });

          // Listen for the worker thread to exit
          worker.on('exit', (code) => {
              if (code !== 0) {
                  console.error(`Worker stopped with exit code ${code}`);
                  reject(new Error(`Worker exited with code ${code}`));
              } else {
                  console.log('Worker finished successfully.');
              }
          });
        });
    }

    if (isMainThread) {
        console.log('Main thread started.');

        // Create a new Worker instance
        const worker = new Worker('./worker.cjs', {
            workerData: { limit: 1000000000 } // Data to pass to the worker
        });

        console.log('Main thread continues its work...');

        const authentication_code = await get_authentication_code(worker);
        const access_token = await get_access_token(authentication_code);
        //console.log('* access_token: ' + access_token);

        const api_url = 'https://graph.microsoft.com/v1.0/me';
        //const api_url = 'https://graph.microsoft.com/v1.0/me/drive/root/children';
        const headers = { 'Authorization': access_token };
        try {
          const response = await axios.get(api_url, { headers: headers });
          console.log('Response Status:', response.status);
          const data = response.data;
          console.log('Response Data:', data);
          //console.log('Response Headers:', response.headers);
        } catch (error) {
          console.error('Error fetching data:', error.message);
          if (error.response) {
            console.error('Error Response Data:', error.response.data);
            console.error('Error Response Status:', error.response.status);
            console.error('Error Response Headers:', error.response.headers);
          }
        }
    } else {
        // This block will be executed if this file is run as a worker
        // (though in this example, it's not designed to be)
        console.log('This code should not be executed in a worker context for this example.');
    }
})()
  .catch(err => console.log('Fatal error', err));
