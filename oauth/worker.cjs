// worker.js
const { parentPort, workerData } = require('worker_threads');

const http = require('http'); // Import the built-in HTTP module

const hostname = '127.0.0.1'; // Localhost
const port = 3000; // Port to listen on

// Create the server
const server = http.createServer((req, res) => {
  if (req.url == "/favicon.ico") {
    res.statusCode = 200; // OK
    res.end();
    return;
  }

  // Set the response HTTP header with status code and content type
  res.statusCode = 200; // OK
  res.setHeader('Content-Type', 'text/plain'); // Plain text response

  const url = require('url');
  //console.log("worker: " + req.url);

  // Send the response body
  var s = "";
  s += 'Hello, World, limit = ' + workerData.limit + '!\n\n';
  //s += req.url + "\n\n";
  const auth_code = url.parse(req.url, true).query.code;
  s += auth_code + "\n\n";
  res.end(s);

  const result = { auth_code: auth_code };
  parentPort.postMessage(result);
});

// Start the server and listen for incoming requests
server.listen(port, hostname, () => {
  console.log(`Server running at http://${hostname}:${port}/`);
});
