const http = require('http'); // Import the built-in HTTP module

const hostname = '127.0.0.1'; // Localhost
const port = 3000; // Port to listen on

// Create the server
const server = http.createServer((req, res) => {
  // Set the response HTTP header with status code and content type
  res.statusCode = 200; // OK
  res.setHeader('Content-Type', 'text/plain'); // Plain text response

  // Send the response body
  res.end('Hello, World!\n');
});

// Start the server and listen for incoming requests
server.listen(port, hostname, () => {
  console.log(`Server running at http://${hostname}:${port}/`);
});
