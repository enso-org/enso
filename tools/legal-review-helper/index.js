const reviewRoot = "target";
const settingsRoot = "legal-review";

const app = require('express')();
const open = require('open');
const fs = require('fs');

app.get('/', function (req, res) {
  let html = "hello<br>";
  const files = fs.readdirSync("../../" + reviewRoot);
  const reports = files.map(f => f.match(/^(.*)-report.html$/)).filter(m => m != null).map(m => m[1]);
  if (reports.length == 0) {
    html += "No reports found. Run <pre style=\"display:inline\">enso / gatherLicenses</pre> first.";
  } else {
    html += "Select report:";
    html += "<ul>";
    reports.forEach(report => {
      html += "<li>" + report + "</li>";
    });
    html += "</ul>";
  }
  res.send(html);
})

const server = app.listen(0, () => {
  const port = server.address().port;
  console.log('Listening on port:', port);
  open("http://localhost:" + port + "/")
});
