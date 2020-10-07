const reviewRoot = "../../target";
const settingsRoot = "../../legal-review";

const express = require('express');
const app = express();
const open = require('open');
const fs = require('fs');
const path = require('path');

app.get('/', function (req, res) {
  let html = "<h1>Choose report</h1>";
  const files = fs.readdirSync(reviewRoot);
  const reports = files.map(f => f.match(/^(.*)-report.html$/)).filter(m => m != null).map(m => m[1]);
  if (reports.length == 0) {
    html += "No reports found. Run <pre style=\"display:inline\">enso / gatherLicenses</pre> first.";
  } else {
    html += "Select report:";
    html += "<ul>";
    reports.forEach(report => {
      html += "<li><a href=\"/report/" + report + "\">" + report + "</a></li>";
    });
    html += "</ul>";
  }
  res.send(html);
});

app.use("/static", express.static("static"));

app.get('/report/:report', function (req, res) {
  const report = req.params["report"];
  console.log("Opening ", report);
  fs.readFile(path.join(reviewRoot, report + "-report.html"), "utf-8", (err, data) => {
    const injection = "<script src=\"/static/inject.js\"></script>" +
       "<script>var reportName = \"" + report + "\";</script>";
    if (err) {
      res.status(400).send(err);
    } else {
      const injected = data.replace("</head>", injection + "</head>");
      res.send(injected);
    }
  });
});

function addLine(report, package, file, line) {
  const dir = path.join(settingsRoot, report, package);
  const location = path.join(dir, file);
  console.log("Adding " + line + " to " + location);
  fs.mkdirSync(dir, {"recursive": true});
  fs.appendFileSync(location, line + "\n");
}

function removeLine(report, package, file, line) {
  const location = path.join(settingsRoot, report, package, file);
  console.log("Removing " + line + " from " + location);
  const lines = fs.readFileSync(location, "utf-8").split(/\r?\n/).filter(x => x.length > 0);
  const toRemove = lines.filter(x => x == line);
  const others = lines.filter(x => x != line);
  if (toRemove.length == 0) {
    throw "Line " + line + " was not present in the file. Are you sure the report is up to date?";
  } else {
    var newContent = others.join("\n") + "\n";
    if (others.length == 0) {
      newContent = "";
    }
    fs.writeFileSync(location, newContent);
  }
}

app.use(express.urlencoded());
app.post('/modify/:report', function (req, res) {
  const report = req.params["report"];
  const package = req.body["package"];
  const action = req.body["action"];
  const file = req.body["file"];
  const line = req.body["line"];

  try {
    if (action == "add") {
      addLine(report, package, file, line);
    } else if (action == "remove") {
      removeLine(report, package, file, line);
    } else {
      throw "Unknown action";
    }
    res.send("OK");
  } catch (error) {
    console.error(error);
    res.status(500).send(error);
  }
});

const server = app.listen(0, () => {
  const port = server.address().port;
  console.log('Listening on at ', "http://localhost:" + port + "/");
  open("http://localhost:" + port + "/")
});
