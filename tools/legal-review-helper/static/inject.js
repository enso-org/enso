/** Sets a status text in bottom left part of the screen. */
function setStatus(text, color) {
  var status = $("#status");
  status.html(text);
  if (color === undefined) {
    color = "white";
  }
  status.css("background-color", color);
}

/** Creates a handler that will request to add or remove a line from a file. */
function makeHandler(elem, data, file, action) {
  return function (ev) {
    data["file"] = file;
    data["action"] = action;
    $.post("/modify/" + reportName, data, function (response) {
      $(elem).html(
        '<span style="color:gray">Modified, if you want to ' +
          "change this value, regenerate the report first</span>"
      );
      var tab = $(elem).closest("div").parent();
      var title = tab.children("h4");
      tab.accordion("option", "active", false);
      var info = "added " + file;
      if (action == "remove") {
        info = "undone review";
      }
      var newTitle =
        '<span style="text-decoration: line-through;">' +
        title.html() +
        "</span><br>" +
        info;
      title.html(newTitle);
      title.find("span").css("color", "gray");
      setStatus("Review for " + data["package"] + " sent.");
    }).fail(function (err) {
      setStatus("Failed to send review: " + JSON.stringify(err), "red");
    });
    setStatus("Sending review...");
  };
}

$(function () {
  $("body").append(
    '<div id="status" ' +
      'style="position: fixed;left:4pt;bottom:4pt">' +
      "Loading...</div>"
  );
  var copys = $(".copyright-ui");
  var files = $(".file-ui");

  copyrightMap = {
    Ignore: "copyright-ignore",
    KeepWithContext: "copyright-keep-context",
    Keep: "copyright-keep",
  };

  copys.each(function (index) {
    var package = $(this).data("package");
    var content = atob($(this).data("content"));
    var status = $(this).data("status");
    var contexts = parseInt($(this).data("contexts"));
    var data = {
      line: content,
      package: package,
    };
    if (status == "NotReviewed") {
      var buttons =
        '<button class="ignore">Ignore</button>' +
        '<button class="keep">Keep</button>' +
        '<button class="keepctx">Keep as context</button>';
      $(this).html(buttons);
      $(this)
        .children(".ignore")
        .on("click", makeHandler(this, data, "copyright-ignore", "add"));
      $(this)
        .children(".keep")
        .on("click", makeHandler(this, data, "copyright-keep", "add"));
      if (contexts == 1) {
        $(this)
          .children(".keepctx")
          .on(
            "click",
            makeHandler(this, data, "copyright-keep-context", "add")
          );
      } else {
        $(this).children(".keepctx").attr("disabled", true);
      }
    } else if (status != "Added") {
      $(this).html("<button>Undo review</button>");
      $(this)
        .children("button")
        .on("click", makeHandler(this, data, copyrightMap[status], "remove"));
    } else {
      $(this).html("<button disabled>This notice was added manually</button>");
    }
  });

  filesMap = {
    Ignore: "files-ignore",
    Keep: "files-keep",
  };

  files.each(function (index) {
    var package = $(this).data("package");
    var filename = $(this).data("filename");
    var status = $(this).data("status");
    var data = {
      line: filename,
      package: package,
    };
    if (status == "NotReviewed") {
      var buttons =
        '<button class="ignore">Ignore</button>' +
        '<button class="keep">Keep</button>';
      $(this).html(buttons);
      $(this)
        .children(".ignore")
        .on("click", makeHandler(this, data, "files-ignore", "add"));
      $(this)
        .children(".keep")
        .on("click", makeHandler(this, data, "files-keep", "add"));
    } else if (status != "Added") {
      $(this).html("<button>Undo review</button>");
      $(this)
        .children("button")
        .on("click", makeHandler(this, data, filesMap[status], "remove"));
    } else {
      $(this).html("<button disabled>This file was added manually</button>");
    }
  });

  setStatus("Initialized");
});
