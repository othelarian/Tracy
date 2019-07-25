var DISCOVERY_DOCS = ["https://www.googleapis.com/discovery/v1/apis/drive/v3/rest"];
var SCOPES = "https://www.googleapis.com/auth/drive.appdata";

var app = Elm.Main.init({node: document.getElementById("elm")});
if (document.getElementById("elm")) { document.getElementById("elm").innerText = 'Bad news from elm...'; }
else {
    app.ports.ask.subscribe(function(request) {
        switch (request) {
            default: break;
        }
    });
}

// TODO : tester avec rust le passage d'info
