var DISCOVERY_DOCS = ["https://www.googleapis.com/discovery/v1/apis/drive/v3/rest"];
var SCOPES = "https://www.googleapis.com/auth/drive.appdata";

var CLIENT_ID = "<>";
var API_KEY = "<>";

function gapi_initClient() {
    gapi.client.init({
        apiKey: API_KEY,
        clientId: CLIENT_ID,
        discoveryDocs: DISCOVERY_DOCS,
        scope: SCOPES
    }).then(function() {
        gapi.auth2.getAuthInstance().isSignedIn.listen(gapi_updateSignInStatus);
        gapi_updateSignInStatus(gapi.auth2.getAuthInstance().isSignedIn.get());
    }, function(error) {
        app.ports.received.send({"status": "Error", "desc": JSON.stringify(error)});
    });
}

function gapi_updateSignInStatus(isSignedIn) {
    app.ports.received.send({"status": isSignedIn? "Connected" : "Unconnected"});
}

function gapi_authenticate() { gapi.auth2.getAuthInstance().signIn(); }

function gapi_SignOut() { gapi.auth2.getAuthInstance().signOut(); }

function gapi_checkExistence() {
    //
    //
}

function gapi_createFile() {
    //
    //
}

function gapi_readFile() {
    //
    //
}

function gapi_updateFile() {
    //
    //
}
