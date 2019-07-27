# Tracy, todo TRACker for Yourself

This project is more a PoC than a real app, but it's still useful to
track your personal todo list without rely on a company to manage
your data across multiple device (only google, but you're used to
now ;) ).

## What Tracy do?

Tracy help me, and maybe you, to keep in mind what you want to do
with this small project you have in a corner of your mind. You know
you will maybe code it, one day, but for now you just want to put
some notes about it, somewhere you know you can find seek for this
notes, someday, if you have time.

Tracy is in fact a small web interface, wrote in elm, with a rust
backend and webview to be able to handle the refresh token (something
from google). It uses Google Drive hidden folder, the mysterious
"appData", to save your data in the cloud.

## How to use Tracy?

Simple, you have two choices:
* launch the rust app
* put the elm files on a webserver

Hum, not that easy, let see more in depth how to do that.

### The client ID, client secret, the Api Key, and Google

Tracy uses Google Drive to store its data, in the hidden place called
"appData", where android apps for example save their configuration,
user inputs, etc. This spicificity means that you must use a Google
client ID / secret and Api Key to enable Tracy to connect to Google
Drive.

It's in my opinion the most difficult part if you want to use Tracy,
but it isn't as challenging as you might think. Il you already have
a Google account, you can already create all the stuff you need.

I did my own setup a long time ago, so I can't explain it here.
But I can share the link where you can find all you need:

[Google Developer Console](https://console.developers.google.com)

As soon as you have a valid client ID and Api Key, you must add a
new file in front/src, called Keys.elm, with the following lines:

<code>
module Keys exposing (..)

getClientId = "myclientid"

getClientSecret = "myclientsecret"

getApiKey = "myapikey"

getScopes = "https://www.googleapis.com/auth/drive.appdata"

</code>

This file is required to enable Tracy to connect to the drive.

Good luck with the Google Developer Console, and if you run into
trouble, just push an issue, maybe I can help, if I can remember
how I did this part the first time.

### The webserver (the easier way)

Unless you custom this project to add the missing part, there is one
big issue: every hour you need to refresh the app, and loose everything
your haven't saved yet in the process. So be careful with this
solution!

Do only use the elm interface, proceed as below:

1. install elm (and optionnaly uglify)
1. reach "src/front"
2. use elm binary to compile: elm make src/Main.elm --optimize --output=app.js
3. use this files to play with Tracy:

* app.html
* theme.css
* app.js

(!! Actually it's a bit more complex, but this will completely true
soon !!)

you can use uglify to make it smaller.
[All the doc is here](https://guide.elm-lang.org/optimization/asset_size.html)

Keep in mind that you're always playing against the clock, just one
hour before the refresh. Maybe one day I'll add the auto refresh
feature, but as long as I will use the rust app, I don't need it.

### The rust app

The real good stuff! I love small apps you can really touch, the
one that is not somewhere you can't reached.

Ok, in fact it's the first way I found interesting to work around with
the refresh token of google, without using pure js boilerplate to
handle local storage or cookie. And because I love writing code, and
I'm always looking forward to the next new thing I can learn, so I
took this as a challenge. So I decided to do it with rust. Yeah,
some people called me crazy...

In fact, it's pretty trivial! You just have to do what you can find in
the previous sections, and add a cargo build --release, and you're
ready to go! Really, it is that simple. oh, only to install
[rust](https://www.rust-lang.org/)

When you launch the app, you just have to authorize google, but the
process is clear enough to let you go alone.

Enjoy!

## NOT PRODUCTION READY!

Even if the project is stable enough (I'm currently using it instead
if github issues to handle the last specifications) it's still in
progress, mostly on the google authentication part. So use it at your
own risk.

## I love Tracy, can I help to make her grow?

Why not, but Tracy is actually nearly finished, just some small
elements to finish (big ones in fact, but it's ok). What you can do is
fork it, custom it, and share your new version. I'm opened to new
ideas.
