extern crate web_view;

use web_view::*;

fn main() {
    //
    // TODO : écriture du token dans un espace local
    //
    //let appjs = include_str!("../front/app.js");
    //let theme = include_str!("../front/theme.css");
    //
    let appjs = include_str!("../front/testEndpoints.js");
    let theme = "";
    //
    let run = include_str!("../front/run.js");
    let app = format!(include_str!("../front/rust.html"), theme = theme, appjs = appjs, run = run);
    web_view::builder()
        .title("Tracy")
        .content(Content::Html(app))
        .size(650, 800)
        //.resizable(false)
        //.debug(true)
        .user_data(())
        .invoke_handler(|_webview, _arg| Ok(()))
        .run()
        .unwrap();
}
