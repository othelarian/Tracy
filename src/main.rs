extern crate micro_http_server;
extern crate web_view;

use micro_http_server::{MicroHTTP, Client};
use web_view::*;

use std::path::Path;
use std::{thread, time};

fn main() {
    // check configuration file
    //
    // TODO : initialisation des données de configuration, avec notamment le refresh token s'il est présent
    //
    // starting micro server
    let server = MicroHTTP::new("127.0.0.1:8000")
        .expect("Cound not create server.");
    thread::spawn(move || loop {
        let result = server.next_client();
        if result.is_err() {
            println!("Server failed: {:?}", result);
            break;
        }
        match result.unwrap() {
            None => thread::sleep(time::Duration::from_millis(500)),
            Some(mut client) => {
                if client.request().is_none() {
                    println!("Client {} didn't send any request", client.addr());
                    client.respond_ok("No request :(".as_bytes())
                        .expect("Could not send data to client.");
                } else {
                    let request_copy: &str = client.request().as_ref().unwrap();
                    let sending = match request_copy {
                        "/" => get_statics(&mut client, "app.html"),
                        "/theme.css" => get_statics(&mut client, "theme.css"),
                        "/app.js" => get_statics(&mut client, "app.js"),
                        "/keys.js" => get_statics(&mut client, "keys.js"),
                        //
                        "/test.html" => get_statics(&mut client, "test.html"),
                        "/testEndpoints.js" => get_statics(&mut client, "testEndpoints.js"),
                        //
                        // TODO : ajouter les entrées manquantes (notamment pour le refresh token)
                        //
                        //
                        _ => client.respond("404 Not Found", "No resource found".as_bytes(), &vec!())
                    };
                    sending.expect("Could not send data to client.");
                }
            }
        }
    });
    // start the web view
    web_view::builder()
        .title("Tracy")
        .content(Content::Url("http://localhost:8000/test.html"))
        .size(600, 800)
        .user_data(())
        //.debug(true)
        .invoke_handler(|_webview, _arg| Ok(()))
        .run()
        .unwrap();
}

fn get_statics(client: &mut Client, resource: &str) -> std::io::Result<usize> {
    let result = if cfg!(debug_assertions) {
        let fp = &format!("front/{}", resource);
        match Path::new(fp).exists() {
            true => std::fs::read_to_string(fp),
            false => Err(std::io::Error::new(std::io::ErrorKind::Other, "No file found"))
        }
    } else {
        match resource {
            "app.html" => Ok(format!("{}", include_str!("../front/app.html"))),
            "theme.css" => Ok(format!("{}", include_str!("../front/theme.css"))),
            "app.js" => Ok(format!("{}", include_str!("../front/app.js"))),
            //"keys.js" => Ok(format!("{}", include_str!("../front/keys.js"))),
            //
            //
            // TODO : ajouter les champs manquant
            //
            //
            _ => Err(std::io::Error::new(std::io::ErrorKind::Other, "We never found what you asked for"))
        }
    };
    match result {
        Ok(content) => client.respond_ok(content.as_bytes()),
        Err(error) => {
            let msg = format!("Error on providing static resource: {}", error);
            client.respond("500 Server Error", msg.as_bytes(), &vec!())
        }
    }
}
