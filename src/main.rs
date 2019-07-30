extern crate clap;
extern crate directories;
extern crate micro_http_server;
extern crate web_view;

use clap::{Arg, App};
use directories::ProjectDirs;
use micro_http_server::{MicroHTTP, Client};
use web_view::*;
use std::io;
use std::fs;
use std::path::Path;
use std::{thread, time};

struct ConfigView<'a> {
    url: &'a str,
    width: i32,
    height: i32
}

fn validate_port(v: String) -> Result<(), String> {
    match v.parse::<u16>().is_ok() {
        true => Ok(()),
        false => Err(String::from("This is not a valid integer for the port"))
    }
}

fn main() {
    // get clap infos
    let matches = App::new("Tracy")
        .version("1.2.0")
        .about("Todo tRACker for Yourself (yes it's a retro acronym ;)")
        .author("othelarian")
        .arg(Arg::with_name("port")
            .help("Set this value if you want to change the app port (default: 8001)")
            .short("p")
            .long("port")
            .value_name("PORT")
            .takes_value(true)
            .validator(validate_port)
        )
        .arg(Arg::with_name("server")
            .help("Use this value to launch Tracy back only as a server")
            .short("s")
            .long("server")
        )
        .get_matches();
    let served = matches.is_present("server");
    let port = matches.value_of("port").unwrap_or("8001");
    // check configuration file
    match ProjectDirs::from("inc", "othelarian", "tracy") {
        Some(proj_dirs) => {
            let config_path = proj_dirs.config_dir();
            match config_path.exists() {
                true => {
                    let pathbuf = config_path.join("tracy.conf");
                    let config_file = pathbuf.as_path();
                    match config_file.exists() {
                        true => Ok(()),
                        false => {
                            match fs::write(config_file, "") {
                                Ok(_) => Ok(()),
                                Err(_) => Err(())
                            }
                        }
                    }
                }
                false => {
                    match fs::create_dir_all(config_path) {
                        Ok(_) => {
                            let pathbuf = config_path.join("tracy.conf");
                            let config_file = pathbuf.as_path();
                            match fs::write(config_file, "") {
                                Ok(_) => Ok(()),
                                Err(_) => Err(())
                            }
                        }
                        Err(_) => Err(())
                    }
                }
            }
        }
        None => Err(())
    }.unwrap();
    // starting micro server
    let server = MicroHTTP::new(&format!("127.0.0.1:{}", &port))
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
                    client.respond_ok("No request :(".as_bytes())
                        .expect("Could not send data to client.");
                } else {
                    let request_copy: &str = client.request().as_ref().unwrap();
                    let url_split: Vec<&str> = request_copy.split('?').collect();
                    let sending = match url_split.get(0) {
                        Some(path) => {
                            match path {
                                &"/" => get_statics(&mut client, "app.html"),
                                &"/app.html" => get_statics(&mut client, "app.html"),
                                &"/theme.css" => get_statics(&mut client, "theme.css"),
                                &"/app.js" => get_statics(&mut client, "app.js"),
                                &"/keys.js" => get_statics(&mut client, "keys.js"),
                                &"/getToken" => get_token(&mut client),
                                &"/saveToken" => save_token(&mut client),
                                &"/windows" => get_statics(&mut client, "windows.html"),
                                _ => client.respond("404 Not Found", "No resource found".as_bytes(), &vec!())
                            }
                        }
                        None => client.respond("400 Bad Request", "Bad request".as_bytes(), &vec!())
                    };
                    sending.expect("Could not send data to client.");
                }
            }
        }
    });
    if served {
        println!("You launch Tracy back as a server, on port {}", port);
        let mut w = String::new();
        io::stdin().read_line(&mut w).unwrap();
        return;
    }
    // start the web view
    let mut url = format!("http://localhost:{}/", &port);
    let config_view = if cfg!(windows) {
        println!("web view support on windows is bad enough to go play with the browser");
        url.push_str("windows");
        ConfigView {url: &url, width: 250, height: 200}
    } else {
        ConfigView {url: &url, width: 600, height: 800}
    };
    web_view::builder()
        .title("Tracy")
        .content(Content::Url(config_view.url))
        .size(config_view.width, config_view.height)
        .user_data(())
        .debug(true)
        .invoke_handler(|_webview, _arg| Ok(()))
        .run()
        .unwrap();
}

fn get_statics(client: &mut Client, resource: &str) -> io::Result<usize> {
    let result = if cfg!(debug_assertions) {
        let fp = &format!("front/{}", resource);
        match Path::new(fp).exists() {
            true => fs::read_to_string(fp),
            false => Err(io::Error::new(io::ErrorKind::Other, "No file found"))
        }
    } else {
        match resource {
            "app.html" => Ok(format!("{}", include_str!("../front/app.html"))),
            "theme.css" => Ok(format!("{}", include_str!("../front/theme.css"))),
            "app.js" => Ok(format!("{}", include_str!("../front/app.js"))),
            "windows.html" => Ok(format!("{}", include_str!("../front/windows.html"))),
            _ => Err(io::Error::new(io::ErrorKind::Other, "We never found what you asked for"))
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

fn get_token(client: &mut Client) -> io::Result<usize> {
    match ProjectDirs::from("inc", "othelarian", "tracy") {
        Some(proj_dirs) => {
            let config_path = proj_dirs.config_dir();
            let pathbuf = config_path.join("tracy.conf");
            let config_file = pathbuf.as_path();
            let rep = match fs::read_to_string(config_file) {
                Ok(token) => token, Err(_) => String::from("failed")
            };
            client.respond_ok(rep.as_bytes())
        }
        None => client.respond_ok("failed".as_bytes())
    }
}

fn save_token(client: &mut Client) -> io::Result<usize> {
    match ProjectDirs::from("inc", "othelarian", "tracy") {
        Some(proj_dirs) => {
            let token = &client.request().as_ref().unwrap().to_owned()[17..];
            let config_path = proj_dirs.config_dir();
            let pathbuf = config_path.join("tracy.conf");
            let config_file = pathbuf.as_path();
            let rep = match fs::write(config_file, token) {
                Ok(_) => "saved", Err(_) => "failed"
            };
            client.respond_ok(rep.as_bytes())
        }
        None => client.respond_ok("failed".as_bytes())
    }
}
