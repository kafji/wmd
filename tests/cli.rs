struct ProcessGuard(std::process::Child);

impl Drop for ProcessGuard {
    fn drop(&mut self) {
        println!("killing pid {}", self.0.id());
        self.0.kill().unwrap()
    }
}

mod cli {

    use super::*;
    use assert_cmd::cargo::CommandCargoExt;
    use rand::Rng;
    use reqwest::StatusCode;
    use std::{
        process::{Command, Stdio},
        thread,
        time::Duration,
    };
    use tokio::{sync::oneshot, time::sleep};

    #[tokio::test]
    async fn serve_starts_server() {
        // todo(kfj): might roll into bounded port
        let port: u16 = rand::thread_rng().gen_range(49152..=65535);
        println!("port: {}", port);

        let (ready_tx, ready_rx) = oneshot::channel();

        let server = thread::spawn(move || {
            let mut cmd = Command::cargo_bin(env!("CARGO_PKG_NAME")).unwrap();
            cmd.args([
                "serve",
                "-p",
                &port.to_string(),
                "-C",
                "./tpyo.example.toml",
            ]);
            cmd.stdout(Stdio::null());
            let ps = cmd.spawn().unwrap();
            println!("server process spawned, pid: {}", ps.id());

            ready_tx.send(()).unwrap();

            ProcessGuard(ps)
        });

        let url = &format!("http://127.0.0.1:{}/", port);
        println!("url: {}", url);

        ready_rx.await.unwrap();

        println!("sending request");
        let resp = {
            let mut tries = 0;
            loop {
                let resp = reqwest::get(url).await;
                match resp {
                    Ok(x) => break x,
                    Err(x) => {
                        if tries < 3 {
                            tries += 1;
                            sleep(Duration::from_millis(100)).await;
                        } else {
                            panic!("{}", x)
                        }
                    }
                }
            }
        };
        assert_eq!(resp.status(), StatusCode::OK);

        drop(server);
    }
}
