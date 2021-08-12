use argh::FromArgs;
use std::path::PathBuf;

#[derive(FromArgs, PartialEq, Debug)]
/// Tpyo command line interface.
pub struct Cli {
    #[argh(subcommand)]
    pub cmd: Subcommand,
}

#[derive(FromArgs, PartialEq, Debug)]
#[argh(subcommand)]
pub enum Subcommand {
    Serve(Serve),
}

#[derive(FromArgs, PartialEq, Debug)]
/// Start Tpyo web server.
#[argh(subcommand, name = "serve")]
pub struct Serve {
    #[argh(option, short = 'p', default = "39496")]
    /// port, default 39496
    pub port: u16,

    #[argh(option, short = 'C', default = "PathBuf::from(\"./tpyo.toml\")")]
    /// configuration file path, default "./tpyo.toml"
    pub config: PathBuf,
}

pub fn cmd() -> Cli {
    argh::from_env()
}
