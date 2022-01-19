use argh::FromArgs;
use std::path::PathBuf;

#[derive(FromArgs, PartialEq, Debug)]
/// wmd command line interface.
pub struct Cli {
    #[argh(subcommand)]
    pub cmd: Option<Subcommand>,

    /// print version
    #[argh(switch)]
    pub version: bool,
}

#[derive(FromArgs, PartialEq, Debug)]
#[argh(subcommand)]
pub enum Subcommand {
    Serve(Serve),
}

#[derive(FromArgs, PartialEq, Debug)]
/// Start wmd web server.
#[argh(subcommand, name = "serve")]
pub struct Serve {
    #[argh(option, short = 'p', default = "8000")]
    /// port, default 8000
    pub port: u16,

    #[argh(option, short = 'c')]
    /// configuration file path
    pub config: PathBuf,
}

pub fn cmd() -> Cli {
    argh::from_env()
}
