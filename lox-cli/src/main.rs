use failure::Error;
use structopt::StructOpt;

#[macro_use]
mod ui;

mod args;
mod commands;
mod util;

use crate::{
    args::{Args, Command},
};


/// We just catch potential errors here and pretty print them. The actual
/// useful code is in `run()`.
fn main() {
    if let Err(e) = run() {
        error!("An error occured: {}", e);

        for cause in e.iter_causes() {
            error!("  ... caused by: {}", cause);
        }

        if std::env::var("RUST_BACKTRACE") == Ok("1".to_string()) {
            error!();
            error!("{}", e.backtrace());
        }

        std::process::exit(1);
    }
}

fn run() -> Result<(), Error> {
    let args = Args::from_args();
    let global_args = &args.global;

    match &args.command {
        Command::Convert { args } => {
            commands::convert::run(&global_args, args)?;
        }
    }

    Ok(())
}
