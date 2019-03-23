//! Utilities for printing and everything related to "UI".

use term_painter::{Color, Style, ToStyle};

macro_rules! print_msg {
    ($kind:ident: $icon:literal => $fmt:literal $($args:tt)*) => {{
        use crate::ui::MsgKind;
        use term_painter::ToStyle;

        MsgKind::$kind.icon_style().with(|| {
            print!("[{}] ", $icon);
        });
        MsgKind::$kind.body_style().with(|| {
            println!($fmt $($args)*);
        })
    }};
}

macro_rules! progress {
    ([$fmt:literal $($args:tt)*] => $body:tt) => {{
        use std::{
            io::{stdout, Write},
            time::Instant,
        };
        use crate::ui::MsgKind;
        use term_painter::ToStyle;

        let print_body = || {
            MsgKind::Progress.body_style().with(|| {
                print!($fmt $($args)*);
                print!(" ... ");
            });
        };

        MsgKind::Progress.icon_style().with(|| print!("[…] "));
        print_body();
        let _ = stdout().flush();

        let before = Instant::now();
        let out = $body;
        let time = before.elapsed();

        print!("\r{} ", MsgKind::Progress.icon_style().paint("[✓]"));
        print_body();
        MsgKind::Progress.body_style().with(|| {
            println!("{} (in {:.2?})", MsgKind::Progress.icon_style().paint("done"), time);
        });

        out
    }};
}

macro_rules! info {
    () => { info!("") };
    ($($t:tt)*) => {
        print_msg!(Info: 'i' => $($t)*);
    };
}

macro_rules! warn {
    () => { warn!("") };
    ($($t:tt)*) => {
        print_msg!(Warning: 'w' => $($t)*);
    };
}

macro_rules! error {
    () => { error!("") };
    ($($t:tt)*) => {
        print_msg!(Error: '!' => $($t)*);
    };
}

pub enum MsgKind {
    Error,
    Warning,
    Info,
    Progress,
}

impl MsgKind {
    pub fn icon_style(&self) -> Style {
        match self {
            MsgKind::Error => Color::Red.bold(),
            MsgKind::Warning => Color::Yellow.bold(),
            MsgKind::Info => Color::Blue.bold(),
            MsgKind::Progress => Color::Green.bold(),
        }
    }

    pub fn body_style(&self) -> Style {
        match self {
            MsgKind::Error => Color::BrightRed.to_style(),
            MsgKind::Warning => Color::BrightYellow.to_style(),
            MsgKind::Info => Color::NotSet.to_style(),
            MsgKind::Progress => Color::NotSet.to_style(),
        }
    }
}

/// Formats the given integer with `,` as thousand separator.
pub fn fmt_with_thousand_sep(mut v: u64) -> String {
    let mut out = (v % 1000).to_string();
    v /= 1000;
    while v != 0 {
        out = format!("{},{}", v % 1000, out);
        v /= 1000;
    }

    out
}
