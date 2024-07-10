use colored::Colorize;
use executor::env::Environment;
use executor::eval;
use parser::parse;
use rustyline::error::ReadlineError;
use rustyline::{Cmd, DefaultEditor, EventHandler, KeyCode, KeyEvent, Modifiers, Result};
use std::borrow::BorrowMut;
use std::env;
use std::fs;

const VERSION: &str = env!("CARGO_PKG_VERSION");

// cli options
const DUMP_AST: &str = "--dump-ast";
const HELP: &str = "--help";
const HELP_SHORT: &str = "-h";

const USAGE: &str = r#"
USAGE: sugar [filename]

    * omit the filename to open the repl

    --dump-ast: dumps the Abstract-Syntax-Tree for every evaluation
    --help, -h: print (this) USAGE string
"#;

fn get_welcome_string() -> String {
    let username = whoami::username().bold();
    let version_string = format!("SugarScript v{VERSION}").green();
    format!(
        "Hi, {username}!
You are currently using {version_string}
Type <history>, <history -clear>, <clear> or <read <filename>> for predefined REPL commands."
    )
}

fn repl(cli_args: &CliArgs) -> Result<()> {
    println!("{}", get_welcome_string());

    let mut rl = DefaultEditor::new()?;

    if rl.load_history("history.txt").is_err() {
        println!("A history.txt file was created in the current directory.");
    }

    rl.bind_sequence(
        KeyEvent(KeyCode::Char('s'), Modifiers::CTRL),
        EventHandler::Simple(Cmd::Newline),
    );

    let mut env = Environment::default();
    loop {
        let readline = rl.readline("» ");
        match readline {
            Ok(line) => {
                let _ = rl.add_history_entry(line.as_str());
                repl_predefined_commands(line, rl.borrow_mut(), &mut env, cli_args);
            }
            Err(ReadlineError::Interrupted) => {
                println!("{}", "CTRL-C".bold());
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("{}", "CTRL-D".bold());
                break;
            }
            Err(err) => {
                println!("{}: {:?}", "Error".red().bold(), err);
                break;
            }
        }
    }
    let _ = rl.save_history("history.txt");
    Ok(())
}

fn repl_predefined_commands(
    line: String,
    rl: &mut rustyline::Editor<(), rustyline::history::FileHistory>,
    env: &mut Environment,
    cli_args: &CliArgs,
) {
    let trimmed_line = line.trim();
    match trimmed_line {
        "history" => {
            for (idx, entry) in rl.history().iter().enumerate() {
                println!("{}: {}", idx + 1, entry);
            }
        }
        "clear" => {
            let _ = rl.clear_screen();
            println!("{}", get_welcome_string());
        }
        "history -clear" => {
            let _ = rl.clear_history();
            println!("{}", "Your REPL was cleaned.".bold());
        }
        _ => {
            run(trimmed_line, env, cli_args);
        }
    }
}

fn run(src_code: &str, env: &mut Environment, cli_args: &CliArgs) {
    match parse(src_code) {
        Ok(ast) => {
            if cli_args.dump_ast {
                println!("{ast:#?}");
            }
            match eval(ast, env) {
                Ok(evaluated) => {
                    print!("{} ", "=>".green().dimmed());
                    for result in evaluated {
                        println!("{}", result);
                    }
                    println!();
                }
                Err(err) => eprintln!("{}: {err}\n", "Error".red()),
            }
        }
        Err(err) => eprintln!("{}: {err}\n", "Error".red()),
    };
}

#[derive(Debug, Default)]
struct CliArgs {
    args: Vec<String>,
    help: bool,
    dump_ast: bool,
}

impl CliArgs {
    fn parse() -> CliArgs {
        let os_args = env::args();
        let mut cli_args = CliArgs::default();

        for arg in os_args {
            match arg.as_str() {
                HELP | HELP_SHORT => {
                    return CliArgs {
                        args: vec![],
                        help: true,
                        dump_ast: false,
                    }
                }
                DUMP_AST => cli_args.dump_ast = true,
                file_string => cli_args.args.push(file_string.to_owned()),
            }
        }
        cli_args
    }
}

fn main() {
    let cli_args = CliArgs::parse();

    if cli_args.help {
        println!("{USAGE}");
        return;
    }

    match &cli_args.args.len() {
        1 => {
            match repl(&cli_args) {
                Ok(_) => {}
                Err(err) => {
                    eprintln!("{err}")
                }
            };
        }
        2 => {
            let filename = &cli_args.args[1];
            match fs::read_to_string(filename) {
                Ok(contents) => {
                    let mut env = Environment::default();
                    run(&contents, &mut env, &cli_args);
                }
                Err(err) => println!("{}: {:?}", "Error reading file".red().bold(), err),
            };
        }
        _ => {
            println!("{USAGE}");
        }
    }
}
