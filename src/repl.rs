use crate::error::*;
use crate::help::{DefaultHelpViewer, HelpContext, HelpEntry, HelpViewer};
use crate::{Command, Parameter};
use rustyline::completion;
use rustyline_derive::{Helper, Highlighter, Hinter, Validator};
use std::boxed::Box;
use std::collections::BTreeMap;
use std::fmt::Display;

type ErrorHandler<Context, E> = fn(error: E, repl: &Repl<Context, E>) -> Result<()>;

fn default_error_handler<Context, E: std::fmt::Display>(
	error: E,
	_repl: &Repl<Context, E>,
) -> Result<()> {
	eprintln!("{}", error);
	Ok(())
}

pub trait Prompt {
	fn prompt(&self) -> String;
	fn complete(&self, command: &str, args: &[&str], incomplete: &str) -> Vec<String>;
}

impl Prompt for () {
	fn prompt(&self) -> String { "> ".into() }
	fn complete(&self, command: &str, args: &[&str], incomplete: &str) -> Vec<String> { vec![] }
}

/// Main REPL struct
pub struct Repl<Context, E: std::fmt::Display> {
	name: String,
	version: String,
	description: String,
	commands: BTreeMap<String, Command<Context, E>>,
	aliases: BTreeMap<String, String>,
	context: Option<Context>,
	help_context: Option<HelpContext>,
	help_viewer: Box<dyn HelpViewer>,
	error_handler: ErrorHandler<Context, E>,
	use_completion: bool,
}

impl<Context, E> Repl<Context, E>
where
	E: Display + From<Error>,
	Context: Prompt,
{
	/// Create a new Repl with the given context's initial value.
	pub fn new(context: Context) -> Self {
		let name = String::new();

		Self {
			name: name.clone(),
			version: String::new(),
			description: String::new(),
			commands: BTreeMap::new(),
			aliases: BTreeMap::new(),
			context: Some(context),
			help_context: None,
			help_viewer: Box::new(DefaultHelpViewer::new()),
			error_handler: default_error_handler,
			use_completion: false,
		}
	}

	/// Give your Repl a name. This is used in the help summary for the Repl.
	pub fn with_name(mut self, name: &str) -> Self {
		self.name = name.to_string();
		self
	}

	/// Give your Repl a version. This is used in the help summary for the Repl.
	pub fn with_version(mut self, version: &str) -> Self {
		self.version = version.to_string();

		self
	}

	/// Give your Repl a description. This is used in the help summary for the Repl.
	pub fn with_description(mut self, description: &str) -> Self {
		self.description = description.to_string();

		self
	}

	/// Pass in a custom help viewer
	pub fn with_help_viewer<V: 'static + HelpViewer>(mut self, help_viewer: V) -> Self {
		self.help_viewer = Box::new(help_viewer);

		self
	}

	/// Pass in a custom error handler. This is really only for testing - the default
	/// error handler simply prints the error to stderr and then returns
	pub fn with_error_handler(mut self, handler: ErrorHandler<Context, E>) -> Self {
		self.error_handler = handler;

		self
	}

	/// Set whether to use command completion when tab is hit. Defaults to false.
	pub fn use_completion(mut self, value: bool) -> Self {
		self.use_completion = value;

		self
	}

	/// Add a command to your REPL
	pub fn add_command(mut self, command: Command<Context, E>) -> Self {
		for i in command.aliases.iter() {
			self.aliases.insert(i.clone(), command.name.clone());
		}
		self.commands.insert(command.name.clone(), command);
		self
	}

	fn validate_arguments(
		&self,
		command: &str,
		parameters: &[Parameter],
		args: &[&str],
	) -> Result<BTreeMap<String, String>> {
		if args.len() > parameters.len() {
			return Err(Error::TooManyArguments(command.into(), parameters.len()));
		}

		let mut validated = BTreeMap::new();
		for (index, parameter) in parameters.iter().enumerate() {
			if index < args.len() {
				validated.insert(parameter.name.clone(), args[index].to_string());
			} else if parameter.required {
				return Err(Error::MissingRequiredArgument(
					command.into(),
					parameter.name.clone(),
				));
			} else if parameter.default.is_some() {
				validated.insert(
					parameter.name.clone(),
					parameter.default.clone().unwrap().to_string(),
				);
			}
		}
		Ok(validated)
	}

	fn handle_command(&mut self, command: &str, args: &[&str]) -> core::result::Result<(), E> {
		let canon = self.aliases.get(command).cloned().unwrap_or(command.into());
		match self.commands.get(&canon) {
			Some(definition) => {
				let validated = self.validate_arguments(command, &definition.parameters, args)?;
				match (definition.callback)(validated, self.context.as_mut().unwrap()) {
					Ok(Some(value)) => println!("{}", value),
					Ok(None) => (),
					Err(error) => return Err(error),
				};
			}
			None => {
				if command == "help" {
					self.show_help(args)?;
				} else {
					return Err(Error::UnknownCommand(command.to_string()).into());
				}
			}
		}

		Ok(())
	}

	fn show_help(&self, args: &[&str]) -> Result<()> {
		if args.is_empty() {
			self.help_viewer
				.help_general(self.help_context.as_ref().unwrap())?;
		} else {
			let entry_opt = self
				.help_context
				.as_ref()
				.unwrap()
				.help_entries
				.iter()
				.find(|entry| entry.command == args[0]);
			match entry_opt {
				Some(entry) => {
					self.help_viewer.help_command(entry)?;
				}
				None => eprintln!("Help not found for command '{}'", args[0]),
			};
		}
		Ok(())
	}

	fn process_line(&mut self, line: &str) -> core::result::Result<(), E> {
		let (command, args) = split_line(line.trim());
		if !command.is_empty() {
			self.handle_command(&command, &args.iter().map(|s| s.as_ref()).collect::<Vec<_>>())?;
		}
		Ok(())
	}

	fn construct_help_context(&mut self) {
		let mut help_entries = self
			.commands
			.values()
			.map(|definition| {
				HelpEntry::new(
					&definition.name,
					&definition.aliases,
					&definition.parameters,
					&definition.help_summary,
				)
			})
			.collect::<Vec<HelpEntry>>();
		help_entries.sort_by_key(|d| d.command.clone());
		self.help_context = Some(HelpContext::new(
			&self.name,
			&self.version,
			&self.description,
			help_entries,
		));
	}

	pub fn run(&mut self) -> Result<()> {
		self.construct_help_context();
		let mut editor: rustyline::Editor<Helper<Context>> = rustyline::Editor::new();
		editor.set_helper(Some(Helper {
			canon: self.commands.keys()
				.map(|x| (x, x))
				.chain(self.aliases.iter())
				.map(|(x, y)| (x.clone(), y.clone()))
				.collect(),
			context: None,
		}));
		// TODO: Read history from a file if history configured.
		// editor.add_history_entry(line.clone());
		println!("Welcome to {} {}", self.name, self.version);
		let mut eof = false;
		while !eof {
			self.handle_line(&mut editor, &mut eof)?;
		}

		Ok(())
	}

//    /// Load the history from the specified file.
//    pub fn load_history<P: AsRef<Path> + ?Sized>(&mut self, path: &P) -> Result<()>;
//    /// Save the history in the specified file.
//    pub fn save_history<P: AsRef<Path> + ?Sized>(&mut self, path: &P) -> Result<()>;
//    /// Append new entries in the specified file.
//    pub fn append_history<P: AsRef<Path> + ?Sized>(&mut self, path: &P) -> Result<()>;

	fn handle_line(
		&mut self,
		editor: &mut rustyline::Editor<Helper<Context>>,
		eof: &mut bool,
	) -> Result<()> {
		let prompt = format!("{}", self.context.as_ref().unwrap().prompt());
		editor.helper_mut().unwrap().context = self.context.take();
		let r = editor.readline(&prompt);
		self.context = editor.helper_mut().unwrap().context.take();
		match r {
			Ok(line) => {
				editor.add_history_entry(line.clone());
				// TODO: Add to a file if history configured.
				if let Err(error) = self.process_line(&line) {
					(self.error_handler)(error, self)?;
				}
				*eof = false;
				Ok(())
			}
			Err(rustyline::error::ReadlineError::Eof) => {
				*eof = true;
				Ok(())
			}
			Err(error) => {
				eprintln!("Error reading line: {}", error);
				*eof = false;
				Ok(())
			}
		}
	}
}

fn split_line(line: &str) -> (String, Vec<String>) {
	let trimmed = line.trim();
	if trimmed.is_empty() {
		Default::default()
	} else {
		let r = regex::Regex::new(r#"("[^"\n]+"|[\S]+)"#).unwrap();
		let mut args = r
			.captures_iter(trimmed)
			.map(|a| a[0].to_string().replace('\"', ""))
			.collect::<Vec<String>>();
		let command = args.remove(0);
		if line.ends_with(' ') {
			args.push(Default::default());
		}
		(command, args)
	}
}

fn quote_if_needed(s: String) -> String {
	if s.contains(' ') {
		format!("\"{s}\"")
	} else {
		s
	}
}

// rustyline Helper struct
// Currently just does command completion with <tab>, if
// use_completion() is set on the REPL
#[derive(Clone, Helper, Hinter, Highlighter, Validator)]
struct Helper<Context: Prompt> {
	canon: BTreeMap<String, String>,
	context: Option<Context>,
}

impl<Context: Prompt> completion::Completer for Helper<Context> {
	type Candidate = String;

	fn complete(
		&self,
		line: &str,
		_pos: usize,
		_ctx: &rustyline::Context<'_>,
	) -> rustyline::Result<(usize, Vec<Self::Candidate>)> {
		// Complete based on whether the current line is a substring
		// of one of the set commands
		let (command, mut args) = split_line(line);
//		println!("split: >{line}< {command} {args:?}");
		if let Some(last_arg) = args.pop() {
			let Some(context) = self.context.as_ref() else { return Ok((0, Vec::new())) };
			let first_args = args.iter().map(|s| s.as_ref()).collect::<Vec<_>>();
			let command = self.canon.get(&command).cloned().unwrap_or(command);
			let last_cands = context.complete(&command, &first_args, &last_arg);
//			println!("complete: {} {:?} >{}< {:?}", command, first_args, last_arg, last_cands);
			let args_blob = args.iter().cloned().map(quote_if_needed).collect::<Vec<_>>().join(" ");
			let prefix = format!("{command}{}{args_blob}", if args_blob.is_empty() { "" } else { " " });
			let completions = last_cands.into_iter().map(|c| format!("{prefix} {c}")).collect();
			Ok((0, completions))
		} else {
			let ret: Vec<Self::Candidate> = self
				.canon
				.keys()
				.filter(|cmd| cmd.contains(&command))
				.map(|s| s.to_string())
				.collect();
			Ok((0, ret))
		}
	}
}

#[cfg(all(test, unix))]
mod tests {
	use crate::error::*;
	use crate::repl::{Helper, Repl};
	use crate::initialize_repl;
	use crate::{Command, Parameter};
	use clap::{crate_description, crate_name, crate_version};
	use nix::sys::wait::{waitpid, WaitStatus};
	use nix::unistd::{close, dup2, fork, pipe, ForkResult};
	use std::collections::BTreeMap;
	use std::fs::File;
	use std::io::Write;
	use std::os::unix::io::FromRawFd;
	use super::Prompt;

	fn test_error_handler<Context>(error: Error, _repl: &Repl<Context, Error>) -> Result<()> {
		Err(error)
	}

	fn foo<T>(args: BTreeMap<String, String>, _context: &mut T) -> Result<Option<String>> {
		Ok(Some(format!("foo {:?}", args)))
	}

	fn run_repl<Context: Prompt>(mut repl: Repl<Context, Error>, input: &str, expected: Result<()>) {
		let (rdr, wrtr) = pipe().unwrap();
		unsafe {
			match fork() {
				Ok(ForkResult::Parent { child, .. }) => {
					// Parent
					let mut f = File::from_raw_fd(wrtr);
					write!(f, "{}", input).unwrap();
					if let WaitStatus::Exited(_, exit_code) = waitpid(child, None).unwrap() {
						assert!(exit_code == 0);
					};
				}
				Ok(ForkResult::Child) => {
					std::panic::set_hook(Box::new(|panic_info| {
						println!("Caught panic: {:?}", panic_info);
						if let Some(location) = panic_info.location() {
							println!(
								"panic occurred in file '{}' at line {}",
								location.file(),
								location.line(),
							);
						} else {
							println!("panic occurred but can't get location information...");
						}
					}));

					dup2(rdr, 0).unwrap();
					close(rdr).unwrap();
					let mut editor: rustyline::Editor<Helper<Context>> = rustyline::Editor::new();
					let mut eof = false;
					let result = repl.handle_line(&mut editor, &mut eof);
					let _ = std::panic::take_hook();
					if expected == result {
						std::process::exit(0);
					} else {
						eprintln!("Expected {:?}, got {:?}", expected, result);
						std::process::exit(1);
					}
				}
				Err(_) => println!("Fork failed"),
			}
		}
	}

	#[test]
	fn test_initialize_sets_crate_values() -> Result<()> {
		let repl: Repl<(), Error> = initialize_repl!(());

		assert_eq!(crate_name!(), repl.name);
		assert_eq!(crate_version!(), repl.version);
		assert_eq!(crate_description!(), repl.description);

		Ok(())
	}

	#[test]
	fn test_empty_line_does_nothing() -> Result<()> {
		let repl = Repl::new(())
			.with_name("test")
			.with_version("v0.1.0")
			.with_description("Testing 1, 2, 3...")
			.with_error_handler(test_error_handler)
			.add_command(
				Command::new("foo", foo)
					.with_parameter(Parameter::new("bar").set_required(true)?)?
					.with_parameter(Parameter::new("baz").set_required(true)?)?
					.with_help("Do foo when you can"),
			);
		run_repl(repl, "\n", Ok(()));

		Ok(())
	}

	#[test]
	fn test_missing_required_arg_fails() -> Result<()> {
		let repl = Repl::new(())
			.with_name("test")
			.with_version("v0.1.0")
			.with_description("Testing 1, 2, 3...")
			.with_error_handler(test_error_handler)
			.add_command(
				Command::new("foo", foo)
					.with_parameter(Parameter::new("bar").set_required(true)?)?
					.with_parameter(Parameter::new("baz").set_required(true)?)?
					.with_help("Do foo when you can"),
			);
		run_repl(
			repl,
			"foo bar\n",
			Err(Error::MissingRequiredArgument("foo".into(), "baz".into())),
		);

		Ok(())
	}

	#[test]
	fn test_unknown_command_fails() -> Result<()> {
		let repl = Repl::new(())
			.with_name("test")
			.with_version("v0.1.0")
			.with_description("Testing 1, 2, 3...")
			.with_error_handler(test_error_handler)
			.add_command(
				Command::new("foo", foo)
					.with_parameter(Parameter::new("bar").set_required(true)?)?
					.with_parameter(Parameter::new("baz").set_required(true)?)?
					.with_help("Do foo when you can"),
			);
		run_repl(
			repl,
			"bar baz\n",
			Err(Error::UnknownCommand("bar".to_string())),
		);

		Ok(())
	}

	#[test]
	fn test_no_required_after_optional() -> Result<()> {
		assert_eq!(
			Err(Error::IllegalRequiredError("bar".into())),
			Command::<(), Error>::new("foo", foo)
				.with_parameter(Parameter::new("baz").set_default("20")?)?
				.with_parameter(Parameter::new("bar").set_required(true)?)
		);

		Ok(())
	}

	#[test]
	fn test_required_cannot_be_defaulted() -> Result<()> {
		assert_eq!(
			Err(Error::IllegalDefaultError("bar".into())),
			Parameter::new("bar").set_required(true)?.set_default("foo")
		);

		Ok(())
	}

	#[test]
	fn test_string_with_spaces_for_argument() -> Result<()> {
		let repl = Repl::new(())
			.with_name("test")
			.with_version("v0.1.0")
			.with_description("Testing 1, 2, 3...")
			.with_error_handler(test_error_handler)
			.add_command(
				Command::new("foo", foo)
					.with_parameter(Parameter::new("bar").set_required(true)?)?
					.with_parameter(Parameter::new("baz").set_required(true)?)?
					.with_help("Do foo when you can"),
			);
		run_repl(repl, "foo \"baz test 123\" foo\n", Ok(()));

		Ok(())
	}

	#[test]
	fn test_string_with_spaces_for_argument_last() -> Result<()> {
		let repl = Repl::new(())
			.with_name("test")
			.with_version("v0.1.0")
			.with_description("Testing 1, 2, 3...")
			.with_error_handler(test_error_handler)
			.add_command(
				Command::new("foo", foo)
					.with_parameter(Parameter::new("bar").set_required(true)?)?
					.with_parameter(Parameter::new("baz").set_required(true)?)?
					.with_help("Do foo when you can"),
			);
		run_repl(repl, "foo foo \"baz test 123\"\n", Ok(()));

		Ok(())
	}
}
