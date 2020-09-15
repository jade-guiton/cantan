
use std::collections::{HashMap, HashSet};
use std::env;

pub struct CommandSpec {
	name: &'static str,
	takes_file: bool,
	options: &'static [(&'static str, bool)],
}

impl CommandSpec {
	pub const fn new(name: &'static str, takes_file: bool, options: &'static [(&'static str, bool)]) -> CommandSpec {
		CommandSpec { name, takes_file, options }
	}
}

pub struct Command {
	pub name: &'static str,
	pub file: Option<String>,
	pub parameters: HashMap<&'static str, String>,
	pub options: HashSet<&'static str>
}

pub fn parse_args(commands: &[CommandSpec], mut args: env::Args) -> Result<Command, String> {
	let _hissy_path = args.next().unwrap();
	
	let cmd_name = args.next()
		.ok_or_else(|| String::from("Expected command name"))?;
	let cmd_spec = commands.iter().find(|cmd| cmd.name == cmd_name)
		.ok_or_else(|| format!("Unknown command '{}'", cmd_name))?;
	let mut cmd = Command {
		name: cmd_spec.name,
		file: None,
		parameters: HashMap::new(),
		options: HashSet::new(),
	};
	
	let mut positional = vec![];
	while let Some(part) = args.next() {
		if part.starts_with('-') {
			if let Some((opt, has_arg)) = cmd_spec.options.iter().find(|(opt,_)| *opt == part) {
				if *has_arg {
					cmd.parameters.insert(opt, args.next()
						.ok_or_else(|| format!("Option '{}' expects a parameter", opt))?);
				} else {
					cmd.options.insert(opt);
				}
			} else {
				return Err(format!("Unknown option '{}' for command '{}'", part, cmd.name));
			}
		} else {
			positional.push(part);
		}
	}
	
	let exp_positional = if cmd_spec.takes_file { 1 } else { 0 };
	if positional.len() != exp_positional {
		return Err(format!("Expected exactly {} positional arguments for command '{}'", exp_positional, cmd.name));
	}
	if cmd_spec.takes_file {
		cmd.file = Some(positional.first().unwrap().clone());
	}
	
	Ok(cmd)
}
