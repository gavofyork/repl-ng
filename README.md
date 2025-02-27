# repl-ng

Library to help you create a [REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop) for your application.

Forked from [repl-rs](https://github.com/jacklund/repl-rs), added deeper support for custom completion, dynamic prompts and command aliases. Also removes the `Value` abstraction in favor using plain `String`s.

Basic example code:

 ```rust
use std::collections::HashMap;
use repl_rs::{Command, Error, Parameter, Result, Value};
use repl_rs::{Convert, Repl};

// Write "Hello"
fn hello<T>(args: HashMap<String, String>, _context: &mut T) -> Result<Option<String>> {
    Ok(Some(format!("Hello, {}!", args["who"])))
}

fn main() -> Result<()> {
    let mut repl = Repl::new(())
        .with_name("MyApp")
        .with_version("v0.1.0")
        .with_description("My very cool app")
        .add_command(
             Command::new("hello", hello)
                 .with_alias("h")
                 .with_parameter(Parameter::new("who").set_required(true)?)?
                 .with_help("Greetings!"),
    );
    repl.run()
}
 ```

Running the example above:

```bash
% my_app
Welcome to MyApp v0.1.0
MyApp> help
MyApp v0.1.0: My very cool app
------------------------------                              
hello - Greetings!
MyApp> help hellp
hello: Greetings!
Aliases: h
Usage:
        hello who
MyApp> hello Gav
Hello, Gav!
MyApp> h Gav
Hello, Gav!
MyApp> 
```

