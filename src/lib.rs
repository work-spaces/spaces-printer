use anyhow::Context;
use indicatif::ProgressStyle;
use owo_colors::{OwoColorize, Stream::Stdout};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::{
    io::{BufRead, Write},
    sync::{Arc, Mutex, mpsc},
};
use strum::Display;

mod file_term;
pub mod markdown;
mod null_term;

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Display, Default, Serialize, Deserialize,
)]
pub enum Level {
    Trace,
    Debug,
    Message,
    #[default]
    Info,
    App,
    Passthrough,
    Warning,
    Error,
    Silent,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LogHeader {
    command: Arc<str>,
    working_directory: Option<Arc<str>>,
    environment: HashMap<Arc<str>, HashMap<Arc<str>, Arc<str>>>,
    arguments: Vec<Arc<str>>,
    shell: Arc<str>,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct Verbosity {
    pub level: Level,
    pub is_show_progress_bars: bool,
    pub is_show_elapsed_time: bool,
    pub is_tty: bool,
}

const PROGRESS_PREFIX_WIDTH: usize = 0;

fn is_verbosity_active(printer_level: Verbosity, verbosity: Level) -> bool {
    verbosity >= printer_level.level
}

fn format_log(
    indent: usize,
    max_width: usize,
    verbosity: Level,
    message: &str,
    is_show_elapsed_time: bool,
    start_time: std::time::Instant,
) -> String {
    let timestamp: Arc<str> = if is_show_elapsed_time {
        let elapsed = std::time::Instant::now() - start_time;
        format!("{:.3}:", elapsed.as_secs_f64()).into()
    } else {
        "".into()
    };
    let mut result = if verbosity == Level::Passthrough {
        format!("{timestamp}{message}")
    } else {
        format!(
            "{timestamp}{}{}: {message}",
            " ".repeat(indent),
            verbosity
                .to_string()
                .if_supports_color(Stdout, |text| text.bold())
        )
    };
    while result.len() < max_width {
        result.push(' ');
    }
    result.push('\n');
    result
}

pub struct Section<'a> {
    pub printer: &'a mut Printer,
}

impl<'a> Section<'a> {
    pub fn new(printer: &'a mut Printer, name: &str) -> anyhow::Result<Self> {
        printer.write(format!("{}{}:", " ".repeat(printer.indent), name.bold()).as_str())?;
        printer.shift_right();
        Ok(Self { printer })
    }
}

impl Drop for Section<'_> {
    fn drop(&mut self) {
        self.printer.shift_left();
    }
}

pub struct MultiProgressBar {
    lock: Arc<Mutex<()>>,
    printer_verbosity: Verbosity,
    start_time: std::time::Instant,
    indent: usize,
    max_width: usize,
    progress_width: usize,
    progress: Option<indicatif::ProgressBar>,
    final_message: Option<Arc<str>>,
    is_increasing: bool,
}

impl MultiProgressBar {
    pub fn total(&self) -> Option<u64> {
        if let Some(progress) = self.progress.as_ref() {
            progress.length()
        } else {
            None
        }
    }

    pub fn reset_elapsed(&mut self) {
        if let Some(progress) = self.progress.as_mut() {
            progress.reset_elapsed();
        }
    }

    pub fn set_total(&mut self, total: u64) {
        if let Some(progress) = self.progress.as_mut() {
            if let Some(length) = progress.length() {
                if length != total {
                    let _lock = self.lock.lock().unwrap();
                    progress.set_length(total);
                    progress.set_position(0);
                }
            }
        }
    }

    pub fn log(&mut self, verbosity: Level, message: &str) {
        if is_verbosity_active(self.printer_verbosity, verbosity) {
            let formatted_message = format_log(
                self.indent,
                self.max_width,
                verbosity,
                message,
                self.printer_verbosity.is_show_elapsed_time,
                self.start_time,
            );
            let _lock = self.lock.lock().unwrap();
            if let Some(progress) = self.progress.as_ref() {
                progress.println(formatted_message.as_str());
            } else {
                print!("{formatted_message}");
            }
        }
    }

    pub fn set_prefix(&mut self, message: &str) {
        if let Some(progress) = self.progress.as_mut() {
            let _lock = self.lock.lock().unwrap();
            progress.set_prefix(message.to_owned());
        }
    }

    fn construct_message(&self, message: &str) -> String {
        let prefix_size = if let Some(progress) = self.progress.as_ref() {
            progress.prefix().len()
        } else {
            0_usize
        };
        let length = if self.max_width > self.progress_width + prefix_size {
            self.max_width - self.progress_width - prefix_size
        } else {
            0_usize
        };
        sanitize_output(message, length)
    }

    pub fn set_message(&mut self, message: &str) {
        let constructed_message = self.construct_message(message);
        if let Some(progress) = self.progress.as_mut() {
            let _lock = self.lock.lock().unwrap();
            progress.set_message(constructed_message);
        }
    }

    pub fn set_ending_message(&mut self, message: &str) {
        self.final_message = Some(self.construct_message(message).into());
    }

    pub fn increment_with_overflow(&mut self, count: u64) {
        let progress_total = self.total();
        if let Some(progress) = self.progress.as_mut() {
            let _lock = self.lock.lock().unwrap();
            if self.is_increasing {
                progress.inc(count);
                if progress.position() == progress_total.unwrap_or(100) {
                    self.is_increasing = false;
                }
            } else if progress.position() >= count {
                progress.set_position(progress.position() - count);
            } else {
                progress.set_position(0);
                self.is_increasing = true;
            }
        }
    }

    pub fn decrement(&mut self, count: u64) {
        if let Some(progress) = self.progress.as_mut() {
            let _lock = self.lock.lock().unwrap();
            if progress.position() >= count {
                progress.set_position(progress.position() - count);
            } else {
                progress.set_position(0);
            }
        }
    }

    pub fn increment(&mut self, count: u64) {
        if let Some(progress) = self.progress.as_mut() {
            let _lock = self.lock.lock().unwrap();
            progress.inc(count);
        }
    }

    fn start_process(
        &mut self,
        command: &str,
        options: &ExecuteOptions,
    ) -> anyhow::Result<std::process::Child> {
        if let Some(directory) = &options.working_directory {
            if !std::path::Path::new(directory.as_ref()).exists() {
                return Err(anyhow::anyhow!("Directory does not exist: {directory}"));
            }
        }

        let child_process = options
            .spawn(command)
            .context(format!("Failed to spawn a child process using {command}"))?;
        Ok(child_process)
    }

    pub fn execute_process(
        &mut self,
        command: &str,
        options: ExecuteOptions,
    ) -> anyhow::Result<Option<String>> {
        self.set_message(&options.get_full_command(command));
        let child_process = self
            .start_process(command, &options)
            .context(format!("Failed to start process {command}"))?;
        let result = monitor_process(command, child_process, self, &options)
            .context(format!("Command `{command}` failed to execute"))?;
        Ok(result)
    }
}

impl Drop for MultiProgressBar {
    fn drop(&mut self) {
        if let Some(message) = &self.final_message {
            let constructed_message = self.construct_message(message);
            if let Some(progress) = self.progress.as_mut() {
                let _lock = self.lock.lock().unwrap();
                progress.finish_with_message(constructed_message.bold().to_string());
            }
        }
    }
}

pub struct MultiProgress<'a> {
    pub printer: &'a mut Printer,
    multi_progress: indicatif::MultiProgress,
}

impl<'a> MultiProgress<'a> {
    pub fn new(printer: &'a mut Printer) -> Self {
        let locker = printer.lock.clone();
        let _lock = locker.lock().unwrap();

        let draw_target = indicatif::ProgressDrawTarget::term_like_with_hz(
            (printer.create_progress_printer)(),
            10,
        );

        Self {
            printer,
            multi_progress: indicatif::MultiProgress::with_draw_target(draw_target),
        }
    }

    pub fn add_progress(
        &mut self,
        prefix: &str,
        total: Option<u64>,
        finish_message: Option<&str>,
    ) -> MultiProgressBar {
        let _lock = self.printer.lock.lock().unwrap();

        let template_string = "{elapsed_precise}|{bar:.cyan/blue}|{prefix} {msg}";

        let (progress, progress_chars) = if let Some(total) = total {
            let progress = indicatif::ProgressBar::new(total);
            (progress, "#>-")
        } else {
            let progress = indicatif::ProgressBar::new(200);
            (progress, "*>-")
        };

        progress.set_style(
            ProgressStyle::with_template(template_string)
                .unwrap()
                .progress_chars(progress_chars),
        );

        let progress = if self.printer.verbosity.is_show_progress_bars {
            let progress = self.multi_progress.add(progress);
            let prefix = format!("{prefix}:");
            progress.set_prefix(
                format!("{prefix:PROGRESS_PREFIX_WIDTH$}")
                    .if_supports_color(Stdout, |text| text.bold())
                    .to_string(),
            );
            Some(progress)
        } else {
            None
        };

        MultiProgressBar {
            lock: self.printer.lock.clone(),
            printer_verbosity: self.printer.verbosity,
            indent: self.printer.indent,
            progress,
            progress_width: 28, // This is the default from indicatif?
            max_width: self.printer.max_width,
            final_message: finish_message.map(|s| s.into()),
            is_increasing: true,
            start_time: self.printer.start_time,
        }
    }
}

pub struct Heading<'a> {
    pub printer: &'a mut Printer,
}

impl<'a> Heading<'a> {
    pub fn new(printer: &'a mut Printer, name: &str) -> anyhow::Result<Self> {
        printer.newline()?;
        printer.enter_heading();
        {
            let heading = if printer.heading_count == 1 {
                format!("{} {name}", "#".repeat(printer.heading_count))
                    .yellow()
                    .bold()
                    .to_string()
            } else {
                format!("{} {name}", "#".repeat(printer.heading_count))
                    .bold()
                    .to_string()
            };
            printer.write(heading.as_str())?;
            printer.write("\n")?;
        }
        Ok(Self { printer })
    }
}

impl Drop for Heading<'_> {
    fn drop(&mut self) {
        self.printer.exit_heading();
    }
}

#[derive(Clone, Debug)]
pub struct ExecuteOptions {
    pub label: Arc<str>,
    pub is_return_stdout: bool,
    pub working_directory: Option<Arc<str>>,
    pub environment: Vec<(Arc<str>, Arc<str>)>,
    pub arguments: Vec<Arc<str>>,
    pub log_file_path: Option<Arc<str>>,
    pub clear_environment: bool,
    pub process_started_with_id: Option<fn(&str, u32)>,
    pub log_level: Option<Level>,
    pub timeout: Option<std::time::Duration>,
}

impl Default for ExecuteOptions {
    fn default() -> Self {
        Self {
            label: "working".into(),
            is_return_stdout: false,
            working_directory: None,
            environment: vec![],
            arguments: vec![],
            log_file_path: None,
            clear_environment: false,
            process_started_with_id: None,
            log_level: None,
            timeout: None,
        }
    }
}

impl ExecuteOptions {
    fn process_child_output<OutputType: std::io::Read + Send + 'static>(
        output: OutputType,
    ) -> anyhow::Result<(std::thread::JoinHandle<()>, mpsc::Receiver<String>)> {
        let (tx, rx) = mpsc::channel::<String>();

        let thread = std::thread::spawn(move || {
            use std::io::BufReader;
            let reader = BufReader::new(output);
            for line in reader.lines() {
                let line = line.unwrap();
                tx.send(line).unwrap();
            }
        });

        Ok((thread, rx))
    }

    fn spawn(&self, command: &str) -> anyhow::Result<std::process::Child> {
        use std::process::{Command, Stdio};
        let mut process = Command::new(command);

        if self.clear_environment {
            process.env_clear();
        }

        for argument in &self.arguments {
            process.arg(argument.as_ref());
        }

        if let Some(directory) = &self.working_directory {
            process.current_dir(directory.as_ref());
        }

        for (key, value) in self.environment.iter() {
            process.env(key.as_ref(), value.as_ref());
        }

        let result = process
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .stdin(Stdio::null())
            .spawn()
            .context(format!("while spawning piped {command}"))?;

        if let Some(callback) = self.process_started_with_id.as_ref() {
            callback(self.label.as_ref(), result.id());
        }

        Ok(result)
    }

    pub fn get_full_command(&self, command: &str) -> String {
        format!("{command} {}", self.arguments.join(" "))
    }

    pub fn get_full_command_in_working_directory(&self, command: &str) -> String {
        format!(
            "{} {command} {}",
            if let Some(directory) = &self.working_directory {
                directory
            } else {
                ""
            },
            self.arguments.join(" "),
        )
    }
}

trait PrinterTrait: std::io::Write + indicatif::TermLike {}
impl<W: std::io::Write + indicatif::TermLike> PrinterTrait for W {}

pub struct Printer {
    pub verbosity: Verbosity,
    lock: Arc<Mutex<()>>,
    indent: usize,
    heading_count: usize,
    max_width: usize,
    writer: Box<dyn PrinterTrait>,
    start_time: std::time::Instant,
    create_progress_printer: fn() -> Box<dyn PrinterTrait>,
}

impl Printer {
    pub fn get_log_divider() -> Arc<str> {
        "=".repeat(80).into()
    }

    pub fn get_terminal_width() -> usize {
        const ASSUMED_WIDTH: usize = 80;
        if let Some((width, _)) = terminal_size::terminal_size() {
            width.0 as usize
        } else {
            ASSUMED_WIDTH
        }
    }

    pub fn new_stdout() -> Self {
        let max_width = Self::get_terminal_width();
        Self {
            indent: 0,
            lock: Arc::new(Mutex::new(())),
            verbosity: Verbosity::default(),
            heading_count: 0,
            max_width,
            writer: Box::new(console::Term::stdout()),
            create_progress_printer: || Box::new(console::Term::stdout()),
            start_time: std::time::Instant::now(),
        }
    }

    pub fn new_file(path: &str) -> anyhow::Result<Self> {
        let file_writer = file_term::FileTerm::new(path)?;
        Ok(Self {
            indent: 0,
            lock: Arc::new(Mutex::new(())),
            verbosity: Verbosity::default(),
            heading_count: 0,
            max_width: 65535,
            writer: Box::new(file_writer),
            create_progress_printer: || Box::new(null_term::NullTerm {}),
            start_time: std::time::Instant::now(),
        })
    }

    pub fn new_null_term() -> Self {
        Self {
            indent: 0,
            lock: Arc::new(Mutex::new(())),
            verbosity: Verbosity::default(),
            heading_count: 0,
            max_width: 80,
            writer: Box::new(null_term::NullTerm {}),
            create_progress_printer: || Box::new(null_term::NullTerm {}),
            start_time: std::time::Instant::now(),
        }
    }

    pub(crate) fn write(&mut self, message: &str) -> anyhow::Result<()> {
        let _lock = self.lock.lock().unwrap();
        write!(self.writer, "{message}")?;
        Ok(())
    }

    pub fn newline(&mut self) -> anyhow::Result<()> {
        self.write("\n")?;
        Ok(())
    }

    pub fn trace<Type: Serialize>(&mut self, name: &str, value: &Type) -> anyhow::Result<()> {
        if is_verbosity_active(self.verbosity, Level::Trace) {
            self.object(name, value)
        } else {
            Ok(())
        }
    }

    pub fn debug<Type: Serialize>(&mut self, name: &str, value: &Type) -> anyhow::Result<()> {
        if is_verbosity_active(self.verbosity, Level::Debug) {
            self.object(name, value)
        } else {
            Ok(())
        }
    }

    pub fn message<Type: Serialize>(&mut self, name: &str, value: &Type) -> anyhow::Result<()> {
        if is_verbosity_active(self.verbosity, Level::Message) {
            self.object(name, value)
        } else {
            Ok(())
        }
    }

    pub fn info<Type: Serialize>(&mut self, name: &str, value: &Type) -> anyhow::Result<()> {
        if is_verbosity_active(self.verbosity, Level::Info) {
            self.object(name, value)
        } else {
            Ok(())
        }
    }

    pub fn warning<Type: Serialize>(&mut self, name: &str, value: &Type) -> anyhow::Result<()> {
        if is_verbosity_active(self.verbosity, Level::Warning) {
            self.object(name.yellow().to_string().as_str(), value)
        } else {
            Ok(())
        }
    }

    pub fn error<Type: Serialize>(&mut self, name: &str, value: &Type) -> anyhow::Result<()> {
        if is_verbosity_active(self.verbosity, Level::Error) {
            self.object(name.red().to_string().as_str(), value)
        } else {
            Ok(())
        }
    }

    pub fn log(&mut self, level: Level, message: &str) -> anyhow::Result<()> {
        if is_verbosity_active(self.verbosity, level) {
            self.write(
                format_log(
                    self.indent,
                    self.max_width,
                    level,
                    message,
                    self.verbosity.is_show_elapsed_time,
                    self.start_time,
                )
                .as_str(),
            )
        } else {
            Ok(())
        }
    }

    pub fn code_block(&mut self, name: &str, content: &str) -> anyhow::Result<()> {
        self.write(format!("```{name}\n{content}```\n").as_str())?;
        Ok(())
    }

    fn object<Type: Serialize>(&mut self, name: &str, value: &Type) -> anyhow::Result<()> {
        let value = serde_json::to_value(value).context("failed to serialize as JSON")?;

        if self.verbosity.level <= Level::Message && value == serde_json::Value::Null {
            return Ok(());
        }

        self.write(
            format!(
                "{}{}: ",
                " ".repeat(self.indent),
                name.if_supports_color(Stdout, |text| text.bold())
            )
            .as_str(),
        )?;

        self.print_value(&value)?;
        Ok(())
    }

    fn enter_heading(&mut self) {
        self.heading_count += 1;
    }

    fn exit_heading(&mut self) {
        self.heading_count -= 1;
    }

    fn shift_right(&mut self) {
        self.indent += 2;
    }

    fn shift_left(&mut self) {
        self.indent -= 2;
    }

    fn print_value(&mut self, value: &serde_json::Value) -> anyhow::Result<()> {
        match value {
            serde_json::Value::Object(map) => {
                self.write("\n")?;
                self.shift_right();
                for (key, value) in map {
                    let is_skip =
                        *value == serde_json::Value::Null && self.verbosity.level > Level::Message;
                    if !is_skip {
                        {
                            self.write(
                                format!(
                                    "{}{}: ",
                                    " ".repeat(self.indent),
                                    key.if_supports_color(Stdout, |text| text.bold())
                                )
                                .as_str(),
                            )?;
                        }
                        self.print_value(value)?;
                    }
                }
                self.shift_left();
            }
            serde_json::Value::Array(array) => {
                self.write("\n")?;
                self.shift_right();
                for (index, value) in array.iter().enumerate() {
                    self.write(format!("{}[{index}]: ", " ".repeat(self.indent)).as_str())?;
                    self.print_value(value)?;
                }
                self.shift_left();
            }
            serde_json::Value::Null => {
                self.write("null\n")?;
            }
            serde_json::Value::Bool(value) => {
                self.write(format!("{value}\n").as_str())?;
            }
            serde_json::Value::Number(value) => {
                self.write(format!("{value}\n").as_str())?;
            }
            serde_json::Value::String(value) => {
                self.write(format!("{value}\n").as_str())?;
            }
        }

        Ok(())
    }

    pub fn start_process(
        &mut self,
        command: &str,
        options: &ExecuteOptions,
    ) -> anyhow::Result<std::process::Child> {
        let args = options.arguments.join(" ");
        let full_command = format!("{command} {args}");

        self.info("execute", &full_command)?;
        if let Some(directory) = &options.working_directory {
            self.info("directory", &directory)?;
            if !std::path::Path::new(directory.as_ref()).exists() {
                return Err(anyhow::anyhow!("Directory does not exist: {directory}"));
            }
        }

        let child_process = options
            .spawn(command)
            .context(format!("while spawning {command}"))?;
        Ok(child_process)
    }

    pub fn execute_process(
        &mut self,
        command: &str,
        options: ExecuteOptions,
    ) -> anyhow::Result<Option<String>> {
        let section = Section::new(self, command)?;
        let child_process = section
            .printer
            .start_process(command, &options)
            .context(format!("Faild to execute process: {command}"))?;
        let mut multi_progress = MultiProgress::new(section.printer);
        let mut progress_bar = multi_progress.add_progress("progress", None, None);
        let result = monitor_process(command, child_process, &mut progress_bar, &options)
            .context(format!("Command `{command}` failed to execute"))?;

        Ok(result)
    }
}

fn sanitize_output(input: &str, max_length: usize) -> String {
    //remove all backspaces and truncate

    let escaped: Vec<_> = input.chars().flat_map(|c| c.escape_default()).collect();

    let mut result = String::new();
    let mut length = 0usize;
    for character in escaped.into_iter() {
        if length < max_length {
            result.push(character);
            length += 1;
        }
    }
    while result.len() < max_length {
        result.push(' ');
    }

    result
}

fn format_monitor_log_message(level: Level, source: &str, command: &str, message: &str) -> String {
    if level == Level::Passthrough {
        message.to_string()
    } else {
        format!("[{source}:{command}] {message}")
    }
}

fn monitor_process(
    command: &str,
    mut child_process: std::process::Child,
    progress_bar: &mut MultiProgressBar,
    options: &ExecuteOptions,
) -> anyhow::Result<Option<String>> {
    let start_time = std::time::Instant::now();

    let child_stdout = child_process
        .stdout
        .take()
        .ok_or(anyhow::anyhow!("Internal Error: Child has no stdout"))?;

    let child_stderr = child_process
        .stderr
        .take()
        .ok_or(anyhow::anyhow!("Internal Error: Child has no stderr"))?;

    let log_level_stdout = options.log_level;
    let log_level_stderr = options.log_level;

    let (stdout_thread, stdout_rx) = ExecuteOptions::process_child_output(child_stdout)?;
    let (stderr_thread, stderr_rx) = ExecuteOptions::process_child_output(child_stderr)?;

    let handle_stdout = |progress: &mut MultiProgressBar,
                         writer: Option<&mut std::fs::File>,
                         content: Option<&mut String>|
     -> anyhow::Result<()> {
        let mut stdout = String::new();
        while let Ok(message) = stdout_rx.try_recv() {
            if writer.is_some() || content.is_some() {
                stdout.push_str(message.as_str());
                stdout.push('\n');
            }
            progress.set_message(message.as_str());
            if let Some(level) = log_level_stdout.as_ref() {
                progress.log(
                    *level,
                    format_monitor_log_message(*level, "stdout", command, message.as_str())
                        .as_str(),
                );
            }
        }

        if let Some(content) = content {
            content.push_str(stdout.as_str());
        }

        if let Some(writer) = writer {
            let _ = writer.write_all(stdout.as_bytes());
        }
        Ok(())
    };

    let handle_stderr = |progress: &mut MultiProgressBar,
                         writer: Option<&mut std::fs::File>,
                         content: &mut String|
     -> anyhow::Result<()> {
        let mut stderr = String::new();
        while let Ok(message) = stderr_rx.try_recv() {
            stderr.push_str(message.as_str());
            stderr.push('\n');
            progress.set_message(message.as_str());
            if let Some(level) = log_level_stderr.as_ref() {
                progress.log(
                    *level,
                    format_monitor_log_message(*level, "stdout", command, message.as_str())
                        .as_str(),
                );
            }
        }
        content.push_str(stderr.as_str());

        if let Some(writer) = writer {
            let _ = writer.write_all(stderr.as_bytes());
        }
        Ok(())
    };

    let mut stderr_content = String::new();
    let mut stdout_content = String::new();

    let mut output_file = create_log_file(command, options).context("Failed to create log file")?;

    let exit_status;

    loop {
        if let Some(status) = child_process
            .try_wait()
            .context("while waiting for child process")?
        {
            exit_status = Some(status);
            break;
        }

        let stdout_content = if options.is_return_stdout {
            Some(&mut stdout_content)
        } else {
            None
        };

        handle_stdout(progress_bar, output_file.as_mut(), stdout_content)
            .context("failed to handle stdout")?;
        handle_stderr(progress_bar, output_file.as_mut(), &mut stderr_content)
            .context("failed to handle stderr")?;
        std::thread::sleep(std::time::Duration::from_millis(100));
        progress_bar.increment_with_overflow(1);

        let now = std::time::Instant::now();
        if let Some(timeout) = options.timeout {
            if now - start_time > timeout {
                child_process.kill().context("Failed to kill process")?;
            }
        }
    }

    let _ = stdout_thread.join();
    let _ = stderr_thread.join();

    {
        let stdout_content = if options.is_return_stdout {
            Some(&mut stdout_content)
        } else {
            None
        };

        handle_stdout(progress_bar, output_file.as_mut(), stdout_content)
            .context("while handling stdout")?;
    }

    handle_stderr(progress_bar, output_file.as_mut(), &mut stderr_content)
        .context("while handling stderr")?;

    if let Some(exit_status) = exit_status {
        if !exit_status.success() {
            let stderr_message = if output_file.is_some() {
                String::new()
            } else {
                format!(": {stderr_content}")
            };
            if let Some(code) = exit_status.code() {
                let exit_message = format!("Command `{command}` failed with exit code: {code}");
                return Err(anyhow::anyhow!("{exit_message}{stderr_message}"));
            } else {
                return Err(anyhow::anyhow!(
                    "Command `{command}` failed with unknown exit code{stderr_message}"
                ));
            }
        }
    }

    Ok(if options.is_return_stdout {
        Some(stdout_content)
    } else {
        None
    })
}

fn create_log_file(
    command: &str,
    options: &ExecuteOptions,
) -> anyhow::Result<Option<std::fs::File>> {
    if let Some(log_path) = options.log_file_path.as_ref() {
        let mut file = std::fs::File::create(log_path.as_ref())
            .context(format!("while creating {log_path}"))?;

        let mut environment = HashMap::new();
        const INHERITED: &str = "inherited";
        const GIVEN: &str = "given";
        environment.insert(INHERITED.into(), HashMap::new());
        environment.insert(GIVEN.into(), HashMap::new());
        let env_inherited = environment.get_mut(INHERITED).unwrap();
        if !options.clear_environment {
            for (key, value) in std::env::vars() {
                env_inherited.insert(key.into(), value.into());
            }
        }
        let env_given = environment.get_mut(GIVEN).unwrap();
        for (key, value) in options.environment.iter() {
            env_given.insert(key.clone(), value.clone());
        }

        let arguments = options.arguments.join(" ");
        let arguments_escaped: Vec<_> =
            arguments.chars().flat_map(|c| c.escape_default()).collect();
        let args = arguments_escaped.into_iter().collect::<String>();
        let shell = format!("{command} {args}").into();

        let log_header = LogHeader {
            command: command.into(),
            working_directory: options.working_directory.clone(),
            environment,
            arguments: options.arguments.clone(),
            shell,
        };

        let log_header_serialized = serde_yaml::to_string(&log_header)
            .context("Internal Error: failed to yamlize log header")?;

        let divider = Printer::get_log_divider();

        file.write(format!("{log_header_serialized}{divider}\n").as_bytes())
            .context(format!("while writing {log_path}"))?;

        Ok(Some(file))
    } else {
        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Serialize)]
    pub struct Test {
        pub name: String,
        pub age: u32,
        pub alive: bool,
        pub dead: bool,
        pub children: f64,
    }

    #[test]
    fn printer() {
        let mut printer = Printer::new_stdout();
        let mut options = ExecuteOptions::default();
        options.arguments.push("-alt".into());

        let runtime =
            tokio::runtime::Runtime::new().expect("Internal Error: Failed to create runtime");

        let (async_sender, sync_receiver) = flume::bounded(1);
        runtime.spawn(async move {
            async_sender.send_async(10).await.expect("Failed to send");
        });
        let received = sync_receiver.recv().expect("Failed to receive");

        drop(runtime);

        printer.info("Received", &received).unwrap();

        printer.execute_process("/bin/ls", options).unwrap();

        {
            let mut heading = Heading::new(&mut printer, "First").unwrap();
            {
                let section = Section::new(&mut heading.printer, "PersonWrapper").unwrap();
                section
                    .printer
                    .object(
                        "Person",
                        &Test {
                            name: "John".to_string(),
                            age: 30,
                            alive: true,
                            dead: false,
                            children: 2.5,
                        },
                    )
                    .unwrap();
            }

            let mut sub_heading = Heading::new(&mut heading.printer, "Second").unwrap();

            let mut sub_section = Section::new(&mut sub_heading.printer, "PersonWrapper").unwrap();
            sub_section.printer.object("Hello", &"World").unwrap();

            {
                let mut multi_progress = MultiProgress::new(&mut sub_section.printer);
                let mut first = multi_progress.add_progress("First", Some(10), None);
                let mut second = multi_progress.add_progress("Second", Some(50), None);
                let mut third = multi_progress.add_progress("Third", Some(100), None);

                let first_handle = std::thread::spawn(move || {
                    first.set_ending_message("Done!");
                    for index in 0..10 {
                        first.increment(1);
                        if index == 5 {
                            first.set_message("half way");
                        }
                        std::thread::sleep(std::time::Duration::from_millis(100));
                    }
                });

                let second_handle = std::thread::spawn(move || {
                    for index in 0..50 {
                        second.increment(1);
                        if index == 25 {
                            second.set_message("half way");
                        }
                        std::thread::sleep(std::time::Duration::from_millis(10));
                    }
                });

                for _ in 0..100 {
                    third.increment(1);
                    std::thread::sleep(std::time::Duration::from_millis(10));
                }

                first_handle.join().unwrap();
                second_handle.join().unwrap();
            }
        }

        {
            let runtime =
                tokio::runtime::Runtime::new().expect("Internal Error: Failed to create runtime");

            let heading = Heading::new(&mut printer, "Async").unwrap();

            let mut multi_progress = MultiProgress::new(heading.printer);

            let mut handles = Vec::new();

            let task1_progress = multi_progress.add_progress("Task1", Some(30), None);
            let task2_progress = multi_progress.add_progress("Task2", Some(30), None);
            let task1 = async move {
                let mut progress = task1_progress;
                progress.set_message("Task1a");
                for _ in 0..10 {
                    tokio::time::sleep(std::time::Duration::from_millis(100)).await;
                    progress.increment(1);
                }

                progress.set_message("Task1b");
                for _ in 0..10 {
                    tokio::time::sleep(std::time::Duration::from_millis(100)).await;
                    progress.increment(1);
                }

                progress.set_message("Task1c");
                for _ in 0..10 {
                    tokio::time::sleep(std::time::Duration::from_millis(100)).await;
                    progress.increment(1);
                }
                ()
            };
            handles.push(runtime.spawn(task1));

            let task2 = async move {
                let mut progress = task2_progress;
                progress.set_message("Task2a");
                for _ in 0..10 {
                    tokio::time::sleep(std::time::Duration::from_millis(100)).await;
                    progress.increment(1);
                }

                progress.set_message("Task2b");
                for _ in 0..10 {
                    tokio::time::sleep(std::time::Duration::from_millis(100)).await;
                    progress.increment(1);
                }

                progress.set_message("Task2c");
                for _ in 0..10 {
                    tokio::time::sleep(std::time::Duration::from_millis(100)).await;
                    progress.increment(1);
                }
                ()
            };
            handles.push(runtime.spawn(task2));

            for handle in handles {
                runtime.block_on(handle).unwrap();
            }
        }
    }
}
