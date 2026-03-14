use anyhow::Context;
use owo_colors::{OwoColorize, Stream::Stdout};
use std::collections::HashMap;
use std::io::Write;
use std::sync::Arc;

use super::{ExecuteOptions, Level, LogHeader, MultiProgressBar, Printer, Secrets, Verbosity};

pub(crate) fn is_verbosity_active(printer_level: Verbosity, verbosity: Level) -> bool {
    verbosity >= printer_level.level
}

pub(crate) fn format_log(
    indent: usize,
    max_width: usize,
    verbosity: Level,
    message: &str,
    is_show_elapsed_time: bool,
    start_time: std::time::Instant,
) -> String {
    let timestamp: Arc<str> = if is_show_elapsed_time {
        let elapsed = std::time::Instant::now() - start_time;
        format!("[{:.3}] ", elapsed.as_secs_f64()).into()
    } else {
        "".into()
    };
    let mut result = if verbosity == Level::Passthrough {
        format!("{timestamp}{message}")
    } else {
        let message_level_string = verbosity.to_string().to_lowercase();
        let message_level = message_level_string.if_supports_color(Stdout, |text| text.bold());
        format!(
            "::{message_level}::{timestamp}{}{message}",
            " ".repeat(indent),
        )
    };
    while result.len() < max_width {
        result.push(' ');
    }
    result.push('\n');
    result
}

pub(crate) fn sanitize_output(input: &str, max_length: usize) -> String {
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

pub(crate) fn format_monitor_log_message(
    level: Level,
    source: &str,
    command: &str,
    message: &str,
) -> String {
    if level == Level::Passthrough {
        message.to_string()
    } else {
        format!("[{source}:{command}] {message}")
    }
}

pub(crate) fn monitor_process(
    command: &str,
    mut child_process: std::process::Child,
    progress_bar: &mut MultiProgressBar,
    options: &ExecuteOptions,
    secrets: &Secrets,
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
            let redacted = secrets.redact(message.into());
            if writer.is_some() || content.is_some() {
                stdout.push_str(redacted.as_ref());
                stdout.push('\n');
            }
            progress.set_message(redacted.as_ref());
            if let Some(level) = log_level_stdout.as_ref() {
                progress.log(
                    *level,
                    format_monitor_log_message(*level, "stdout", command, redacted.as_ref())
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
            let redacted = secrets.redact(message.into());
            stderr.push_str(redacted.as_ref());
            stderr.push('\n');
            progress.set_message(redacted.as_ref());
            if let Some(level) = log_level_stderr.as_ref() {
                progress.log(
                    *level,
                    format_monitor_log_message(*level, "stdout", command, redacted.as_ref())
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

    let mut output_file =
        create_log_file(command, options, secrets).context("Failed to create log file")?;

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
        if let Some(timeout) = options.timeout
            && now - start_time > timeout
        {
            child_process.kill().context("Failed to kill process")?;
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

    if let Some(exit_status) = exit_status
        && !exit_status.success()
    {
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

    Ok(if options.is_return_stdout {
        Some(stdout_content)
    } else {
        None
    })
}

pub(crate) fn create_log_file(
    command: &str,
    options: &ExecuteOptions,
    secrets: &Secrets,
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
                let redacted = secrets.redact(value.into());
                env_inherited.insert(key.into(), redacted);
            }
        }
        let env_given = environment.get_mut(GIVEN).unwrap();
        for (key, value) in options.environment.iter() {
            let redacted = secrets.redact(value.clone());
            env_given.insert(key.clone(), redacted);
        }

        let arguments = options.arguments.join(" ");
        let arguments_escaped: Vec<_> =
            arguments.chars().flat_map(|c| c.escape_default()).collect();
        let args = arguments_escaped.into_iter().collect::<String>();
        let shell = format!("{command} {args}").into();

        let redacted_arguments = options
            .arguments
            .iter()
            .map(|arg| secrets.redact(arg.clone()))
            .collect();

        let log_header = LogHeader {
            command: command.into(),
            working_directory: options.working_directory.clone(),
            environment,
            arguments: redacted_arguments,
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
