use std::sync::Arc;

#[derive(Debug, Clone)]
pub struct Secrets {
    pub secrets: Vec<Arc<str>>,
    pub redacted: Arc<str>,
}

impl Secrets {
    pub fn redact(&self, text: Arc<str>) -> Arc<str> {
        if self.secrets.is_empty() {
            text
        } else {
            let mut result = text.to_string();
            for secret in &self.secrets {
                if !secret.is_empty() {
                    result = result.replace(secret.as_ref(), self.redacted.as_ref());
                }
            }
            result.into()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn redact_skips_empty_secrets() {
        let secrets = Secrets {
            secrets: vec!["".into()],
            redacted: "REDACTED".into(),
        };
        let input: Arc<str> = "hello world".into();
        let result = secrets.redact(input.clone());
        assert_eq!(result, input, "Empty secret should not alter the text");
    }

    #[test]
    fn redact_skips_empty_secrets_among_valid_ones() {
        let secrets = Secrets {
            secrets: vec!["".into(), "world".into()],
            redacted: "REDACTED".into(),
        };
        let input: Arc<str> = "hello world".into();
        let result = secrets.redact(input);
        assert_eq!(
            result.as_ref(),
            "hello REDACTED",
            "Only the non-empty secret should be redacted"
        );
    }

    #[test]
    fn redact_with_no_secrets() {
        let secrets = Secrets {
            secrets: vec![],
            redacted: "REDACTED".into(),
        };
        let input: Arc<str> = "hello world".into();
        let result = secrets.redact(input.clone());
        assert_eq!(result, input, "No secrets means text is returned unchanged");
    }

    #[test]
    fn redact_replaces_valid_secret() {
        let secrets = Secrets {
            secrets: vec!["secret_token".into()],
            redacted: "REDACTED".into(),
        };
        let input: Arc<str> = "my secret_token is here".into();
        let result = secrets.redact(input);
        assert_eq!(result.as_ref(), "my REDACTED is here");
    }

    #[test]
    fn redact_all_empty_secrets_leaves_text_unchanged() {
        let secrets = Secrets {
            secrets: vec!["".into(), "".into(), "".into()],
            redacted: "REDACTED".into(),
        };
        let input: Arc<str> = "nothing should change".into();
        let result = secrets.redact(input.clone());
        assert_eq!(
            result, input,
            "All-empty secrets list should not alter the text"
        );
    }
}
