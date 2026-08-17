//! Case conversion for Fortran keywords, procedures, operators, and constants
//!
//! Implements case formatting for the --case option:
//! - 0: no change
//! - 1: lowercase
//! - 2: uppercase

use std::collections::BTreeMap;
use std::sync::LazyLock;

use regex::Regex;

use crate::parser::char_filter::CharFilter;

/// Fortran keywords
static F90_KEYWORDS_RE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(&format!(
        r"(?i)\b({})\b",
        [
            "allocatable",
            "allocate",
            "assign",
            "assignment",
            "backspace",
            "block",
            "call",
            "case",
            "character",
            "close",
            "common",
            "complex",
            "contains",
            "continue",
            "cycle",
            "data",
            "deallocate",
            "dimension",
            "do",
            "double",
            "else",
            "elseif",
            "elsewhere",
            "end",
            "enddo",
            "endfile",
            "endif",
            "entry",
            "equivalence",
            "exit",
            "external",
            "forall",
            "format",
            "function",
            "goto",
            "if",
            "implicit",
            "include",
            "inquire",
            "integer",
            "intent",
            "interface",
            "intrinsic",
            "logical",
            "module",
            "namelist",
            "none",
            "nullify",
            "only",
            "open",
            "operator",
            "optional",
            "parameter",
            "pause",
            "pointer",
            "precision",
            "print",
            "private",
            "procedure",
            "program",
            "public",
            "read",
            "real",
            "recursive",
            "result",
            "return",
            "rewind",
            "save",
            "select",
            "sequence",
            "stop",
            "subroutine",
            "target",
            "then",
            "type",
            "use",
            "where",
            "while",
            "write",
            // F95
            "elemental",
            "pure",
            // F2003
            "abstract",
            "associate",
            "asynchronous",
            "bind",
            "class",
            "deferred",
            "enum",
            "enumerator",
            "extends",
            "extends_type_of",
            "final",
            "generic",
            "import",
            "non_intrinsic",
            "non_overridable",
            "nopass",
            "pass",
            "protected",
            "same_type_as",
            "value",
            "volatile",
            // F2008
            "contiguous",
            "submodule",
            "concurrent",
            "codimension",
            "critical",
            "image_index",
            "error",
            "sync",
            "lock",
            "unlock",
            "impure",
            "mold",
            "source",
            "newunit",
            // F2018
            "rank",
            "team",
            "change",
            "form",
            "event",
            "post",
            "local",
            "local_init",
            "shared",
            "non_recursive",
            "stat",
            "errmsg",
            // F2023
            "simple",
            "typeof",
            "classof",
            "enumeration",
            "notify",
            "reduce",
            "leading_zero"
        ]
        .join("|")
    ))
    .unwrap()
});

/// Fortran intrinsic procedures (functions that take parentheses)
static F90_PROCEDURES_RE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(&format!(
        r"(?i)\b({})\b",
        [
            "abs",
            "achar",
            "acos",
            "adjustl",
            "adjustr",
            "aimag",
            "aint",
            "all",
            "allocated",
            "anint",
            "any",
            "asin",
            "associated",
            "atan",
            "atan2",
            "bit_size",
            "btest",
            "ceiling",
            "char",
            "cmplx",
            "conjg",
            "cos",
            "cosh",
            "count",
            "cshift",
            "date_and_time",
            "dble",
            "digits",
            "dim",
            "dot_product",
            "dprod",
            "eoshift",
            "epsilon",
            "exp",
            "exponent",
            "floor",
            "fraction",
            "huge",
            "iachar",
            "iand",
            "ibclr",
            "ibits",
            "ibset",
            "ichar",
            "ieor",
            "index",
            "int",
            "ior",
            "ishft",
            "ishftc",
            "kind",
            "lbound",
            "len",
            "len_trim",
            "lge",
            "lgt",
            "lle",
            "llt",
            "log",
            "log10",
            "matmul",
            "max",
            "maxexponent",
            "maxloc",
            "maxval",
            "merge",
            "min",
            "minexponent",
            "minloc",
            "minval",
            "mod",
            "modulo",
            "mvbits",
            "nearest",
            "nint",
            "not",
            "pack",
            "present",
            "product",
            "radix",
            "random_number",
            "random_seed",
            "range",
            "repeat",
            "reshape",
            "rrspacing",
            "scale",
            "scan",
            "selected_int_kind",
            "selected_real_kind",
            "set_exponent",
            "shape",
            "sign",
            "sin",
            "sinh",
            "size",
            "spacing",
            "spread",
            "sqrt",
            "sum",
            "system_clock",
            "tan",
            "tanh",
            "tiny",
            "transfer",
            "transpose",
            "trim",
            "ubound",
            "unpack",
            "verify",
            // F95
            "null",
            "cpu_time",
            // F2003
            "move_alloc",
            "command_argument_count",
            "get_command",
            "get_command_argument",
            "get_environment_variable",
            "selected_char_kind",
            "wait",
            "flush",
            "new_line",
            "c_loc",
            "c_funloc",
            "c_associated",
            "c_f_pointer",
            // F2008
            "bge",
            "bgt",
            "ble",
            "blt",
            "dshiftl",
            "dshiftr",
            "leadz",
            "popcnt",
            "poppar",
            "trailz",
            "maskl",
            "maskr",
            "shifta",
            "shiftl",
            "shiftr",
            "merge_bits",
            "iall",
            "iany",
            "iparity",
            "storage_size",
            "bessel_j0",
            "bessel_j1",
            "bessel_jn",
            "bessel_y0",
            "bessel_y1",
            "bessel_yn",
            "erf",
            "erfc",
            "erfc_scaled",
            "gamma",
            "hypot",
            "log_gamma",
            "norm2",
            "parity",
            "findloc",
            "is_contiguous",
            "num_images",
            "this_image",
            "compiler_options",
            "compiler_version",
            "c_sizeof",
            // F2018
            "out_of_range",
            "random_init",
            "reduce",
            "coshape",
            "image_status",
            "failed_images",
            "stopped_images",
            "get_team",
            "team_number",
            "co_sum",
            "co_min",
            "co_max",
            "co_reduce",
            "co_broadcast",
            // F2023
            "acosd",
            "asind",
            "atand",
            "atan2d",
            "cosd",
            "sind",
            "tand",
            "acospi",
            "asinpi",
            "atanpi",
            "atan2pi",
            "cospi",
            "sinpi",
            "tanpi",
            "selected_logical_kind",
            "split",
            "tokenize",
            "c_f_strpointer",
            "f_c_string",
            "ieee_max",
            "ieee_max_mag",
            "ieee_max_num",
            "ieee_max_num_mag",
            "ieee_min",
            "ieee_min_mag",
            "ieee_min_num",
            "ieee_min_num_mag"
        ]
        .join("|")
    ))
    .unwrap()
});

/// Fortran module names
static F90_MODULES_RE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(&format!(
        r"(?i)\b({})\b",
        [
            "iso_fortran_env",
            "iso_c_binding",
            "ieee_exceptions",
            "ieee_arithmetic",
            "ieee_features"
        ]
        .join("|")
    ))
    .unwrap()
});

/// Fortran operators (dotted form)
static F90_OPERATORS_RE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"(?i)\.(and|eq|eqv|false|ge|gt|le|lt|ne|neqv|not|or|true)\.").unwrap()
});

/// Fortran constants
static F90_CONSTANTS_RE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(&format!(
        r"(?i)\b({})\b",
        [
            // iso_fortran_env constants
            "input_unit",
            "output_unit",
            "error_unit",
            "iostat_end",
            "iostat_eor",
            "numeric_storage_size",
            "character_storage_size",
            "file_storage_size",
            "stat_stopped_image",
            "int8",
            "int16",
            "int32",
            "int64",
            "real16",
            "real32",
            "real64",
            "real128",
            "logical8",
            "logical16",
            "logical32",
            "logical64",
            "event_type",
            "lock_type",
            "notify_type",
            "team_type",
            // iso_c_binding constants
            "c_int",
            "c_short",
            "c_long",
            "c_long_long",
            "c_signed_char",
            "c_size_t",
            "c_int8_t",
            "c_int16_t",
            "c_int32_t",
            "c_int64_t",
            "c_int_least8_t",
            "c_int_least16_t",
            "c_int_least32_t",
            "c_int_least64_t",
            "c_int_fast8_t",
            "c_int_fast16_t",
            "c_int_fast32_t",
            "c_int_fast64_t",
            "c_intmax_t",
            "c_intptr_t",
            "c_float",
            "c_double",
            "c_long_double",
            "c_float_complex",
            "c_double_complex",
            "c_long_double_complex",
            "c_bool",
            "c_char",
            "c_null_char",
            "c_alert",
            "c_backspace",
            "c_form_feed",
            "c_new_line",
            "c_carriage_return",
            "c_horizontal_tab",
            "c_vertical_tab",
            "c_ptr",
            "c_funptr",
            "c_null_ptr",
            "c_null_funptr"
        ]
        .join("|")
    ))
    .unwrap()
});

/// Pattern to detect string openings
static STR_OPEN_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r#"^["']"#).unwrap());

/// Pattern to detect numeric literals with kind suffix (e.g., `2_int64`, `2.0_real64`)
/// Matches: digits or decimal point, followed by underscore, followed by identifier
static NUMERIC_KIND_SUFFIX_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^([\d.]+)_(\w+)$").unwrap());

/// Pattern to detect numeric literals with exponents (e.g., 2.0e3, 2.0d-5)
/// Matches: number with e/E/d/D exponent marker
static NUMERIC_EXPONENT_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^([\d.]*[dDeE])([+-]?\d+)$").unwrap());

/// Pattern to detect numeric literals with both exponent and kind suffix (e.g., `2.e3_real64`)
static NUMERIC_EXP_KIND_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^([\d.]*[dDeE])([+-]?\d+)_(\w+)$").unwrap());

/// How many of [`CASE_KEYS`] the `--case` flag takes. The last category,
/// `types`, has its own `--case-types` flag: a fifth value on `--case` would
/// be read as a file name.
pub(crate) const CASE_FLAG_VALUES: usize = CASE_KEYS.len() - 1;

/// The case categories, in the order `--case` and `! fprettier: --case` take
/// them. Also the accepted keys of the `[case_dict]` config table.
pub const CASE_KEYS: [&str; 5] = ["keywords", "procedures", "operators", "constants", "types"];

/// Case conversion mode
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CaseMode {
    NoChange = 0,
    Lower = 1,
    Upper = 2,
}

impl TryFrom<u8> for CaseMode {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(CaseMode::NoChange),
            1 => Ok(CaseMode::Lower),
            2 => Ok(CaseMode::Upper),
            _ => Err(()),
        }
    }
}

/// Deserialize from the documented integer encoding (0=none, 1=lower, 2=upper),
/// rejecting anything else at the parse boundary.
impl<'de> serde::Deserialize<'de> for CaseMode {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = u8::deserialize(deserializer)?;
        CaseMode::try_from(value).map_err(|()| {
            serde::de::Error::custom(format!(
                "invalid case mode {value}, expected 0 (none), 1 (lower), or 2 (upper)"
            ))
        })
    }
}

/// Case conversion settings for different token types
#[derive(Debug, Clone)]
pub struct CaseSettings {
    pub keywords: CaseMode,
    pub procedures: CaseMode,
    pub operators: CaseMode,
    pub constants: CaseMode,
    pub types: CaseMode,
}

impl Default for CaseSettings {
    fn default() -> Self {
        Self {
            keywords: CaseMode::NoChange,
            procedures: CaseMode::NoChange,
            operators: CaseMode::NoChange,
            constants: CaseMode::NoChange,
            types: CaseMode::NoChange,
        }
    }
}

impl CaseSettings {
    /// Create from a `BTreeMap` (like `Config.case_dict`)
    #[must_use]
    pub fn from_dict(dict: &BTreeMap<String, CaseMode>) -> Self {
        let get = |key| dict.get(key).copied().unwrap_or(CaseMode::NoChange);
        Self {
            keywords: get("keywords"),
            procedures: get("procedures"),
            operators: get("operators"),
            constants: get("constants"),
            types: get("types"),
        }
    }

    /// Check if any case conversion is enabled
    #[must_use]
    pub fn is_enabled(&self) -> bool {
        self.keywords != CaseMode::NoChange
            || self.procedures != CaseMode::NoChange
            || self.operators != CaseMode::NoChange
            || self.constants != CaseMode::NoChange
            || self.types != CaseMode::NoChange
    }
}

/// Apply case conversion based on mode
fn apply_case(s: &str, mode: CaseMode) -> String {
    match mode {
        CaseMode::NoChange => s.to_string(),
        CaseMode::Lower => s.to_lowercase(),
        CaseMode::Upper => s.to_uppercase(),
    }
}

/// Convert numeric literals with type suffixes and/or exponents
///
/// Handles patterns like:
/// - `2_int64` → `2_INT64` (kind suffix)
/// - `2.0e3` → `2.0E3` (exponent)
/// - `2.e3_real64` → `2.E3_REAL64` (both exponent and kind suffix)
fn convert_numeric_literal(token: &str, mode: CaseMode) -> Option<String> {
    if mode == CaseMode::NoChange {
        return None;
    }

    // Check for exponent + kind suffix (e.g., 2.e3_real64)
    if let Some(caps) = NUMERIC_EXP_KIND_RE.captures(token) {
        let num_exp = &caps[1]; // e.g., "2.e" or "2.0E"
        let exp_val = &caps[2]; // e.g., "3" or "-5"
        let suffix = &caps[3]; // e.g., "real64"

        // Convert exponent letter and suffix (num_exp is guaranteed non-empty by regex)
        let last_char = num_exp.chars().last().unwrap_or('e');
        let num_part = &num_exp[..num_exp.len() - 1];
        let exp_char = apply_case(&last_char.to_string(), mode);
        let converted_suffix = apply_case(suffix, mode);

        return Some(format!("{num_part}{exp_char}{exp_val}_{converted_suffix}"));
    }

    // Check for kind suffix only (e.g., 2_int64, 2.0_real64)
    if let Some(caps) = NUMERIC_KIND_SUFFIX_RE.captures(token) {
        let num_part = &caps[1]; // e.g., "2" or "2.0"
        let suffix = &caps[2]; // e.g., "int64"

        // Only convert if it looks like a numeric literal (starts with digit or dot)
        if num_part
            .chars()
            .next()
            .is_some_and(|c| c.is_ascii_digit() || c == '.')
        {
            let converted_suffix = apply_case(suffix, mode);
            return Some(format!("{num_part}_{converted_suffix}"));
        }
    }

    // Check for exponent only (e.g., 2.0e3, 2.0d-5)
    if let Some(caps) = NUMERIC_EXPONENT_RE.captures(token) {
        let num_exp = &caps[1]; // e.g., "2.0e" or "2.d"
        let exp_val = &caps[2]; // e.g., "3" or "-5"

        // Only convert if it's actually a numeric literal
        let first_char = num_exp.chars().next();
        if first_char.is_some_and(|c| c.is_ascii_digit() || c == '.') {
            // num_exp is guaranteed non-empty by regex
            let last_char = num_exp.chars().last().unwrap_or('e');
            let num_part = &num_exp[..num_exp.len() - 1];
            let exp_char = apply_case(&last_char.to_string(), mode);

            return Some(format!("{num_part}{exp_char}{exp_val}"));
        }
    }

    None
}

/// Convert case of keywords in a Fortran line
///
/// Splits the line respecting strings and comments, identifies token types,
/// and applies appropriate case conversion.
#[must_use]
pub fn convert_case(line: &str, settings: &CaseSettings) -> String {
    if !settings.is_enabled() {
        return line.to_string();
    }

    // Collect parts of the line, separating strings from code
    let mut parts: Vec<String> = Vec::new();
    let mut current_part = String::new();
    // Byte position just past the last character CharFilter yielded. Computed
    // from the character's own width, so multi-byte characters do not make the
    // skipped-content slices land inside a character.
    let mut next_byte: Option<usize> = None;

    // Use CharFilter to iterate through non-string, non-comment positions
    for (pos, c) in CharFilter::new(line, false, true, true) {
        // If we skipped positions, there was a string
        if let Some(prev_end) = next_byte {
            if pos > prev_end {
                // Save current part
                if !current_part.is_empty() {
                    parts.push(current_part);
                    current_part = String::new();
                }
                // Add the skipped string part unchanged
                parts.push(line[prev_end..pos].to_string());
            }
        } else if pos > 0 {
            // String at the beginning
            parts.push(line[..pos].to_string());
        }

        current_part.push(c);
        next_byte = Some(pos + c.len_utf8());
    }

    // Handle remaining content
    if !current_part.is_empty() {
        parts.push(current_part);
    }
    if let Some(prev_end) = next_byte {
        if prev_end < line.len() {
            // Remaining string/comment at the end
            parts.push(line[prev_end..].to_string());
        }
    } else if !line.is_empty() {
        // Entire line was skipped (string or comment)
        return line.to_string();
    }

    // Process each part
    let result: String = parts
        .into_iter()
        .map(|part| {
            // Skip string parts
            if STR_OPEN_RE.is_match(&part) {
                return part;
            }

            // Split by word boundaries while preserving separators
            let tokens = split_preserving_separators(&part);

            // Process tokens with look-ahead for procedures and kind=value context
            let mut result_part = String::new();
            let mut after_kind_equals = false; // Track if we're after "kind="

            for (i, token) in tokens.iter().enumerate() {
                // Check if this token follows "kind="
                let is_kind_value = after_kind_equals
                    && !token.trim().is_empty()
                    && token
                        .chars()
                        .next()
                        .is_some_and(|c| c.is_alphabetic() || c == '_');

                // Reset after_kind_equals for next iteration (set below if we see kind=)
                after_kind_equals = false;

                // For procedures, check if followed by '('
                // Only convert intrinsics when called as procedures
                let is_procedure_call = F90_PROCEDURES_RE.is_match(token)
                    && next_significant_token(&tokens, i) == Some("(");

                // Check if this is "kind" followed by "="
                if token.eq_ignore_ascii_case("kind") {
                    after_kind_equals = next_significant_token(&tokens, i) == Some("=");
                }

                // Also check if we're at "=" and the previous meaningful token was "kind"
                if token == "="
                    && prev_significant_token(&tokens, i)
                        .is_some_and(|prev| prev.eq_ignore_ascii_case("kind"))
                {
                    after_kind_equals = true;
                }

                // Convert the token
                if is_kind_value && settings.types != CaseMode::NoChange {
                    // Apply type case conversion to kind= value
                    result_part.push_str(&apply_case(token, settings.types));
                } else {
                    result_part.push_str(&convert_token_with_context(
                        token,
                        settings,
                        is_procedure_call,
                    ));
                }
            }
            result_part
        })
        .collect();

    result
}

/// The first token after index `i` that is not whitespace
fn next_significant_token(tokens: &[String], i: usize) -> Option<&str> {
    tokens[i + 1..]
        .iter()
        .map(String::as_str)
        .find(|t| !t.trim().is_empty())
}

/// The last token before index `i` that is not whitespace
fn prev_significant_token(tokens: &[String], i: usize) -> Option<&str> {
    tokens[..i]
        .iter()
        .rev()
        .map(String::as_str)
        .find(|t| !t.trim().is_empty())
}

/// A numeric literal cut off at its exponent letter, e.g. "1.0e" of "1.0e-3"
static EXPONENT_HEAD_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^[\d.]+[dDeE]$").unwrap());

/// Split a string into tokens, preserving separators
fn split_preserving_separators(s: &str) -> Vec<String> {
    let mut tokens = Vec::new();
    let mut current = String::new();
    let chars = s.chars().peekable();

    for c in chars {
        if c.is_alphanumeric() || c == '_' || c == '.' {
            current.push(c);
        } else {
            if !current.is_empty() {
                tokens.push(current);
                current = String::new();
            }
            tokens.push(c.to_string());
        }
    }

    if !current.is_empty() {
        tokens.push(current);
    }

    rejoin_signed_exponents(&tokens)
}

/// Re-join the sign of an exponent onto its literal: `1.0e-3` arrives as
/// "1.0e", "-", "3" because the splitter breaks on `+`/`-`, which left the
/// signed branch of the numeric patterns unreachable and signed exponents
/// unconverted. A sign binds this tightly nowhere else in Fortran: the head
/// must be digits (or a dot) followed by an exponent letter, and the tail
/// must be bare digits, optionally with a kind suffix.
fn rejoin_signed_exponents(tokens: &[String]) -> Vec<String> {
    let mut out: Vec<String> = Vec::with_capacity(tokens.len());
    let mut i = 0;

    while i < tokens.len() {
        let joinable = i + 2 < tokens.len()
            && EXPONENT_HEAD_RE.is_match(&tokens[i])
            && (tokens[i + 1] == "+" || tokens[i + 1] == "-")
            && tokens[i + 2]
                .chars()
                .next()
                .is_some_and(|c| c.is_ascii_digit());
        if joinable {
            out.push(format!("{}{}{}", tokens[i], tokens[i + 1], tokens[i + 2]));
            i += 3;
        } else {
            out.push(tokens[i].clone());
            i += 1;
        }
    }

    out
}

/// Convert a single token based on its type, with context for procedures
///
/// - `is_procedure_call`: true if this procedure name is followed by '('
///   This allows distinguishing between procedure calls (convert) and
///   variable names that happen to match procedure names (don't convert)
fn convert_token_with_context(
    token: &str,
    settings: &CaseSettings,
    is_procedure_call: bool,
) -> String {
    // Skip empty tokens
    if token.is_empty() {
        return token.to_string();
    }

    // Safe: we just verified token is not empty
    let Some(first_char) = token.chars().next() else {
        return token.to_string();
    };
    let second_char = token.chars().nth(1);

    // Check for numeric literals with type suffixes or exponents
    // These start with a digit or a dot followed by a digit (for .0_real64 style)
    let is_numeric_start = first_char.is_ascii_digit()
        || (first_char == '.' && second_char.is_some_and(|c| c.is_ascii_digit()));

    if is_numeric_start {
        if let Some(converted) = convert_numeric_literal(token, settings.types) {
            return converted;
        }
        // If not a special numeric literal, return as-is
        return token.to_string();
    }

    // Skip non-word tokens (not starting with letter or dot)
    if !first_char.is_alphabetic() && first_char != '.' {
        return token.to_string();
    }

    // Check operators first (they have dots)
    if F90_OPERATORS_RE.is_match(token) {
        return apply_case(token, settings.operators);
    }

    // Check keywords
    if F90_KEYWORDS_RE.is_match(token) {
        return apply_case(token, settings.keywords);
    }

    // Check modules (treated as procedures)
    if F90_MODULES_RE.is_match(token) {
        return apply_case(token, settings.procedures);
    }

    // Check procedures - only convert if it's actually a call (followed by '(')
    if F90_PROCEDURES_RE.is_match(token) {
        if is_procedure_call {
            return apply_case(token, settings.procedures);
        }
        // Not a procedure call, could be a variable with same name
        return token.to_string();
    }

    // Check constants
    if F90_CONSTANTS_RE.is_match(token) {
        return apply_case(token, settings.constants);
    }

    token.to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tables_reach_f2023() {
        for keyword in [
            "error",
            "sync",
            "impure",
            "rank",
            "team",
            "stat",
            "errmsg",
            "simple",
            "typeof",
            "classof",
            "enumeration",
            "notify",
            "leading_zero",
        ] {
            assert!(F90_KEYWORDS_RE.is_match(keyword), "keyword: {keyword}");
        }
        for procedure in [
            "out_of_range",
            "co_sum",
            "failed_images",
            "cosd",
            "tanpi",
            "selected_logical_kind",
            "tokenize",
            "f_c_string",
            "ieee_min_num_mag",
        ] {
            assert!(
                F90_PROCEDURES_RE.is_match(procedure),
                "procedure: {procedure}"
            );
        }
        for constant in ["int32", "real64", "logical8", "notify_type", "team_type"] {
            assert!(F90_CONSTANTS_RE.is_match(constant), "constant: {constant}");
        }
    }

    #[test]
    fn test_case_mode_try_from() {
        assert_eq!(CaseMode::try_from(0), Ok(CaseMode::NoChange));
        assert_eq!(CaseMode::try_from(1), Ok(CaseMode::Lower));
        assert_eq!(CaseMode::try_from(2), Ok(CaseMode::Upper));
        assert_eq!(CaseMode::try_from(99), Err(()));
    }

    #[test]
    fn test_apply_case() {
        assert_eq!(apply_case("Hello", CaseMode::NoChange), "Hello");
        assert_eq!(apply_case("Hello", CaseMode::Lower), "hello");
        assert_eq!(apply_case("Hello", CaseMode::Upper), "HELLO");
    }

    #[test]
    fn test_convert_case_keywords() {
        let settings = CaseSettings {
            keywords: CaseMode::Upper,
            ..Default::default()
        };

        assert_eq!(
            convert_case("if (x > 0) then", &settings),
            "IF (x > 0) THEN"
        );
        assert_eq!(convert_case("do i = 1, 10", &settings), "DO i = 1, 10");
        assert_eq!(convert_case("end if", &settings), "END IF");
    }

    #[test]
    fn test_convert_case_procedures() {
        let settings = CaseSettings {
            procedures: CaseMode::Lower,
            ..Default::default()
        };

        assert_eq!(convert_case("x = SIN(y)", &settings), "x = sin(y)");
        assert_eq!(convert_case("n = SIZE(arr)", &settings), "n = size(arr)");
    }

    #[test]
    fn test_convert_case_operators() {
        let settings = CaseSettings {
            operators: CaseMode::Upper,
            ..Default::default()
        };

        assert_eq!(convert_case("x .and. y", &settings), "x .AND. y");
        assert_eq!(convert_case("a .eq. b", &settings), "a .EQ. b");
    }

    #[test]
    fn test_convert_case_preserves_strings() {
        let settings = CaseSettings {
            keywords: CaseMode::Upper,
            ..Default::default()
        };

        // String content should not be modified
        assert_eq!(
            convert_case("print *, \"if then else\"", &settings),
            "PRINT *, \"if then else\""
        );
    }

    #[test]
    fn test_convert_case_no_change() {
        let settings = CaseSettings::default();
        let line = "IF (x > 0) THEN";
        assert_eq!(convert_case(line, &settings), line);
    }

    #[test]
    fn test_case_settings_from_dict() {
        let mut dict = BTreeMap::new();
        dict.insert("keywords".to_string(), CaseMode::Lower);
        dict.insert("procedures".to_string(), CaseMode::Upper);

        let settings = CaseSettings::from_dict(&dict);
        assert_eq!(settings.keywords, CaseMode::Lower);
        assert_eq!(settings.procedures, CaseMode::Upper);
        assert_eq!(settings.operators, CaseMode::NoChange);
        assert_eq!(settings.constants, CaseMode::NoChange);
    }
}
