---
description: Scaffold failing language specs (success and failure paths) for a described behaviour
argument-hint: <behaviour description>
---

Scaffold PooLang language specs for this behaviour: $ARGUMENTS

1. Read the relevant code in `src/` and the existing specs and helpers in
   `tests/language_specs.rs` to learn current behaviour and conventions.
2. Choose names of the form `spec_<component>_<behavior>`, reusing the existing
   component prefixes (for example `spec_for_range_loop_honors_step_value` or
   `spec_checked_api_reports_parse_error_for_missing_semicolon`). Check that no existing
   spec already covers the case.
3. Add at least:
   - one success-path spec asserting the exact resulting `Value` or output, and
   - one spec per relevant failure path asserting the `LangErrorKind` and an actionable
     message. Failures must be typed errors, never panics.
4. Keep each spec to one behaviour with minimal inline PooLang source, reusing existing
   helpers.
5. Run `cargo test --test language_specs` filtered to the new names and confirm each
   fails for the intended reason, or report that it already passes.

Do not implement the behaviour. Report the spec names and their current status.
