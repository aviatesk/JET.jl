---
name: changelog
description: >-
  Invoke before committing user-facing JET changes such as features, bug fixes,
  compatibility changes, or deprecations. Skip internal refactors, CI or
  docs-only changes, and routine dependency bumps without user impact.
---

# Updating CHANGELOG.md

## Format

This project follows [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## Section structure

Add new entries under `## [Unreleased]`. Do not edit a released section unless
the user explicitly requests it.

Use the established Keep a Changelog subsections as appropriate:

- `### Added` for new functionality
- `### Changed` for changes to existing functionality or compatibility
- `### Deprecated` for features that will be removed
- `### Removed` for removed functionality
- `### Fixed` for bug fixes

Use `### Internal` only when maintainers intentionally want to record a notable
internal change. Do not add empty subsections or introduce a new category when
an existing one fits.

## Entry style

- Start Added entries with "Added ..." and Fixed entries with "Fixed ...".
- Describe Changed entries in terms of concrete behavior or compatibility.
- Refer to GitHub issues as `aviatesk/JET.jl#NNN`.
- Use backticks for report types, function names, configuration names, and
  other code elements.
- Record dependency changes when they affect supported Julia versions,
  compatibility, installation, or user-visible behavior. Omit routine bumps.

## Entry content

Write entries from the user's perspective. Describe what changed in terms of
user-visible behavior, not implementation details.

- Do not include lowered IR details, Compiler.jl hook methods, abstract
  interpretation state, or virtual-process internals unless users can
  reasonably observe or interact with them.
- The "why" of a fix rarely matters to users; the "what" does. If the prior
  behavior is worth mentioning, describe its user-visible symptom rather than
  its internal cause.
