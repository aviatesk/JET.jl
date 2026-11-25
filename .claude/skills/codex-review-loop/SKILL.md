---
name: codex-review-loop
description: >
  When running as Claude Code in JET, have Codex review your work at
  implementation milestones and iterate on its findings. Use after finishing a
  coherent unit of a multi-step change (and its tests pass), before reporting a
  non-trivial change as complete, or when the user asks for a Codex review.
  Skip for trivial edits.
argument-hint: "[extra review focus]"
---

# Codex review loop

Get an independent review from Codex (OpenAI's coding agent CLI), judge
its findings yourself, fix what is valid, and let the same Codex session
re-check until it approves or the round limit is hit.

Extra review focus from the user, if any: $ARGUMENTS

## When to run

- At milestones: a coherent unit of the task is done and its relevant
  tests and self diagnostics pass. Not after every edit.
- Before reporting a non-trivial change as complete.
- When the user asks for it.

## Rules

- Codex must not modify the git worktree and should avoid writing files
  elsewhere too. Writes that the commands it needs to run cannot avoid, such
  as `julia` updating `~/.julia/compiled`, are acceptable.
- Codex can be wrong. Verify every finding against the code before
  acting on it. Reject findings that are incorrect, out of scope, or
  conflict with the user's instructions or project rules, and say why.
- Codex tends to flag edge cases that are possible only in theory. Accept
  one only after confirming a real input or call path in this codebase
  reaches it. Do not complicate the implementation to handle cases that
  cannot occur in practice; reject such findings.
- Do not edit files while a Codex run is in progress.
- No git write operations as part of this loop; the review target is the
  working tree.
- At most 3 rounds per milestone. If Codex and you still disagree after
  that, stop and leave the decision to the user.
- If `codex` is not found or fails to start, stop and tell the user
  instead of working around it.
- If a run prints `WORKTREE CHANGED`, Codex modified the worktree. Stop and
  tell the user; do not revert anything yourself.

## Sandbox

Outside Claude Code's sandbox, Codex runs in its own read-only sandbox
(`-s read-only`). Inside Claude Code's sandbox, which sets `SANDBOX_RUNTIME`,
Codex cannot create one, because the operating system does not nest
sandboxes. There, run Codex with `--dangerously-bypass-approvals-and-sandbox`
so that Claude Code's sandbox confines it instead. Codex then needs write
access to `~/.codex` and network access to OpenAI; if it fails under the
sandbox, stop and tell the user what was denied. The commands below choose
the mode and check that the worktree did not change during the run.

## Round 1

Run in the foreground with the Bash tool's maximum timeout (600000 ms).
Pass the prompt on stdin via a heredoc. Stdout is only Codex's final
message; the full log goes to a file.

```bash
log=$(mktemp -t codex-review) || exit 1
if [ -n "$SANDBOX_RUNTIME" ]; then
  mode=(--dangerously-bypass-approvals-and-sandbox)
else
  mode=(-s read-only)
fi
fp() { { git status --porcelain=v1 -uall; git diff --binary HEAD; } | git hash-object --stdin; }
before=$(fp)
codex exec "${mode[@]}" - 2> "$log" <<'EOF'
<prompt>
EOF
rc=$?
[ "$(fp)" = "$before" ] || { echo "WORKTREE CHANGED during the Codex run"; rc=99; }
echo "exit=$rc session=$(sed -n 's/^session id: //p' "$log") log=$log"
```

Keep the printed session id; later rounds resume that session.

The prompt must be self-contained, since Codex has none of your context.
Codex reads `AGENTS.md` itself, so do not restate project rules.

- Goal of the overall task and of this milestone.
- What changed in this milestone (files, key functions) and what was
  verified (tests and self diagnostics run, with results).
- Review target: the uncommitted changes (`git diff HEAD` and untracked
  files), focusing on this milestone's files; the rest of the diff is
  context only.
- What to look for: correctness bugs, unhandled edge cases, design
  problems, missing tests. No style nits unless they matter. Ground
  every finding in the code. Report an edge case only with the concrete
  input or call path that triggers it. Do not modify any files.
- Any extra focus from the user.
- This output contract:

```text
First line exactly one of:
VERDICT: LGTM
VERDICT: CHANGES_REQUESTED
Then numbered findings, each as:
<n>. [high|medium|low] <path>:<line> — <problem> — <suggested fix>
```

If the run hits the timeout, the review scope was too large: narrow it to
fewer files and run once more. If it times out again, stop and report.

## Evaluate and fix

For each finding, decide: accept (fix it) or reject (with a concrete
reason). Fix the accepted ones, then validate as `AGENTS.md` requires
(relevant tests and self diagnostics).

## Round 2 and later

Resume the same session so Codex keeps its context. `resume` does not
inherit the sandbox mode, so pass it again.

```bash
if [ -n "$SANDBOX_RUNTIME" ]; then
  mode=(--dangerously-bypass-approvals-and-sandbox)
else
  mode=(-c sandbox_mode=read-only)
fi
fp() { { git status --porcelain=v1 -uall; git diff --binary HEAD; } | git hash-object --stdin; }
before=$(fp)
codex exec resume "${mode[@]}" <session-id> - 2>> <log> <<'EOF'
<prompt>
EOF
rc=$?
[ "$(fp)" = "$before" ] || { echo "WORKTREE CHANGED during the Codex run"; rc=99; }
echo "exit=$rc"
```

The prompt lists each finding by number with your disposition (fixed and
how, or rejected and why) and the validation you ran. Ask Codex to
verify the fixes, reconsider the rejections, and answer with the same
output contract. Do not repeat the round-1 context.

Stop when Codex returns `VERDICT: LGTM`, when the remaining findings are
only ones you rejected with solid reasons, or at the round limit.

## Report to the user

Briefly:

- Rounds run and the final verdict.
- Findings fixed.
- Findings rejected, with reasons, and any unresolved disagreement.
- The Codex session id, so the user can continue it with
  `codex resume <session-id>`.
