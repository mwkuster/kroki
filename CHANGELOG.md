# Revision history for kroki

## 0.9.6 -- 2026-05-17

* All-info overlay (Ctrl-a): kanji subjects now list visually similar kanji, with their readings and meanings
* `kroki <command> --help` now also lists the global `--token` option, so every option a command accepts is shown

## 0.9.5 -- 2026-05-02

* `Enter` now also closes the all-info overlay (Ctrl-a), alongside Ctrl-a and Esc
* Wrong-answer feedback lines wrap when the list of accepted answers is long
* Ctrl-a on a fresh question (no input typed yet) now opens the all-info overlay for the just-answered subject, so you can review what you just got right or wrong before starting the next one

## 0.9.4 -- 2026-04-26

* TUI no longer freezes during review submission: POSTs run on a background thread, with a "Submitting…" banner and input blocked until the result arrives
* Submissions run in parallel (capped at 50 in-flight) to keep wall-time short on large batches without breaching WaniKani's 60 req/min limit
* Refactor: `src/Tui.hs` split into `Tui.State`, `Tui.Draw`, and `Tui.Event` submodules behind a thin facade

## 0.9.3 -- 2026-04-25

* All-info overlay (Ctrl-a): kanji subjects now list the vocabulary that uses them; vocabulary subjects show the accepted readings of each component kanji
* Fix duplicate hour label in the review schedule overlay (Ctrl-v)

## 0.9.2 -- 2026-04-12

* Release workflow: fix Linux static build by dropping `gmp-static`; grant `contents: write` so binaries upload to GitHub releases

## 0.9.1 -- 2026-04-12

* Add GitHub Actions release workflow producing static Linux x86-64 and macOS arm64 binaries on `v*` tags

## 0.9.0.0 -- 2026-04-12

* SRS stage shown in question border (`Current · Apprentice`) and in the Ctrl-a info overlay
* Post-submission list shows the resulting SRS stage per item (`→ Guru`)
* Subject level shown in Ctrl-a info overlay
* British/American spelling normalisation for meaning answers (including `-ourable`/`-ourite` suffixes)
* Pronunciation audio playback via configurable external player (Ctrl-p)
* Review schedule overlay for the next 24 hours (Ctrl-v)
* Romaji→hiragana live conversion during reading input
* Full WaniKani review flow: fetch → study → submit
