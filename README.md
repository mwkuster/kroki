# kroki

A minimal terminal client for [WaniKani](https://www.wanikani.com/) — do your kanji and vocabulary reviews without leaving the command line.

```
┌── Queue ─────────────────────┐ ┌── Current · Apprentice ──────────────────────────────────────────┐
│ 日 (Kanji) [meaning]         │ │                                                                  │
│ 学校 (Vocab) [reading]       │ │  日 — meaning                                                    │
│ 一 (Radical) [meaning]       │ │                                                                  │
│ 日 (Kanji) [reading]         │ │  ┌── Input ──────────────────────────────────────────────────┐  │
│                              │ │  │ sun                                                       │  │
│                              │ │  └───────────────────────────────────────────────────────────┘  │
│                              │ │                                                                  │
│ remaining: 3                 │ │  Enter=submit  Ctrl-o=override  Ctrl-r=requeue                  │
└──────────────────────────────┘ │  Ctrl-a=all info  Ctrl-u=user  Ctrl-v=reviews  Esc=quit         │
                                 └──────────────────────────────────────────────────────────────────┘
```

## Features

- Review meanings and readings in a split-pane TUI
- Current SRS stage (Apprentice / Guru / Master / Enlightened / Burned) shown in the question border
- Romaji input converted to hiragana live as you type (`gakkou` → `がっこう`)
- British/American spelling normalisation for meaning answers
- Per-item level, SRS stage, mnemonics, and component breakdowns (Ctrl-a)
- Review schedule for the next 24 hours (Ctrl-v)
- Optional pronunciation audio via an external player (Ctrl-p)
- Submits results back to WaniKani at the end of each batch; post-submit list shows the resulting SRS stage per item
- Configurable batch size (0 = all available reviews)

## Installation

Requires GHC and Cabal (tested with GHC 9.6, Cabal 3.x).

```bash
git clone https://github.com/yourname/kroki
cd kroki
cabal build
cabal install
```

Or run directly without installing:

```bash
cabal run kroki -- [command]
```

## Configuration

Run the interactive setup wizard once to create `~/.config/kroki/config`:

```bash
kroki init
```

Or write the file manually:

```
token=<your-wanikani-api-token>
batch_size=10
requeue_after=7
audio_player=mpv --really-quiet
```

| Key | Default | Description |
|---|---|---|
| `token` | — | WaniKani API token (required) |
| `batch_size` | 10 | Reviews per session; `0` = all available |
| `requeue_after` | 7 | Positions later to requeue a missed item |
| `audio_player` | — | Command to play audio; URL appended as last argument |

The token can also be supplied via the `WANIKANI_API_TOKEN` environment variable or the `--token` flag. Priority: `--token` > env var > config file.

## Usage

```
kroki                        # start a review session (default)
kroki study --batch-size 20  # session with a custom batch size
kroki whoami                 # show account info
kroki reviews                # show review schedule for the next 24 h
kroki init                   # (re)create config file interactively
```

## TUI keybindings

### During a review

| Key | Action |
|---|---|
| `Enter` | Submit answer |
| `Ctrl-o` | Override — mark current answer as correct |
| `Ctrl-r` | Requeue — skip for now, no wrong-answer penalty |
| `Ctrl-a` | All-info overlay (level, SRS stage, components, meanings, readings, mnemonics) |
| `Ctrl-p` | Play pronunciation audio (vocabulary, requires `audio_player`) |
| `Ctrl-u` | User info overlay |
| `Ctrl-v` | Review schedule overlay |
| `Esc` / `Ctrl-q` | Quit |

### Wrong-answer screen

| Key | Action |
|---|---|
| `Enter` | Requeue (counts as wrong) |
| `Ctrl-o` | Override as correct |
| `Ctrl-r` | Requeue without penalty |

### Overlays (all-info, user, reviews)

| Key | Action |
|---|---|
| `↑` / `k` | Scroll up |
| `↓` / `j` | Scroll down |
| `Ctrl-a` / `Ctrl-u` / `Ctrl-v` / `Esc` | Close overlay |

### Done screen

| Key | Action |
|---|---|
| `Ctrl-s` | Submit batch results to WaniKani |
| `Ctrl-n` | Start next batch (if more reviews are available) |
| `↑↓` / `j` / `k` | Scroll submission details |
| `Esc` / `Ctrl-q` | Quit |

## License

GPL-3.0-only — see [LICENSE](LICENSE).
