# Revision history for kroki

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
