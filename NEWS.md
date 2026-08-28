PIDE MCP NEWS -- history of user-relevant changes
=================================================

New in this PIDE MCP version
----------------------------

* New get_progress parameters wait_until_processed (default: false) and wait_timeout (default: 120s):
  block until ALL theories in question are consolidated. With origins given, those theories are
  loaded first if required; with origins omitted, the wait covers every theory loaded at the time of
  the call. On timeout, "origins_unprocessed" names the theories that are still not done.
* New get_progress parameter details_limit (default: 10) bounds the number of theory entries and of
  running-command entries. Theories are ordered failed, then pending, then ok, so truncation drops
  the finished ones; "theories_count"/"count" always cover everything and details_limit = 0 gives a
  counts-only summary. Command status counts of 0 are omitted per theory entry, and the list of
  running commands became an object with "count", "count_returned" and "commands", as in get_state.
* New get_state parameters wait_until_processed (default: false) and wait_timeout (default: 120s):
  block until the origin is fully processed instead of polling get_state/get_progress in a loop.
  On timeout, the state is returned with "processed": false and "wait_timed_out": true, which
  distinguishes a non-terminating command from one that is merely not finished yet.
* get_state commands_limit now also bounds the command details per summary category
  (errors, warnings, still running, ...), and each category reports "count_returned".
  In particular, commands_limit = 0 now yields a small counts-only summary.
* Update get_state: include tracing, remove unprocessed command details, and fix wrong message line number attribution for blobs.
* Set visible perspective on last read or edited origin only. Dependencies are hidden. Use option pide_mcp_range_context to set number of lines around range that stay visible.
* Await stable snapshot after editing operations (without blocking other tools)
* Reworked loading and editing: closes several issues reported by Diego Marmsoler and Yong Kiam Tan (stuck process due to invalid imports and de-synchronisation of session and resources, edit race conditions, etc.)
* New tool unload: Remove ("clean") a theory and its dependents.
* New tool get_progress: show progress of theories and currently processed commands.
* list_loaded_theories now excludes non-theories.
* Add tool plugin system: users can freely add and remove (new) tools.
* The edit tool now matches substrings within a given range and can edit multiple occurrences at once. 
  This is in line with how most coding agents implement their edit tools.

New in PIDE MCP 2025-2
----------------------------

* Port of PIDE MCP c13a4bd3c018 compatible with Isabelle2025-2
* Includes all post PIDE MCP c13a4bd3c018 updates until the release of PIDE MCP 2026

New in PIDE MCP c13a4bd3c018
----------------------------

* Initial release compatible with Isabelle/c13a4bd3c018
