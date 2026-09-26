/*  Title:      PIDE_MCP/pide_mcp_tool_schema.scala
    Author:     Kevin Kappelmann

Shared JSON-Schema fragments for MCP tools.
*/

package isabelle.pide.mcp

import isabelle._

object PIDE_MCP_Tool_Schema {
  val implicit_load_file: String = "Implicitly loads the file if required."
  val implicit_reload_file: String = "Implicitly (re)loads the file."
  val range_visibility: String = "Sets the PIDE perspective on the range."

  val origin_prop =
    "origin" -> JSON_Object("type" -> "string",
      "description" -> "Session-qualified theory name (e.g. \"HOL.Nat\") or file path (e.g. \"foo.ML\").")
  val start_line_prop =
    "start_line" -> JSON_Object("type" -> "integer",
      "description" -> "First line to include.", "minimum" -> 1)
  val opt_start_line_prop =
    "start_line" -> JSON_Object("type" -> "integer",
      "description" -> "First line to include.", "minimum" -> 1, "default" -> 1)
  val opt_end_line_prop =
    "end_line" -> JSON_Object("type" -> "integer",
      "description" -> "Last line to include (default: end of file).", "minimum" -> 1)

  /* waiting for processing to finish, instead of polling */

  val default_wait_timeout: Time = Time.seconds(120)

  def wait_until_processed_prop(description: String) =
    "wait_until_processed" -> JSON_Object("type" -> "boolean",
      "description" -> (description
        + " The wait is bounded by wait_timeout; on timeout, the current state is returned anyway with "
        + "\"processed\": false and \"wait_timed_out\": true - that is the signal for a command that "
        + "does not terminate, as opposed to one that is merely not done yet."),
      "default" -> false)

  val wait_timeout_prop =
    "wait_timeout" -> JSON_Object("type" -> "number",
      "description" -> ("Maximum time in seconds to block when wait_until_processed is set "
        + "(default: " + default_wait_timeout.seconds.toInt + "). Ignored otherwise."),
      "minimum" -> 0, "default" -> default_wait_timeout.seconds)

  /* per-call rationale, required when the server runs with -r */

  val reason_key = "reason"
  val reason_prop =
    reason_key -> JSON_Object("type" -> "string", "minLength" -> 1,
      "description" -> ("One sentence, written BEFORE the call: what you are about to do and why. "
        + "Not part of the operation - it is recorded so a reader can see why each call was made."))

  /* Put the reason property first, so an agent writes its rationale before the
     operation's own arguments, and make it required. */
  def with_reason(schema: JSON.Object.T): JSON.Object.T = {
    val props = schema.get("properties") match {
      case Some(JSON.Object(ps)) => ps
      case _ => JSON.Object.empty
    }
    val required = schema.get("required") match {
      case Some(xs: List[_]) => xs.collect { case x: String => x }
      case _ => Nil
    }
    schema +
      ("properties" -> (JSON_Object(reason_prop) ++ (props - reason_key))) +
      ("required" -> (reason_key :: required.filterNot(_ == reason_key)))
  }
}
