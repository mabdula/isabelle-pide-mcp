/*  Title:      PIDE_MCP/tool_get_state.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.mcp

import isabelle._

class Tool_Get_State extends PIDE_MCP_Tool("get_state") {
  def description: String =
    "Inspect the state of a (range in a) file: goals, variables, errors, warnings, etc. "
      + "Returns a summary (errors, warnings, commands still running, total timing,...) and details for all commands in range. "
      + "**Use this frequently to check if you are making progress, what is left to be done, and importantly, if certain commands are still processing, potentially even looping.** "
      + "For non-theory files, the response has a single command entry derived from the command that loaded it in the respective theory. "
      + "**Set wait_until_processed to block until processing has finished instead of polling this tool in a loop.** "
      + PIDE_MCP_Tool_Schema.implicit_load_file

  def input_schema: JSON.Object.T =
    JSON_Object("type" -> "object", "properties" -> JSON_Object(
      PIDE_MCP_Tool_Schema.origin_prop,
      PIDE_MCP_Tool_Schema.opt_start_line_prop,
      PIDE_MCP_Tool_Schema.opt_end_line_prop,
      "include_types" -> JSON_Object("type" -> "boolean",
        "description" -> "Include types (Isabelle and ML) for variables and constants.",
        "default" -> false),
      "include_facts" -> JSON_Object("type" -> "boolean",
        "description" -> "Include facts/theorems used in range.",
        "default" -> false),
      "include_infos" -> JSON_Object("type" -> "boolean",
        "description" -> "Include writeln and information output in the state. This can get large, but often contains useful information. Avoid using it for large ranges if possible.",
        "default" -> false),
      "include_full_markup" -> JSON_Object("type" -> "boolean",
        "description" -> "Include full PIDE markup information. This gets very large - **use only sparingly and very targeted to get local details**.",
        "default" -> false),
      "commands_limit" -> JSON_Object("type" -> "integer",
        "description" -> ("Maximum number of commands to return, both in the \"commands\" list and per "
          + "summary category (errors, warnings, still running, ...). Omit to return all commands. "
          + "**Set to 0 whenever you only want to get summary information (e.g. to see if there are any errors, warnings, etc.); "
          + "this returns counts only and is always small enough to fit your context.** "
          + "Set it to a small number (e.g. 5) to also see the first few offending commands. "
          + "Note that the returned summary counts all commands, even the truncated ones; "
          + "\"count_returned\" tells you how many details are included. "
          + "Warning: for large ranges, there are thousands of commands that may flood your context. Set the commands limit to 0 if you do not need details."),
        "minimum" -> 0),
      PIDE_MCP_Tool_Schema.wait_until_processed_prop(
        "Block until the origin is fully processed (all its commands terminated), "
          + "then return the state. Loads the origin first if required. "
          + "**Use this instead of calling get_state or get_progress repeatedly to wait for processing.**"),
      PIDE_MCP_Tool_Schema.wait_timeout_prop
    ), "required" -> List("origin"))

  override def annotations: Option[JSON.Object.T] = Some(JSON_Object("readOnlyHint" -> true))

  def handle(params: JSON.Object.T): Exn.Result[JSON.T] = Exn.capture {
    val node_name = Exn.release(PIDE_MCP_Tool_Util.origin_param(session, params))
    val wait_until_processed = PIDE_MCP_Tool_Util.wait_until_processed_param(params)
    val wait_timeout = PIDE_MCP_Tool_Util.wait_timeout_param(params)
    val start = Time.now()
    val (snapshot, opt_processed) =
      if (wait_until_processed) {
        val (snapshot, processed) =
          PIDE_MCP_Tool_Util.await_loaded_origin_snapshot(session, node_name, wait_timeout)
        (snapshot, Some(processed))
      }
      else (PIDE_MCP_Tool_Util.require_loaded_origin_snapshot(session, node_name), None)
    val opt_start_line = JSON.int(params, "start_line")
    val opt_end_line = JSON.int(params, "end_line")
    val doc = Line.Document(snapshot.node.source)
    val (start_line, end_line) =
      Exn.release(PIDE_MCP_Tool_Util.resolve_lines(opt_start_line, opt_end_line, doc.lines.length))
    val include_types = JSON.bool(params, "include_types").getOrElse(false)
    val include_facts = JSON.bool(params, "include_facts").getOrElse(false)
    val include_infos = JSON.bool(params, "include_infos").getOrElse(false)
    val include_full_markup = JSON.bool(params, "include_full_markup").getOrElse(false)
    val opt_limit = JSON.int(params, "commands_limit")

    val range = PIDE_MCP_Util.text_range(doc, start_line, end_line).get
    val entries = PIDE_MCP_Commands.state_entries(snapshot, Some(range))
    val opts = PIDE_MCP_Commands.State_Options(include_types, include_facts, include_infos, include_full_markup)
    val state = PIDE_MCP_Commands.state_json(entries, doc, opts, opt_limit)
    opt_processed match {
      case None => state
      case Some(processed) =>
        PIDE_MCP_Tool_Util.wait_json(processed, Time.now() - start, wait_timeout) ++ state
    }
  }
}

class Tools_Get_State extends PIDE_MCP_Tools(new Tool_Get_State)
