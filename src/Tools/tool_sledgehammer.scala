/*  Title:      PIDE_MCP/tool_sledgehammer.scala
    Author:     Maximilian Schäffeler

Sledgehammer as a one-shot PIDE query operation: run the built-in "sledgehammer"
print function at a goal position via a temporary document overlay, without
modifying the file.

Ported from schaeffm/copilots-isabelle (branch isabelle-agent, commit 194570f).
*/

package isabelle.pide.mcp

import isabelle._

class Tool_Sledgehammer extends PIDE_MCP_Tool("sledgehammer") {
  def description: String =
    "Run Sledgehammer on the open proof goal at a given line, invoking external ATPs to find a proof. "
      + "Returns a list of proof method invocations that close the goal (e.g. \"by (metis foo bar)\"), "
      + "or an empty list if no proof was found. "
      + "Runs as a read-only overlay: the file is NOT modified. "
      + PIDE_MCP_Tool_Schema.implicit_load_file

  def input_schema: JSON.Object.T =
    JSON_Object("type" -> "object", "properties" -> JSON_Object(
      PIDE_MCP_Tool_Schema.origin_prop,
      "line" -> JSON_Object("type" -> "integer",
        "description" -> ("Line number within the file. Must fall inside an open proof step with an "
          + "unsolved goal (e.g. inside a proof block, after show/have/suffices). "
          + "Point it at the `sorry` or tactic line whose goal you want to target."),
        "minimum" -> 1),
      "timeout" -> JSON_Object("type" -> "number",
        "description" -> "Time limit in seconds for sledgehammer (default: 10).",
        "minimum" -> 1, "default" -> 10)
    ), "required" -> List("origin", "line"))

  override def annotations: Option[JSON.Object.T] = Some(JSON_Object("readOnlyHint" -> true))

  def handle(params: JSON.Object.T): Exn.Result[JSON.T] = Exn.capture {
    val node_name = Exn.release(PIDE_MCP_Tool_Util.origin_param(session, params))
    val snapshot = PIDE_MCP_Tool_Util.require_loaded_origin_snapshot(session, node_name)
    val range = PIDE_MCP_Query_Operation.line_param_range(snapshot, params)
    val timeout = Time.seconds(JSON.double(params, "timeout").getOrElse(10.0))

    val override_params = ""; val do_learn = "false"; val fact_selection = "true"
    val result =
      PIDE_MCP_Query_Operation(session, snapshot, "sledgehammer",
        List(override_params, do_learn, fact_selection), range, timeout = timeout,
        render = bodies => bodies.flatMap(PIDE_MCP_Query_Operation.sendback_commands).distinct)
    JSON_Object("results" -> Exn.release(result))
  }
}

class Tools_Sledgehammer extends PIDE_MCP_Tools(new Tool_Sledgehammer)
