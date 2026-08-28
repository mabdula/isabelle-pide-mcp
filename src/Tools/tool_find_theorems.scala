/*  Title:      PIDE_MCP/tool_find_theorems.scala
    Author:     Maximilian Schäffeler

find_theorems as a one-shot PIDE query operation: run the built-in
"find_theorems" print function in the theory context at a given line, without
modifying the file.

Ported from schaeffm/copilots-isabelle (branch isabelle-agent, commit 194570f).
*/

package isabelle.pide.mcp

import isabelle._

class Tool_Find_Theorems extends PIDE_MCP_Tool("find_theorems") {
  def description: String =
    "Search for theorems visible in the theory context at a given line. "
      + "Results respect locale parameters, local assumptions, and imports at that position. "
      + "Use this to find library lemmas before attempting a manual proof or calling sledgehammer. "
      + "Runs as a read-only overlay: the file is NOT modified. "
      + PIDE_MCP_Tool_Schema.implicit_load_file

  def input_schema: JSON.Object.T =
    JSON_Object("type" -> "object", "properties" -> JSON_Object(
      PIDE_MCP_Tool_Schema.origin_prop,
      "line" -> JSON_Object("type" -> "integer",
        "description" -> ("Line number within the file. The theory context at the enclosing command "
          + "(visible constants, locale parameters, local assumptions) is used for the search."),
        "minimum" -> 1),
      "query" -> JSON_Object("type" -> "string",
        "description" -> ("Search query; criteria are separated by spaces. "
          + "Prefix any criterion with - to exclude matches. "
          + "Supported criteria: "
          + "(1) name: <pat> — theorems whose qualified name contains pat (e.g. name: Nat.add); "
          + "(2) \"<term pat>\" — theorems matching the term pattern "
          +     "(use _ or ?x as wildcards; e.g. \"?xs @ [] = ?xs\" — keep double quotes inside query string); "
          + "(3) simp: \"<pat>\" — theorems usable as simp rules rewriting the pattern; "
          + "(4) intro, elim, dest — intro/elim/destruction rules; "
          + "(5) solves — rules that close the current goal outright. "
          + "Example: intro \"_ \\<and> _\" -name: HOL")),
      "limit" -> JSON_Object("type" -> "integer",
        "description" -> "Maximum number of theorems to return (default: 10).",
        "minimum" -> 1, "default" -> 10)
    ), "required" -> List("origin", "line", "query"))

  override def annotations: Option[JSON.Object.T] = Some(JSON_Object("readOnlyHint" -> true))

  def handle(params: JSON.Object.T): Exn.Result[JSON.T] = Exn.capture {
    val node_name = Exn.release(PIDE_MCP_Tool_Util.origin_param(session, params))
    val snapshot = PIDE_MCP_Tool_Util.require_loaded_origin_snapshot(session, node_name)
    val query = JSON.string(params, "query").getOrElse(error("Missing query parameter"))
    val limit = JSON.int(params, "limit").getOrElse(10)
    val range = PIDE_MCP_Query_Operation.line_param_range(snapshot, params)

    val following_all = "false"
    val result =
      PIDE_MCP_Query_Operation(session, snapshot, "find_theorems",
        List(limit.toString, following_all, query), range,
        render = bodies => bodies.flatMap(PIDE_MCP_Query_Operation.collect_items))
    JSON_Object("results" -> Exn.release(result))
  }
}

class Tools_Find_Theorems extends PIDE_MCP_Tools(new Tool_Find_Theorems)
