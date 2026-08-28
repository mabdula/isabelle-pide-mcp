/*  Title:      PIDE_MCP/tool_find_consts.scala
    Author:     Maximilian Schäffeler

find_consts as a one-shot PIDE query operation: run the built-in "find_consts"
print function in the theory context at a given line, without modifying the file.

Ported from schaeffm/copilots-isabelle (branch isabelle-agent, commit 194570f).
*/

package isabelle.pide.mcp

import isabelle._

class Tool_Find_Consts extends PIDE_MCP_Tool("find_consts") {
  def description: String =
    "Search for constants (functions, predicates, type constructors) visible in the theory context "
      + "at a given line. "
      + "Use this to discover what constants are available and check their types before writing a "
      + "definition or theorem. "
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
          + "(1) name: <pat> — constants whose qualified name contains pat (e.g. name: List.map); "
          + "(2) \"<type pat>\" — constants whose type contains the pattern (use _ as wildcard; "
          +     "e.g. \"_ \\<Rightarrow> bool\" matches all predicates — keep the double quotes inside the query string); "
          + "(3) strict: \"<type pat>\" — constants whose type matches the pattern exactly. "
          + "Example: \"nat \\<Rightarrow> nat\" -name: Num"))
    ), "required" -> List("origin", "line", "query"))

  override def annotations: Option[JSON.Object.T] = Some(JSON_Object("readOnlyHint" -> true))

  def handle(params: JSON.Object.T): Exn.Result[JSON.T] = Exn.capture {
    val node_name = Exn.release(PIDE_MCP_Tool_Util.origin_param(session, params))
    val snapshot = PIDE_MCP_Tool_Util.require_loaded_origin_snapshot(session, node_name)
    val query = JSON.string(params, "query").getOrElse(error("Missing query parameter"))
    val range = PIDE_MCP_Query_Operation.line_param_range(snapshot, params)

    val result =
      PIDE_MCP_Query_Operation(session, snapshot, "find_consts", List(query), range,
        render = bodies => bodies.flatMap(PIDE_MCP_Query_Operation.collect_items))
    JSON_Object("results" -> Exn.release(result))
  }
}

class Tools_Find_Consts extends PIDE_MCP_Tools(new Tool_Find_Consts)
