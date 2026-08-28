/*  Title:      PIDE_MCP/tool_get_progress.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.mcp

import isabelle._

object Tool_Get_Progress {

  val default_details_limit: Int = 10

  def theory_status_key(key: String): String = "theories_" + key

  def prefix_theory_status_keys(obj: JSON.Object.T): JSON.Object.T = {
    val theory_status_keys = Document_Status.Overall_Status.values.map(_.toString).toSet
    obj.map { case (k, v) => if (theory_status_keys.contains(k)) (theory_status_key(k), v) else (k, v) }
  }

  sealed case class Theories_Progress(
    session: PIDE_MCP_Session,
    snapshot: Document.Snapshot,
    theories: List[Document.Node.Name]
  ) {
    private lazy val nodes_status: Document_Status.Nodes_Status =
      Document_Status.Nodes_Status.empty.update_nodes(
        Date.now(), session.resources, snapshot.state, snapshot.version, domain = Some(theories.toSet))

    /*Interesting theories first, so that truncation drops the finished ones: failed
      before pending before ok, and alphabetical within each group for stable output.*/
    private def rank(name: Document.Node.Name): Int =
      nodes_status.overall_status(name) match {
        case Document_Status.Overall_Status.failed => 0
        case Document_Status.Overall_Status.pending => 1
        case _ => 2
      }

    def theory_states_json(limit: Option[Int]): JSON.Object.T = {
      val sorted = theories.sortBy(name => (rank(name), session.origin(name)))
      val shown = limit.fold(sorted)(sorted.take)
      val theory_states = JSON_Object(shown.map { name =>
        val node_status = nodes_status(name)
        // Node_Status.canceled is a boolean (is any command canceled), unlike get_state's "commands_canceled"
        // which is a per-command count; we hence omit it here to avoid confusion.
        // zero counts are omitted: a missing key means 0, as for absent sections in get_state
        val command_status_counts = JSON_Object(
          List(
            PIDE_MCP_Commands.Status.unprocessed -> node_status.unprocessed,
            PIDE_MCP_Commands.Status.running -> node_status.running,
            PIDE_MCP_Commands.Status.warned -> node_status.warned,
            PIDE_MCP_Commands.Status.failed -> node_status.failed,
            PIDE_MCP_Commands.Status.finished -> node_status.finished)
          .filter({ case (_, count) => count > 0 }): _*)
        session.origin(name) -> (JSON_Object(
          "overall_status" -> nodes_status.overall_status(name).toString,
          "percentage_commands_processed" -> node_status.percentage) ++
          PIDE_MCP_Commands.prefix_command_status_keys(command_status_counts))
      }: _*)

      val by_status = theories.groupMapReduce(nodes_status.overall_status(_).toString)(_ => 1)(_ + _)
      val theory_status_counts = JSON_Object(Document_Status.Overall_Status.values.toList.map(s =>
        s.toString -> by_status.getOrElse(s.toString, 0)): _*)

      prefix_theory_status_keys(theory_status_counts) ++
        JSON_Object(
          "theories_count" -> theories.length,
          "theories_count_returned" -> theory_states.size,
          "theories" -> theory_states)
    }

    def commands_running_json(limit: Option[Int]): JSON.Object.T = {
      val opts = PIDE_MCP_Commands.State_Options()
      val running =
        theories.flatMap { name =>
          val node_snapshot = snapshot.switch(name)
          PIDE_MCP_Commands.state_entries_commands(node_snapshot, None).toList.flatMap { entry =>
            Option.when(entry.status.contains(PIDE_MCP_Commands.Status.running))((name, node_snapshot, entry))
          }
        }
      val shown = limit.fold(running)(running.take)
      val commands = shown.map { case (name, node_snapshot, entry) =>
        val doc = Line.Document(node_snapshot.node.source)
        JSON_Object("origin" -> session.origin(name)) ++ entry.json(doc, opts)
      }
      JSON_Object("count" -> running.length, "count_returned" -> commands.length,
        "commands" -> commands)
    }
  }

  def theory_names(
    session: PIDE_MCP_Session,
    snapshot: Document.Snapshot,
    origins: Set[String]
  ): List[Document.Node.Name] = {
    val (_, dynamic) = Tool_List_Loaded_Theories.loaded_theories(session, snapshot.version.nodes)
    if (origins.isEmpty) dynamic else dynamic.filter(name => origins.contains(session.origin(name)))
  }
}

class Tool_Get_Progress extends PIDE_MCP_Tool("get_progress") {
  def description: String = "Show progress of theories and list currently running commands. "
    + "**Use this for global progress overview, e.g. when you think the prover is stuck.** "
    + "**Set wait_until_processed to block until the theories in question are processed instead of polling this tool in a loop.** "
    + "Note that sorrys are not listed."

  def input_schema: JSON.Object.T =
    JSON_Object("type" -> "object", "properties" -> JSON_Object(
      "origins" -> JSON_Object("type" -> "array",
        "items" -> JSON_Object("type" -> "string"),
        "description" -> ("Restrict to these origins (session-qualified theory names or file paths). "
          + "Omit to include all theories.")),
      PIDE_MCP_Tool_Schema.wait_until_processed_prop(
        "Block until ALL theories in question are fully processed (all their commands terminated), "
          + "then return the progress. With origins given, that is exactly those theories, and any of "
          + "them that is not loaded yet is loaded first; with origins omitted, it is every theory "
          + "loaded at the time of the call - **prefer listing the origins you care about, since a "
          + "single stuck theory anywhere then makes the call run into the full wait_timeout.** "
          + "On timeout, \"origins_unprocessed\" names the theories that are still not done. "
          + "\"processed\": true means every theory in question has overall_status ok or failed."),
      PIDE_MCP_Tool_Schema.wait_timeout_prop,
      "details_limit" -> JSON_Object("type" -> "integer",
        "description" -> ("Maximum number of theory entries and of running-command entries to return "
          + "(default: " + Tool_Get_Progress.default_details_limit + "). "
          + "Theories are ordered failed first, then pending, then ok, so truncation drops the "
          + "finished ones; \"theories_count\" and the \"count\" fields always cover everything. "
          + "Within a theory entry, command status counts of 0 are omitted. "
          + "**Set to 0 whenever you only want the global counts** (how many theories are ok/failed/pending, "
          + "how many commands are running); this is always small enough to fit your context."),
        "minimum" -> 0)
    ))

  override def annotations: Option[JSON.Object.T] = Some(JSON_Object("readOnlyHint" -> true))

  /*Wait until all given theories are consolidated, sharing one deadline. Returns the ones
    that are still not done; a timeout is not an error (cf. get_state). The criterion is
    is_consolidated rather than is_processed, so that "processed": true agrees with the
    overall_status reported for each theory below.*/
  private def await_consolidated(
    node_names: List[Document.Node.Name],
    timeout: Time
  ): List[Document.Node.Name] = {
    val deadline = Time.now() + timeout
    node_names.filterNot(name =>
      session.await_processed(name, deadline - Time.now(),
        is_done = session.is_consolidated)._2)
  }

  def handle(params: JSON.Object.T): Exn.Result[JSON.T] = Exn.capture {
    val origin_names = JSON.strings(params, "origins").getOrElse(Nil)
      .map(s => Exn.release(session.node_name(s)))
    val origins = origin_names.map(session.origin).toSet
    val wait_until_processed = PIDE_MCP_Tool_Util.wait_until_processed_param(params)
    val wait_timeout = PIDE_MCP_Tool_Util.wait_timeout_param(params)
    val limit = JSON.int(params, "details_limit").orElse(Some(Tool_Get_Progress.default_details_limit))
    val start = Time.now()

    val opt_unprocessed =
      Option.when(wait_until_processed) {
        val targets =
          if (origin_names.nonEmpty) {
            for (name <- origin_names if !name.is_theory)
              error("get_progress reports theories only, but " + session.origin(name)
                + " is an auxiliary file. Use get_state to wait for it.")
            origin_names.foreach(PIDE_MCP_Tool_Util.load_origin_if_required(session, _))
            origin_names
          }
          else Tool_Get_Progress.theory_names(session, session.snapshot(), Set.empty)
        await_consolidated(targets.distinct, wait_timeout)
      }

    val snapshot = session.snapshot()
    val progress = Tool_Get_Progress.Theories_Progress(
      session, snapshot, Tool_Get_Progress.theory_names(session, snapshot, origins))
    val state =
      JSON_Object(
        PIDE_MCP_Commands.command_status_key(PIDE_MCP_Commands.Status.running) ->
          progress.commands_running_json(limit)) ++
        progress.theory_states_json(limit)

    opt_unprocessed match {
      case None => state
      case Some(unprocessed) =>
        PIDE_MCP_Tool_Util.wait_json(unprocessed.isEmpty, Time.now() - start, wait_timeout,
          timeout_details =
            JSON_Object("origins_unprocessed" -> unprocessed.map(session.origin))) ++ state
    }
  }
}

class Tools_Get_Progress extends PIDE_MCP_Tools(new Tool_Get_Progress)
