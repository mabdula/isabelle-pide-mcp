/*  Title:      PIDE_MCP/pide_mcp_tool_util.scala
    Author:     Kevin Kappelmann

Utilities for PIDE MCP Tools.
*/

package isabelle.pide.mcp

import isabelle._

object PIDE_MCP_Tool_Util {
  val retry_soon_message: String = "Please retry soon."

  def make_tool_table: Exn.Result[Map[String, PIDE_MCP_Tool]] = Exn.capture {
    Isabelle_System.make_services(classOf[PIDE_MCP_Tools]).flatMap(_.entries).
      foldLeft(Map.empty[String, PIDE_MCP_Tool]) {
        case (tools, tool) =>
          if (tools.isDefinedAt(tool.name))
            error("Duplicate PIDE_MCP tool: " + Library.quote(tool.name))
          else tools + (tool.name -> tool)
      }
  }

  def origin_param(session: PIDE_MCP_Session, params: JSON.Object.T): Exn.Result[Document.Node.Name] =
    Exn.capture {
      val origin = JSON.string(params, "origin").getOrElse(error("Missing origin parameter"))
      Exn.release(session.node_name(origin))
    }

  private def require_blob_loaded(
    session: PIDE_MCP_Session,
    node_name: Document.Node.Name,
    snapshot: Document.Snapshot
  ): Unit =
    if (!node_name.is_theory && snapshot.commands_loading.isEmpty)
      error(s"File ${session.origin(node_name)} is not loaded by any theory. Load the containing theory first.")

  private def is_loaded_origin(session: PIDE_MCP_Session, node_name: Document.Node.Name): Boolean =
    session.is_base_session_theory(node_name) ||
      PIDE_MCP_Util.is_loaded_dynamic(Exn.release(session.tip_version()).nodes, node_name)

  // pass empty range to load empty nodes
  private def queue_for_loading(session: PIDE_MCP_Session, node_name: Document.Node.Name): Unit =
    Exn.release(session.read_update_resolve(node_name, List((1, Some(0))),
      await_stable_before_resolve = false, hide_others = false, range_context = 0))

  def load_origin_if_required(session: PIDE_MCP_Session, node_name: Document.Node.Name): Unit =
    if (!is_loaded_origin(session, node_name)) queue_for_loading(session, node_name)

  def require_loaded_origin_snapshot(
    session: PIDE_MCP_Session,
    node_name: Document.Node.Name
  ): Document.Snapshot = {
    val snapshot =
      session.node_snapshot(node_name) match {
        case Exn.Res(snapshot) => snapshot
        case Exn.Exn(_) =>
          if (PIDE_MCP_Util.is_loaded_dynamic(Exn.release(session.tip_version()).nodes, node_name))
            error(s"The origin ${session.origin(node_name)} is loaded but has not been processed yet. "
              + retry_soon_message)
          else {
            queue_for_loading(session, node_name)
            error(s"The origin ${session.origin(node_name)} was not loaded and has now been queued for loading. "
              + retry_soon_message)
          }
      }
    require_blob_loaded(session, node_name, snapshot)
    snapshot
  }

  /*Like require_loaded_origin_snapshot, but instead of failing for origins that are not
    loaded or not processed yet, load them if required and block until processing has
    finished or the timeout has expired. Returns the snapshot and whether processing has
    finished: a timeout is not an error, since a still unfinished origin is exactly how a
    non-terminating command shows up.*/
  def await_loaded_origin_snapshot(
    session: PIDE_MCP_Session,
    node_name: Document.Node.Name,
    timeout: Time
  ): (Document.Snapshot, Boolean) = {
    load_origin_if_required(session, node_name)
    // an auxiliary file that no theory includes will never be processed: stop waiting for it
    def give_up(snapshot: Document.Snapshot): Boolean =
      !node_name.is_theory && snapshot.commands_loading.isEmpty
    val (result, processed) = session.await_processed(node_name, timeout, give_up = give_up)
    val snapshot =
      result match {
        case Exn.Res(snapshot) => snapshot
        case Exn.Exn(_) =>
          error(s"The origin ${session.origin(node_name)} has no processed snapshot after waiting "
            + timeout.message + ". " + retry_soon_message)
      }
    require_blob_loaded(session, node_name, snapshot)
    (snapshot, processed)
  }

  /* waiting for processing to finish */

  def wait_until_processed_param(params: JSON.Object.T): Boolean =
    JSON.bool(params, "wait_until_processed").getOrElse(false)

  def wait_timeout_param(params: JSON.Object.T): Time =
    Time.seconds(JSON.double(params, "wait_timeout").getOrElse(
      PIDE_MCP_Tool_Schema.default_wait_timeout.seconds))

  /*Report of a bounded wait, to be prefixed to the payload of a tool response.
    timeout_details are only included if the wait did not succeed.*/
  def wait_json(
    processed: Boolean,
    waited: Time,
    timeout: Time,
    timeout_details: JSON.Object.T = JSON_Object()
  ): JSON.Object.T =
    JSON_Object("processed" -> processed, "waited_ms" -> waited.ms) ++
      (if (processed) JSON_Object()
       else JSON_Object(
         "wait_timed_out" -> true,
         "wait_note" -> ("Still not fully processed after waiting " + timeout.message
           + ": commands are unprocessed or still running. If commands are still running, "
           + "the proof method in question is likely non-terminating; consider replacing it. "
           + "Otherwise, retry with a larger wait_timeout.")) ++ timeout_details)

  def require_ordered_lines(
    opt_start_line: Option[Int],
    opt_end_line: Option[Int]
  ): Exn.Result[Unit] = Exn.capture {
    for (start_line <- opt_start_line; end_line <- opt_end_line if end_line < start_line)
      error(s"end_line $end_line < start_line $start_line")
  }

  def require_lines_in_bounds(
    opt_start_line: Option[Int],
    opt_end_line: Option[Int],
    total_lines: Int
  ): Exn.Result[Unit] = Exn.capture {
    for (start_line <- opt_start_line if start_line < 1 || start_line > total_lines)
      error(s"start_line $start_line out of bounds (file has $total_lines lines)")
    for (end_line <- opt_end_line if end_line < 1 || end_line > total_lines)
      error(s"end_line $end_line out of bounds (file has $total_lines lines)")
  }

  def require_valid_lines(
    opt_start_line: Option[Int],
    opt_end_line: Option[Int],
    total_lines: Int
  ): Exn.Result[Unit] = Exn.capture {
    Exn.release(require_ordered_lines(opt_start_line, opt_end_line))
    Exn.release(require_lines_in_bounds(opt_start_line, opt_end_line, total_lines))
  }

  def resolve_lines(
    opt_start_line: Option[Int],
    opt_end_line: Option[Int],
    total_lines: Int
  ): Exn.Result[(Int, Int)] =
    Exn.capture {
      Exn.release(require_valid_lines(opt_start_line, opt_end_line, total_lines))
      (opt_start_line.getOrElse(1), opt_end_line.getOrElse(total_lines))
    }
}
