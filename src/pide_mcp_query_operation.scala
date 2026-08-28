/*  Title:      PIDE_MCP/pide_mcp_query_operation.scala
    Author:     Maximilian Schäffeler

One-shot blocking adaptation of isabelle.Query_Operation
(Pure/PIDE/query_operation.scala): installs a temporary overlay running
operation_name + "_query" with query_args, then blocks until the prover reports
the result for our instance id. The overlay is always removed again, so the
document text is never modified.

Ported from schaeffm/copilots-isabelle (branch isabelle-agent, commit 194570f,
"feat(pide_mcp): add query operations").
*/

package isabelle.pide.mcp

import isabelle._

object PIDE_MCP_Query_Operation {
  /* rendering of prover output */

  // proof methods offered as sendback commands (sledgehammer)
  def sendback_commands(body: XML.Body): List[String] =
    body.flatMap {
      case XML.Elem(Markup(Markup.SENDBACK, _), b) => List(XML.content(b))
      case XML.Elem(_, b) => sendback_commands(b)
      case XML.Wrapped_Elem(_, _, b) => sendback_commands(b)
      case XML.Text(_) => Nil
    }

  // list items (find_theorems / find_consts)
  def collect_items(tree: XML.Tree): List[String] = tree match {
    case XML.Elem(Markup(Markup.ITEM, _), body) => List(XML.content(body))
    case XML.Elem(_, body) => body.flatMap(collect_items)
    case _ => Nil
  }
  def collect_items(body: XML.Body): List[String] = body.flatMap(collect_items)

  /* the line parameter, as a text range in the given snapshot */

  def line_param_range(snapshot: Document.Snapshot, params: JSON.Object.T): Text.Range = {
    val line = JSON.int(params, "line").getOrElse(error("Missing line parameter"))
    val doc = Line.Document(snapshot.node.source)
    Exn.release(PIDE_MCP_Tool_Util.require_valid_lines(Some(line), Some(line), doc.lines.length))
    PIDE_MCP_Util.text_range(doc, line, line).getOrElse(
      error("Cannot determine text range for line " + line))
  }

  /* one-shot query operation */

  def apply(
    mcp_session: PIDE_MCP_Session,
    snapshot: Document.Snapshot,
    operation_name: String,
    query_args: List[String],
    range: Text.Range,
    timeout: Time = Time.seconds(10.0),
    render: List[XML.Body] => JSON.T = bodies => cat_lines(bodies.map(XML.content))
  ): Exn.Result[JSON.T] = Exn.capture {
    val pide_session = mcp_session.session
    val node_name = snapshot.node_name
    if (mcp_session.is_base_session_theory(node_name))
      error("Cannot run " + operation_name + " directly in the already-built base session theory "
        + mcp_session.origin(node_name) + ". Run it from a dynamic theory that imports it instead.")
    val command =
      snapshot.node.command_iterator(range).map(_._1).nextOption()
        .getOrElse(error("No command found in range " + range))

    val instance = Document_ID.make().toString
    val overlay_args = instance :: query_args

    def set_overlays(overlays: Document.Node.Overlays): Unit =
      pide_session.update(Document.Blobs.empty, List(
        node_name -> Document.Node.Perspective(true, Text.Perspective.full, overlays)))

    val result = Synchronized[Option[Exn.Result[JSON.T]]](None)
    val update_pending = Synchronized(false)
    def check(): Unit = {
      val snap = pide_session.get_state().snapshot(node_name)
      if (snap.is_outdated) update_pending.change(_ => true)
      else {
        update_pending.change(_ => false)
        if (!snap.node.commands.contains(command))
          result.change(_ => Some(Exn.capture {
            error(operation_name + ": command removed from document")
          }))
        else {
          val messages =
            (for {
              case (_, XML.Elem(Markup(Markup.RESULT, Markup.Instance(`instance`)), List(XML.Elem(markup, body))))
                <- snap.command_results(command).iterator
            } yield markup.name -> body).toList
          if (messages.exists(_._1 == Markup.FINISHED))
            result.change(_ => Some(Exn.capture {
              messages.collect { case (Markup.ERROR, body) => XML.content(body) } match {
                case Nil => render(messages.collect { case (Markup.WRITELN, body) => body })
                case errors => error(cat_lines(errors))
              }
            }))
        }
      }
    }

    val consumer =
      Session.Consumer[Session.Commands_Changed](operation_name) { changed =>
        if (result.value.isEmpty && (update_pending.value || changed.commands.contains(command))) check()
      }

    pide_session.commands_changed += consumer
    try {
      set_overlays(Document.Node.Overlays.empty.insert(command, operation_name + "_query", overlay_args))
      val deadline = Time.now() + timeout
      Exn.release(
        result.timed_access[Exn.Result[JSON.T]](_ => Some(deadline), state => state.map((_, state)))
          .getOrElse(error(operation_name + " timed out after " + timeout.message)))
    }
    finally {
      pide_session.commands_changed -= consumer
      set_overlays(Document.Node.Overlays.empty)
    }
  }
}
