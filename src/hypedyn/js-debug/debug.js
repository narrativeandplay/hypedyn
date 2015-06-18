/* debugging: sends debugging info to the editor */

function send_debug(event_type, event_data) {
  jQuery.get("./debug.scm", { event_type: event_type, event_data: event_data });
}

