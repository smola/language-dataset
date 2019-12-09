Sourcefile -> COMMENTS:* EMPTY SECTION:+ PROJECTSECTION EMPTY:*                                   {% function ([comments, empty, sections, projectSection, empties]) { const result = {}; for (const section of sections) { if (section.title in result) { throw new Error("Duplicate section " + section.title + " found!") } result[section.title] = section.tasks }; result.projects = projectSection; return result  } %}

SECTION -> SECTIONHEADER TASK_OR_BLOCK:*                                                          {% d => ( {type: "section", title: d[0], tasks: d[1] } ) %}

SECTIONHEADER -> "# " ("later" | DATE) "\n"                                                       {% d => d[1][0] %}

TASK[prefix] -> $prefix MARKER " " CHECKBOX " " ID ": " TITLE TASKDESC[$prefix]:*                 {% ([prefix, marker, space1, checkbox, space, taskId, colon, title, taskdesc]) => ( {type: "task", title: title.text, tags: title.tags, id: taskId, status: checkbox, tentative: marker, description: taskdesc.length > 0 ? taskdesc.join("\n") : undefined} ) %}
MARKER -> ("-" | "~" )                                                                            {% d => d[0][0] === "~" %}
ID -> [a-zA-Z0-9_-]:+                                                                             {% d => d[0].join("") %}
CHECKBOX -> "[" CHECKBOX_STATE "]"                                                                {% ([leftparen, state, rightparen]) => state %}
CHECKBOX_STATE -> CHECKBOX_OPEN                                                                   {% ([status]) => status %}
                | CHECKBOX_CLOSED
                | CHECKBOX_CANCELED
                | CHECKBOX_IN_WORK
                | CHECKBOX_MERGED
CHECKBOX_OPEN -> " "                                                                              {% d => ({type: "OPEN"}) %}
CHECKBOX_CLOSED -> "X"                                                                            {% d => ({type: "CLOSED"}) %}
CHECKBOX_CANCELED -> "-"                                                                          {% d => ({type: "CANCELED"}) %}
CHECKBOX_IN_WORK -> "."                                                                           {% d => ({type: "IN_WORK"}) %}
CHECKBOX_MERGED -> ">" [^\]]:*                                                                    {% ([bracket, otherTask]) => ({type: "MERGED", otherTask: otherTask.join("")}) %}

TITLE -> .:+ TAG:* "\n"                                                                           {% d => ({ text: d[0].join(""), tags: d[1] }) %}
TASKDESC[prefix] -> $prefix "      " REST_OF_LINE                                                 {% ([prefix, space, content]) => content %}

TASK_OR_BLOCK -> (TASK["  "] | BLOCK)                                                             {% d => d[0][0] %}

BLOCK -> BLOCKHEADER BLOCKBODY:* TASK["    "]:*                                                   {% ([header, body, tasks]) => ({type: "work-unit", ...header, description: body.join("\n"), tasks: tasks }) %}
BLOCKHEADER -> "  " BLOCKID BLOCKDESCRIPTION "\n"                                                 {% ([space, id, desc]) => ({...desc, id: id}) %}
BLOCKID -> [a-zA-Z0-9_-]:+                                                                        {% d => d[0].join("") %}
BLOCKDESCRIPTION -> ": " BLOCKTITLE BLOCKVERSION:? TAG:*                                          {% ([colon, title, version, tags]) => ({title: title, version: version, tags: tags}) %}
BLOCKTITLE -> [^(\n]:+                                                                            {% d => d[0].join("") %}
BLOCKVERSION -> " ":? "(" [^)]:+ ")"                                                              {% ([space, leftparen, version, rightparen]) => version.join("") %}
BLOCKBODY -> BLOCKBODYLINE:* EMPTY                                                                {% d => d[0].join("\n") %}
BLOCKBODYLINE -> ("    " REST_OF_LINE | EMPTY)                                                    {% d => d[0] ? d[0][1] : "" %}

COMMENTS -> "//" REST_OF_LINE
EMPTY -> [ \t]:* "\n"                                                                             {% d => "" %}

TAG -> " " "@" [^ ]:+                                                                             {% ([space, at, content]) => content.join("") %}

PROJECTSECTION -> "# projects\n" PROJECTLIST:+                                                    {% ([header, projectList]) => projectList %}

PROJECTLIST -> PROJECT_TITLE_LINE DELIVERABLES:+                                                  {% ([titleLine, deliverables]) => ({type: "project", name: titleLine.name, contacts: titleLine.contacts, deliverables: deliverables}) %}

PROJECT_TITLE_LINE -> "  " NAME " (" CONTACTS ")\n"                                               {% d => ( { name: d[1] , contacts: d[3] } ) %}

NAME -> [^(\n)]:*                                                                                 {% d =>  d[0].join("") %}
CONTACTS -> ((CONTACTS "; " CONTACT ) | CONTACT)                                                  {% function (d) { if (d[0][0].email) { return d[0] } else { return d[0][0][0].concat(d[0][0][2]) } } %}
CONTACT -> "\"" [^"]:+ "\" <" [^>]:+ ">"                                                          {% ([quote, name, sep, email, sep2]) => ({name: name.join(""), email:email.join("")}) %}

DELIVERABLES -> "    - " DEL_DONE " " ID DEADLINE:? "\n" DELIVERABLE_DESC:* QUOTE:* LOG_ENTRY:*   {% ([prefix, checkbox, space, id, deadline, newline, description, quotes, logs]) => ({type: "deliverable", id: id, done: checkbox, description: description.join("\n"), quotes: quotes, logs: logs}) %}
DEL_DONE -> ("[ ]" | "[X]" | "[-]")                                                               {% d => d[0][0] === "[X]" ? "CLOSED" : d[0][0] === "[-]" ? "CANCELLED" : "OPEN" %}
DEADLINE -> " ":? "(" DATE ")"
DELIVERABLE_DESC -> ("      " REST_OF_LINE | EMPTY) EMPTY                                         {% d => d[0] ? d[0][1] : "" %}

DATE -> "20" [0-9] [0-9] "-" [0-1] [0-9] ("-" [0-3] [0-9]):?                                      {% d => d.map(x => Array.isArray(x) ? x.join("") : x).join("") %}

QUOTE -> CONTENTLINE:+ QUOTE_LOCATION EMPTY                                                       {% ([contents, location, empty]) => ({type: "quote", content: contents.join("\n"), location: location}) %}
CONTENTLINE -> "          > " REST_OF_LINE                                                        {% d => d[1] %}
QUOTE_LOCATION -> "            -- " REST_OF_LINE                                                  {% d => d[1] %}

LOG_ENTRY -> LOG_ENTRY_HEADER ":\n" LOG_ENTRY_BODY                                                {% ([header, newline, body]) => ({...header, type: "log", body: body}) %}
LOG_ENTRY_HEADER -> "          " DATE                                                             {% ([prefix, date]) => ({date: date}) %}
LOG_ENTRY_BODY -> LOG_ENTRY_BODY_LINE:+                                                           {% d => d[0].join("\n") %}
LOG_ENTRY_BODY_LINE -> (("            " REST_OF_LINE) | EMPTY)                                    {% d => typeof d[0][0] === "string" ? d[0][0] : d[0][0][1] %}

REST_OF_LINE -> [^\n]:* "\n"                                                                      {% d => d[0].join("") %}