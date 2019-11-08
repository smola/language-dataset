note
	description: "Summary description for {APP_ACCOUNT_VERIFY_CREDENTIAL}."
	date: "$Date$"
	revision: "$Revision$"

class
	APP_ACCOUNT_VERIFY_CREDENTIAL

inherit
	APP_REQUEST_HANDLER
		redefine
			initialize,
			execute_unauthorized
		end

create
	make

feature {NONE} -- Initialization

	make (a_path: STRING)
		do
			path := a_path
			description := "Verify credentials"
			initialize
		end

	initialize
		do
			Precursor
			enable_request_method_get
			enable_format_json
			enable_format_xml
			enable_format_text
		end

feature -- Access

	authentication_required: BOOLEAN = True

feature -- Execution

	execute_unauthorized (ctx: REST_REQUEST_CONTEXT; a_format: detachable STRING; a_args: detachable STRING)
		local
			h: HTTPD_HEADER
		do
			create h.make
			h.put_status ({HTTP_STATUS_CODE}.unauthorized)
			h.put_header ("WWW-Authenticate: Basic realm=%"My Silly demo auth, password must be the same as login such as foo:foo%"")
			ctx.output.put_string (h.string)
			h.recycle
		end

	execute_application (ctx: REST_REQUEST_CONTEXT; a_format: detachable STRING; a_args: detachable STRING)
		local
			l_full: BOOLEAN
			rep: detachable REST_RESPONSE
			l_login: STRING_8
			s: STRING
		do
			if ctx.authenticated then
				l_full := attached ctx.variables_get.variable ("details") as v and then v.is_case_insensitive_equal ("true")
				if attached ctx.authenticated_identifier as log then
					l_login := log.as_string_8
					create rep.make (path)

					create s.make_empty
					inspect format_id (a_format)
					when {REST_FORMAT_CONSTANTS}.json then
						rep.headers.put_content_type_text_plain
						s.append_string ("{ %"login%": %"" + l_login + "%" }%N")
					when {REST_FORMAT_CONSTANTS}.xml then
						rep.headers.put_content_type_text_xml
						s.append_string ("<login>" + l_login + "</login>%N")
					when {REST_FORMAT_CONSTANTS}.text then -- Default
						rep.headers.put_content_type_text_plain
						s.append_string ("login: " + l_login + "%N")
					else
					end
					if not s.is_empty then
						rep.set_message (s)
						ctx.output.put_string (rep.string)
					end
					rep.recycle
				else
					process_error (ctx, "User/password unknown", a_format)
				end
			else
				process_error (ctx, "Authentication rejected", a_format)
			end
		end

note
	copyright: "Copyright (c) 1984-2011, Eiffel Software and others"
	license: "Eiffel Forum License v2 (see http://www.eiffel.com/licensing/forum.txt)"
	source: "[
			Eiffel Software
			5949 Hollister Ave., Goleta, CA 93117 USA
			Telephone 805-685-1006, Fax 805-685-6869
			Website http://www.eiffel.com
			Customer support http://support.eiffel.com
		]"
end
