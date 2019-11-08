note
	description: "Package definition."
	author: "Olivier Ligot"

class
	EPM_PACKAGE

create

	make

feature {NONE} -- Initialization

	make (a_name, a_version: STRING)
		do
			name := a_name
			version := a_version
			create dependencies.make_default
		ensure
			name_set: name = a_name
			version_set: version = a_version
		end

feature -- Access

	name: STRING
			-- Name

	version: STRING
			-- Version

	description: detachable STRING
			-- Description

	dependencies: DS_HASH_TABLE [EPM_PACKAGE_DEPENDENCY, STRING_32]

	environment_variable: detachable STRING
			-- Environment variable

feature -- Element change

	set_description (a_description: like description)
			-- Set `description' to `a_description'.
		do
			description := a_description
		ensure
			description_set: description = a_description
		end

	set_environment_variable (an_environment_variable: like environment_variable)
			-- Set `environment_variable' to `an_environment_variable'.
		do
			environment_variable := an_environment_variable
		ensure
			environment_variable_set: environment_variable = an_environment_variable
		end

end
