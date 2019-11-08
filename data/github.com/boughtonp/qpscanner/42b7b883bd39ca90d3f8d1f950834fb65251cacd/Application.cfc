<cfcomponent output="false" extends="framework">
	<cfsetting showdebugoutput=false />

	<cfset This.Name = "qpscanner_v0.8-rc@#CGI.SERVER_NAME#_#hash(getCurrentTemplatePath())#"/>
	<cfset This.SessionManagement = true/>

	<cfset Variables.Framework =
		{ DefaultSection   = 'start'
		, DefaultItem      = 'intro'
		, ReloadApplicationOnEveryRequest = true
		, CfcBase = ''
		}/>

	<!---
		FW/1's default behaviour fails when the directory has a dot in it,
		as is the case with GitHub's default download. Setting cfcbase to
		blank should fix this, but the cfcFilePath function assumes blank
		means webroot (instead of current directory), so that function has
		been overridden here in order to change the behaviour.
	--->
	<cffunction name="cfcFilePath" returntype="string" output=false access="private">
		<cfargument name="dottedPath" type="string" />
		<cfreturn len(dottedPath)
			? super.cfcFilePath(ArgumentCollection=Arguments)
			: ''
			/>
	</cffunction>


	<cffunction name="setupApplication" output="false">

		<cfset Application.Version = "0.8 (RC)"/>

		<cfset Application.Cfcs =
			{ Settings = new cfcs.settings( ConfigDirectory : expandPath('./config') )
			, Scanner  = createObject("component","cfcs.qpscanner")
			}/>
	</cffunction>


</cfcomponent>