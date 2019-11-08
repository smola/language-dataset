<?LassoScript
//log_critical('loading knop_base from LassoApp')
/**!All Knop custom types should have this type as parent type. This is to be able to identify all registered knop types.
	*/
define knop_knoptype => type {

	data public isknoptype = true
}
	/**!
		Base data type for Knop framework. Contains common member tags. Used as boilerplate when creating the other types. \
							All member tags and instance variables in this type are available in the other knop types as well.
	*/
define knop_base => type {

	/*

	CHANGE NOTES
	2016-07-06	JS	Removed handling of custom error strings since it's not working
	2016-06-08	JS	Removed tagtime
	2013-01-31	JC	Major code cleanup of minor details.
					Removed all semicolons
					Changed local('xyz' to local(xyz
					Replaced += with -> append
					Changed old style if to brackets
					Replaced iterate with query expression
	2012-07-02	JC	Improved speed for method varname. Prev: ca 500 micros to run, now ca 55 micros. Note that varname does not work if the knop object is stored in a local
	2012-05-18	JC	Removed an old style colon syntax call
	2011-05-30	JC	Fixed bug in regards to calling error_msg
	2010-08-27	TT	complete Lasso9 syntax rewrite of all functions (not as hard as it sounds)
	2009-09-14	JS	Syntax adjustments for Lasso 9
	2009-09-04	JS	Changed $__html_reply__ to content_body
	2009-04-07	JS	->error_msg: custom error numbers can now be added, even if the language already exists.
	2008-01-10	JS	->error_msg: improved reporting of custom error messages such as from bad database queries
	2007-12-13	JS	Added -> error_lang to provide a reference to the knop_lang object for error messages, to be able to add localized error messages to any Knop type (except knop_lang and knop_base)
	2007-12-12	JS	Added -html and -xhtml to ->help to get a nicely formatted output.
	2007-12-11	JS	Centralized ->error_code and ->error_msg to knop_base. Moved all error codes to error_msg
	2007-12-06	JS	Changed ->help to improve the self-documentation. It will now always return an up to date list of member tags and parameter.
	2007-11-05	JS	Added var name to trace output
	2007-06-17	JS	Added ->tagtime (was in nav earlier)
	2007-06-13	JS	Added -> varname to be able to retreive the name of the page variable that a type instance is stored in.
	2007-06-13	JS	Added -> xhtml to automatically sense if an xhtml doctype exists in the current page buffer. The result is cached in a page variable for performance.
					This is for internal use for member tags that output html.
	2007-06-13	JS	Introduced page variable $_knop_data for general page level storage and caching, common between different knop objects.
	2007-06-13	JS	Created the data type

	TODO: ->help: add output option to format for Google Code Wiki
	->xhtml is not working properly when site is run by atbegin handler and explicitly writing to content_body


	*/

	data public version = '2016-07-06'
	data public debug_trace::array = array
	data public _debug_trace::array = array
	data public instance_unique = null
	data public instance_varname = null
	// data public tagtime::integer				// time for entire tag in ms
	data public tagtime_tagname::tag
	data public error_code = 0
	data public error_msg = string
	data public error_lang = null // must be defined as knop_lang in each type instead, to avoid recursion

/* is this needed anymore? Jolle 2011-01-26
	public ondeserialize =>{

		local(description = 'Recreates transient variables after coming back from a session')
		self -> properties -> first -> insert('_debug_trace' = array)
	}
*/

	/**!
	help
	Auto generates an overview of all member tags of a type, with all parameters specified for each member tag.
	*/
	public help(html::boolean = false, xhtml::boolean = false) => {

		local(description = 'Auto generates an overview of all member tags of a type, with all parameters specified for each member tag.')

//		log_critical('curr params: '+params)
		local(endslash = self->xhtml(params) ? ' /' | '')
		local(eol = (#html || #endslash == '') ? ('<br' + #endslash + '>\n') | '\n')

		local(output = string)
		local(tags = array)
		local(parameters = string)

		#output -> append((self -> type) + ' - version ' + (self -> 'version') + '\n' + (self -> 'description') + '\n\n')
		with listmethod in self->listmethods do {
			#tags->insert(#listmethod)
		}
		if(self -> parent -> type != null) => { // this doesn't work
			with listmethod in self -> parent -> listmethods do {
				#tags -> insert(#listmethod)
			}
		}

//		log_critical('found tags: '+#tags+' which is a '+#tags->type)
		#tags -> sort

		with tag in #tags do {
			#parameters = string
			#output -> append('-> ' + (#tag -> methodname))
			#description = (#tag -> paramdescs)

			with desc in #tag -> paramdescs do {
				if(#description !>> ('-' + #desc)) => {
					//#parameters += '-' + (#desc -> get(1)) + ' (' (#desc -> isrequired ? 'required' | 'optional')
//						+ (#desc -> get(2) != 'null' && #desc -> get(2) -> size ? ' ' + (#desc -> get(2)))  + ')\n'
					#parameters -> append('-' + (#desc -> get(1)) + ' (' + (#desc -> get(2) != 'null' && #desc -> get(2) -> size ? ' ' + (#desc -> get(2)))  + ')\n')
				}
			}
			#output -> append(#description -> size || #parameters -> size ? '\n' + #description)
			#output -> append(#description >> 'Parameters:' ?  '\n')
			#output -> append(#description !>> 'Parameters:' && #parameters -> size ? '\nParameters:\n')
			#output -> append(#parameters -> size ? #parameters)
			#output -> removetrailing('\n')
			#output -> append('\n\n')
		}

		if((local_defined('html') && #html != false) || (local_defined('xhtml') && #xhtml != false)) => {
			#output = encode_html(#output)
			// normalize line breaks and convert to <br>
			#output -> replace('\r\n', '\n') & replace('\r', '\n') & replace('\n', #eol + '\n')
		}
		return #output
	}

	/**!
	xhtml
	Internal. Finds out if xhtml output should be used. Looks at doctype unless -xhtml is specified \
			in the params array. The result is cached in a page variable. \n\
			Looking at doctype doesn\'t work when using atbegin driven solutions since content_body isn\'t filled with the page buffer until the page has already been processed.
	*/
	public xhtml(params = '') => {

		if(#params >> '-xhtml') => {
			local(xhtmlparam = #params -> find('-xhtml') -> first)

			if(#xhtmlparam -> type == 'pair') => {// -xhtml = true / -xhtml = false
				return boolean(#xhtmlparam -> value)
			else // plain -xhtml
				return true
			}
		//added else to bypass problems with content_body inside ajax called page
		else
			return false
		}

		local(rawcontent = web_response->rawContent)

		if(var('_knop_data') -> type != 'map') => {
			$_knop_data = map
		}
		if($_knop_data !>> 'doctype_xhtml') => {
			local(doctype = #rawcontent->substring(1, #rawcontent->find('>')))

			$_knop_data -> insert('doctype_xhtml' = (#doctype >> '<!DOCTYPE' && #doctype >> 'xhtml'))
		}
		return $_knop_data -> find('doctype_xhtml')
	}

	/**!
	error_lang
	Returns a reference to the language object used for error codes, to be able to add localized error messages to any Knop type (except knop_lang and knop_base)
	*/
	public error_lang() => {
		return .'error_lang'
	}

	/**!
	error_code
	Either proprietary error code or standard Lasso error code
	*/
	public error_code() => integer(self -> 'error_code')

	public error_msg(error_code::integer = -1) =>{
		#error_code < 0 ? #error_code = .error_code
		local(error_lang_custom = .error_lang)
		local(error_lang = knop_lang('en', true))

		local(errorcodes = map(
			0 = 'No error',
			-1728 = 'No records found', // standard Lasso error code

			// database errors 7000
			7001 ='The specified table was not found',
			7002 = 'Keyfield not specified',
			7003 = 'Lockfield not specified',
			7004 = 'User not specified for record lock',
			7005 = 'Either keyvalue or lockvalue must be specified for update or delete',
			7006 = 'Keyfield or keyvalue missing',
			7007 = 'Keyvalue missing',
			7008 = 'Keyvalue not unique',
			7009 = '-sql can not be used with FileMaker',
			7010 = 'Record locked by another user', // see error_data
			7011 = 'Record lock not valid any more',
			7012 = 'Could not set record lock', // see error_data
			7013 = 'Failed to clear record locks', // see error_data
			7016 = 'Add error', // see error_data
			7017 = 'Add failed, duplicate key value',
			7018 = 'Update error', // see error_data
			7019 = 'Delete error', // see error_data
			7020 = 'Keyfield not present in query',
			7021 = 'Lockfield not present in query',

			// form errors 7100
			7101 ='Form validation failed',
			7102 = 'Unsupported field type',
			7103 = 'Form->process requires that a database object is defined for the form',
			7104 = 'Copyfield must copy to a different field name',

			// grid errors 7200
			// lang errors 7300
			// nav errors 7400

			// user errors 7500
			7501 = 'Authentication failed',
			7502 = 'Username or password missing',
			7503 = 'Client fingerprint has changed'

			))
		#error_lang -> addlanguage(-language = 'en', -strings = #errorcodes)
		// add any custom error strings
	 	/*with custom_language in #error_lang_custom -> 'strings' do {
			if(#error_lang -> 'strings' !>> #custom_language -> name) => {
				// add entire language at once
				#error_lang -> addlanguage(-language = #custom_language -> name, -strings = #custom_language -> value)
			else
				// add one string at a time
				with custom_string in #custom_language -> value do {
					#error_lang -> insert(-language = #custom_language -> name,
						-key = #custom_string -> name,
						-value = #custom_string -> value)
				}
			}
		}*/

		if(#errorcodes >> #error_code) => {
			// return error message defined by this tag
			if(#error_lang -> keys >> #error_code) => {

				return #error_lang -> getstring(#error_code)
			else
				return #errorcodes -> find(#error_code)
			}
		else
			if((self -> 'error_msg') != '') => {
				// return literal error message
				return (self -> 'error_msg')
			else
				// test for error known by lasso
				error_code = #error_code
				// return Lasso error message
				return error_msg
			}
		}
	}

	public varname() => debug => {

		.'instance_unique' == null ? .'instance_unique' = knop_unique9

		local(thisvar = string)
		if(.'instance_varname' == null) => {
			// look for the var name and store it in instance variable
			with varname in vars -> keys do => {
				#thisvar = var(#varname)
				if(#thisvar -> type == .type
					&& (#thisvar -> 'instance_unique') == .'instance_unique') => {
					.'instance_varname' = string(#varname)
					return .'instance_varname'
				}
			}

		}

	} // END varname

/* removed by Jolle 2011-03-10
	public trace(html::boolean = false, xhtml::boolean = false) =>{
		local(description = 'Returns the debug trace for a type instance')

		local(endslash = (self ->xhtml(params) ? ' /' | ''))
		local(eol = ( local_defined('html') || #endslash -> size >0 ? ('<br' + #endslash + '>\n') | '\n'))
		local(trace= self -> 'debug_trace')
		self ->'_debug_trace'-> isa('array') ? #trace -> merge(self -> '_debug_trace')
		return(#eol + 'Debug trace for ' +self -> type+' $' +self -> varname+ #eol+#trace->join(#eol)+#eol)

	}


	public tagtime(html::boolean = false, xhtml::boolean = false) => {
		local(description = 'Returns the time it took to execute the last executed member tag for a type instance.')
		// Standard timer code
		//At beginning of tag code:
		//local: 'timer' = knop_timer

		//Before the end of tag code (before return):
		//self -> 'tagtime_tagname' = tag_name
		//self -> 'tagtime' = integer(#timer) // cast to integer to trigger onconvert and to "stop timer"

		local(endslash = (self ->xhtml(params) ? ' /' | ''))

		(#html || #xhtml) ? return(self -> type+ '->' + self -> 'tagtime_tagname' + ': ' + self -> 'tagtime' + ' ms<br' + #endslash + '>')
		return(self -> 'tagtime')
	}

	public tagtime(html::boolean = false, xhtml::boolean = false) => {
		local(description = 'Returns the time it took to execute the last executed member tag for a type instance.')
		local(endslash = (self ->xhtml(params) ? ' /' | ''))

		(#html || #xhtml) ? return(self -> type+ '->' + self -> 'tagtime_tagname' + ': ' + self -> 'tagtime' + ' ms<br' + #endslash + '>')
		return(self -> 'tagtime')
	}

*/
/*
Commented out in an effort to track why Lasso crashes. And since it's looks like it's not used
In dialog between Jolle and Tim 2011-03-10

    trait {
      import knop_trait_providesProperties
    }
*/

}

//log_critical('loading  knop_base done')

?>
