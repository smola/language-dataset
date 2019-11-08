using util::AbstractMain
using util::Arg
using util::Opt

** Runs a BedSheet web application (Bed App) from the command line.
** 
** pre>
**   C:\> fan afBedSheet [-port <port>] [-env <env>] [-proxy] [-watchAllPods] <appModule>
** <pre
** 
** Where:
**   table:
** 
**   Option        Description
**   ------------  ----------------------------------------------------------------
**   env           (optional) The environment to start BedSheet in -> dev|test|prod
**   proxy         (optional) Starts a dev proxy on <port> and launches the real web app on (<port> + 1)
**   watchAllPods  (optional) Have the proxy monitor the timestamps of all pods, not just the direct dependencies of the application
**   port          (optional) The HTTP port to run the Bed App on. Defaults to 8069
**   appModule     The qname of the AppModule or pod which configures the BedSheet web app
** 
** Example:
** 
**   C:\> fan afBedSheet -port 8080 -env DEV -proxy acme::AppModule
** 
class Main : AbstractMain {

	@Opt { help="Starts a dev proxy on <port> and launches the real web app on (<port> + 1)" }
	private Bool proxy

	@Opt { help="Have the proxy monitor the timestamps of all pods, not just the direct dependencies of the application" }
	private Bool watchAllPods

	@Opt { help="The environment to start BedSheet in -> dev|test|prod" }
	private Str? env

	@Opt { help="The HTTP port to run the Bed App on"; aliases=["p"] } 
	private Int port := 8069

	@Arg { help="The qname of the AppModule or pod which configures the BedSheet web app" }
	private Str? appModule
	
	** Run baby, run!
	@NoDoc
	override Int run() {
		BedSheetBuilder(appModule, true).setOption(BsConstants.meta_watchAllPods, watchAllPods).startWisp(port, proxy, env)
	}
}
