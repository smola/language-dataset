// History:
//   11 8 12 Creation

using camembert
using netColarUtils
using camFantomPlugin

**
** AxonPlugin
**
const class AxonPlugin : Plugin
{
  const AxonDocs docProv := AxonDocs()

  const Unsafe actors := Unsafe(AxonActors())

  override PluginCommands? commands() {null} // no build / run commands

  override PluginDocs? docProvider() {docProv}

  override Type? envType() {return null}

  override Void onInit(File configDir)
  {
    // create axon template if not there yet
    axon := configDir + `templates/axon_function.json`
    if( ! axon.exists)
      JsonUtils.save(axon.out, Template{it.name="Axon function"
        it.extensions=["axon"]
        it.text="/*\nHistory: {date} {user} Creation\n*/\n\n() => do\n  //TODO\nend\n"})
  }

  override Void onFrameReady(Frame frame, Bool initial := true)
  {
    plugins := (frame.menuBar as MenuBar).plugins
    plugins.remove(plugins.children.find{it->text == "Axon"})
    plugins.add(AxonMenu(frame))
  }

  override const |Uri -> Project?| projectFinder:= |Uri uri -> Project?|
  {
    f := uri.toFile
    if(f.isDir && (f + `_axon_conn.props`).exists)
    {
      return Project{
        it.dis = f.name
        it.dir = f.uri
        it.icon = AxonSpace.funcIcon
        it.plugin = this.typeof.pod.name
      }
    }
    return null
  }

  override Space createSpace(Project prj)
  {
    return AxonSpace(Sys.cur.frame, prj.dir.toFile, (prj.dir + `_axon_conn.props`).toFile)
  }

  override Int spacePriority(Project prj)
  {
    if(prj.plugin != this.typeof.pod.name)
      return 0

    return 75
  }

  override Void onShutdown(Bool isKill := false)
  {
    AxonActors act := actors.val
    act.actors.vals.each |a| {a.pool.stop}
  }

  ** Called via Dynamic call
  Str:TrioInfo trioData(File[] podDirs)
  {
    return AxonIndexer().trioData(podDirs)
  }
}