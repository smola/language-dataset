uses gw.vark.annotations.*

var srcDir = file("src")
var distDir = file("dist")

/**
 * Packages up the gosu-git project source into a jar file.
 * The jar file will be built in the /dist sub-directory of the project.
 */
@Target
function jar() {
  Ant.mkdir(:dir = distDir)
  Ant.jar(:destfile = distDir.file("gosu-git.jar"),
          :basedir = srcDir)
}

/**
 * Deletes the jar file built by the jar target.
 */
@Target
function clean() {
  Ant.delete(:dir = distDir)
}

@Target
function deps() {
  Ivy.configure(:file = file("ivy-settings.xml"))
  Ivy.retrieve(:pattern = "[conf]/[artifact]-[revision](-[classifier]).[ext]", :log = "download-only")
}