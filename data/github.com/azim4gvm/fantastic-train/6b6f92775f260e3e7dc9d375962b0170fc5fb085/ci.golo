## ci.golo 
function check = |context| {
  context: info("{0}", "=== Jarvis-CI ===")
  context: info("Repository {0}", context: repo(): name())
  context: info("Current branch {0}", context: repo(): branchName())
  let path = currentDir() + "/" + context: tmp_dir()
  context: info("path: {0}", path)
  # Stage: initialize
  context: info("{0}", "1- initialize")
  if context: sh("./npm_install.sh {0}", path): equals(0) {
    context: success("{0}", "packages installation OK")
    # Stage: tests
    context: info("{0}", "2- tests")
    if context: sh("./npm_run.sh {0} {1}", path, "test"):  equals(0) {
      context: success("{0}", "tests OK")
      return DynamicObject(): initialize("ok"): tests("ok"): status("success"): description("you are the best!"): context("jarvis-ci")
    } else {
      context: warning("{0}", "tests KO")
      return DynamicObject(): initialize("ok"): tests("ko"): status("failure"): description("ouch!"): context("jarvis-ci")
    }
  } else {
    context: fail("{0}", "packages installation KO")
    return DynamicObject(): initialize("ko"): tests("ko"): status("failure"): description("ouch!"): context("jarvis-ci")
  }

}