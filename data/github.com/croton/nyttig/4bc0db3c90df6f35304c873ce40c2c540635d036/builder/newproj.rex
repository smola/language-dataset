/* newproj -- Create a new JS project
   Created 06-20-2017
*/
parse arg input
if abbrev('-?', input) then call help

call install input

exit

/* Install basic components */
install: procedure
  parse arg name tasks
  refdir=value('cjp',,'ENVIRONMENT')
  snips=refdir'\snips'
  pkg='package.json'
  tasker='Gruntfile.js'
  main='index.html'
  if stream(pkg,'c','query exists')<>'' then pkg='package-'name'.json'
  say 'Creating' pkg '...'
  'call merge' snips'\pkgjson.tmpl' name '>>' pkg
  'call merge' snips'\bootstrap.tmpl' name '>>' main
  call charout , 'Press ENTER to continue'; pull .
  say 'Installing project dependencies ...'
  'call npmx install'
  say 'Creating gruntfile ...'
  'call merge' snips'\gruntfile.tmpl' tasks '>>' tasker
  'call grunt' -- Run default task
  return

help:
say 'newproj - Create a new JS project'
say 'usage: newproj name [tasks]'
exit
