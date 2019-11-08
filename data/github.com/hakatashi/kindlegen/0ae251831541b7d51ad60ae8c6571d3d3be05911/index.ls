require! {
  child_process: {spawn}
  path
  temp
  fs
}

module.exports = (epub, callback) ->
  temp.track!

  error, temp-dir <- temp.mkdir \node-kindlegen
  return callback error if error

  input-path = path.join temp-dir, 'input.epub'
  output-path = path.join temp-dir, 'output.mobi'

  error, written, string <- fs.write-file input-path, epub
  return callback error if error

  kindlegen = spawn do
    path.resolve __dirname, 'bin/kindlegen'
    <[input.epub -c2 -verbose -o output.mobi]>
    {cwd: temp-dir, env: {}}

  code <- kindlegen.on \close
  return callback new Error "kindlegen returned error #{code}" if code not in [0 1]

  error, mobi <- fs.read-file output-path
  return callback error if error

  callback null, mobi
