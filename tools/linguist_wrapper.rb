#!/usr/bin/env ruby

require 'json'
require 'linguist'

res = {}
ARGV.each do |path|
  path = ARGV[0]
  blob = Linguist::FileBlob.new(path)
  res[path] = {}

  l = Linguist.detect(blob)
  if l.nil?
    res[path]['all'] = []
  else
    res[path]['all'] = [l]
  end

  res[path]['modeline'] = Linguist::Strategy::Modeline.call(blob, [])
  res[path]['filename'] = Linguist::Strategy::Filename.call(blob, [])
  res[path]['shebang'] = Linguist::Shebang.call(blob, [])
  res[path]['extension'] = Linguist::Strategy::Extension.call(blob, [])
  res[path]['xml'] = Linguist::Strategy::XML.call(blob, [])
  res[path]['manpage'] = Linguist::Strategy::Manpage.call(blob, [])
  res[path]['heuristics'] = Linguist::Heuristics.call(blob, res[path]['extension'])
  #begin
  #    res[path]['classifier-ext'] = Linguist::Classifier.call(blob, res['extension'])
  #rescue
  #    #TODO: https://github.com/github/linguist/issues/4995
  #end
end
puts JSON.generate(res)
