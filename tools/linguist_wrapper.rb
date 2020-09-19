#!/usr/bin/env ruby

require 'json'
require 'linguist'

path = ARGV[0]

blob = Linguist::FileBlob.new(path)

res = {}

res['modeline'] = Linguist::Strategy::Modeline.call(blob, [])
res['filename'] = Linguist::Strategy::Filename.call(blob, [])
res['shebang'] = Linguist::Shebang.call(blob, [])
res['extension'] = Linguist::Strategy::Extension.call(blob, [])
res['xml'] = Linguist::Strategy::XML.call(blob, [])
res['manpage'] = Linguist::Strategy::Manpage.call(blob, [])
res['heuristics'] = Linguist::Heuristics.call(blob, res['extension'])
#begin
#    res['classifier-ext'] = Linguist::Classifier.call(blob, res['extension'])
#rescue
#    #TODO: https://github.com/github/linguist/issues/4995
#end

puts JSON.generate(res)
