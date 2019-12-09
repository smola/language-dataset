#!/usr/bin/env cwl-runner

cwlVersion: v1.0
class: CommandLineTool
baseCommand: grep
stdout: results.txt
inputs:
  extended:
    type: boolean?
    inputBinding:
      position: 1
      prefix: -e
    label: allows for use of regex in search_string
  search_string:
    type: string
    inputBinding:
      position: 2
    label: the string or expression to search for
  search_file:
    type: File
    streamable: true
    inputBinding:
      position: 3
    label: the file to look occurences of the search_string for.
outputs:
  occurences:
    type: stdout
    label: all the occurences of the search_string in the search_file.



    


