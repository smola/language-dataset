#!/usr/bin/env cwl-runner

class: CommandLineTool
id: bwa-index-0.7.4--ha92aebf_0
label: bwa-index-0.7.4--ha92aebf_0
cwlVersion: v1.0

$namespaces:
  edam: 'http://edamontology.org/'

hints:
  - class: DockerRequirement
    dockerPull: 'quay.io/biocontainers/bwa:0.7.4--ha92aebf_0'

baseCommand: [ bwa, index ]

inputs:
  - id: fa
    type: File
    format: edam:format_1929
    inputBinding:
      position: 2
    doc: FastA file for reference genome

outputs:
  - id: amb
    type: File
    outputBinding:
      glob: $(inputs.fa.basename).amb
  - id: ann
    type: File
    outputBinding:
      glob: $(inputs.fa.basename).ann
  - id: bwt
    type: File
    outputBinding:
      glob: $(inputs.fa.basename).bwt
  - id: pac
    type: File
    outputBinding:
      glob: $(inputs.fa.basename).pac
  - id: sa
    type: File
    outputBinding:
      glob: $(inputs.fa.basename).sa
      
arguments:
  - position: 1
    prefix: -p
    valueFrom: $(inputs.fa.basename)
