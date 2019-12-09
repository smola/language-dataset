#!/usr/bin/env cwl-runner

class: CommandLineTool

cwlVersion: v1.0

baseCommand: []

requirements:
  - class: DockerRequirement
    dockerImageId: andrewjesaitis/snpeff

inputs:
  genome:
    type: string
    inputBinding:
      position: 1
  input_vcf:
    type: File
    inputBinding:
      position: 2
    doc: VCF file to annotate

outputs:
  output:
    type: stdout
  stats:
    type: File
    outputBinding:
      glob: "*.html"
  genes:
    type: File
    outputBinding:
      glob: "*.txt"

stdout: output.vcf
