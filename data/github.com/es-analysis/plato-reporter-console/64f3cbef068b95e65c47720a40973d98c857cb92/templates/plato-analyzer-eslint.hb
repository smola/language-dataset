
{{#ifnAll totalWarnings totalErrors mostWarnings.num mostErrors.num}}
All clear!  
{{else}}
{{#if totalWarnings}}
Total Warnings : {{ totalWarnings }}
{{/if}}
{{#if totalErrors}}
Total Errors : {{ totalErrors }}
{{/if}}
{{#if mostWarnings.num}}
Most Warnings : {{ mostWarnings.file }} ({{ mostWarnings.num }})
{{/if}}
{{#if mostErrors.num}}
Most Errors : {{ mostErrors.file }} ({{ mostErrors.num }})
{{/if}}
{{/ifnAll}}
{{!--
{ totalWarnings: 0,
  totalErrors: 2,
  mostErrors: { file: 'src/commands/batch/get-all.js', num: 1 },
  mostWarnings: { file: '', num: 0 } }
--}}