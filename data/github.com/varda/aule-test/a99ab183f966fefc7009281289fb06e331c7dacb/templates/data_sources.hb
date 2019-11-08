{{#title}}Data sources{{/title}}

{{#if auth.rights.list_data_sources}}
  <ul class="nav nav-pills">
{{else}}
  {{#if auth.rights.add_data_source}}
    <ul class="nav nav-pills">
  {{/if}}
{{/if}}

{{#if auth.rights.list_data_sources}}
  <li class="{{#equals subpage 'list'}}{{#equals filter ''}}active{{/equals}}{{/equals}}">
    <a href="{{uri base 'data_sources'}}"><i class="fa fa-th"></i> All data sources</a>
  </li>
{{/if}}

{{#if auth.rights.list_data_sources}}
  <li class="{{#equals subpage 'list'}}{{#equals filter 'own'}}active{{/equals}}{{/equals}}">
    <a href="{{uri base 'data_sources' filter='own'}}"><i class="fa fa-th-large"></i> My data sources</a>
  </li>
{{else}}
  {{#if auth.rights.add_data_source}}
    <li class="{{#equals subpage 'list'}}{{#equals filter 'own'}}active{{/equals}}{{/equals}}">
      <a href="{{uri base 'data_sources' filter='own'}}"><i class="fa fa-th-large"></i> My data sources</a>
    </li>
  {{/if}}
{{/if}}

{{#if auth.rights.add_data_source}}
  <li class="pull-right{{#equals subpage 'add'}} active{{/equals}}">
    <a href="{{uri base 'data_sources_add'}}"><i class="fa fa-plus"></i> Add data source</a>
  </li>
{{/if}}

{{#if auth.rights.list_data_sources}}
  </ul>
{{else}}
  {{#if auth.rights.add_data_source}}
    </ul>
  {{/if}}
{{/if}}

{{> (lookup . 'subpage') }}


{{#*inline 'add'}}

<form action="{{uri base 'data_sources'}}" method="post">
  <fieldset>
    <label for="name">Data source name</label>
    <input type="text" class="input-xlarge" name="name" id="name">
    <label for="filetype">Filetype</label>
    <select class="input-medium" name="filetype" id="filetype">
      <option>vcf</option>
      <option>bed</option>
    </select>
    <label for="local_path">Path on server</label>
    <input type="text" class="input-xlarge" name="local_path">
    <div class="form-actions">
      <button type="submit" class="btn btn-success"><i class="fa fa-plus"></i> Add data source</button>
      <button type="reset" class="btn">Reset</button>
    </div>
  </fieldset>
</form>

{{/inline}}


{{#*inline 'list'}}

{{#if data_sources}}
  {{> pagination}}
  <table class="table table-hover">
    <thead><tr><th>Name</th><th>Filetype</th><th>Added</th></tr></thead>
    <tbody>
      {{#each data_sources}}
      <tr data-href="{{uri ../base 'data_sources' ./uri}}">
        <td>{{name}}</td>
        <td>{{filetype}}</td>
        <td>{{date added}}</td>
      </tr>
      {{/each}}
    </tbody>
  </table>
  {{> pagination}}
{{else}}
  <p>No data sources are here.</p>
{{/if}}

{{/inline}}
