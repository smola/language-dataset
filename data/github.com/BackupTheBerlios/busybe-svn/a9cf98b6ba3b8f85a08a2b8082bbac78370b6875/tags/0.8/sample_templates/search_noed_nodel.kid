<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xmlns:py="http://purl.org/kid/ns#"
    py:extends="'master.kid'">

<head>
    <meta content="text/html; charset=UTF-8" http-equiv="content-type" />
	<title> ${title} Search [8layer Technologies] </title>
	<script language="JavaScript">
	<!-- Begin
	var checkflag = "false";
	function check(field) {
		if (checkflag == "false") {
			for (i = 0; i < field.length; i++) {
				field[i].checked = true;}
			field.checked = true;
			checkflag = "true";
			return 0; }
		else {
			for (i = 0; i < field.length; i++) {
				field[i].checked = false; }
			field.checked = false;
			checkflag = "false";
			return 0; }
	}
	//  End --></script>

	<style>
	<!--

	table.edit {
	}
	table.edit th, table.edit td {
		margin: 0px 0px 0px 0px;
		padding: 4px 6px 4px 6px;
	}
	table.edit tbody th {
		background-color: #030;
		color: #f0fff0;
		font-size: smaller;
		text-align: right;
	}
	table.edit tfoot th {
		background-color: #9a9;
		color: #efe;
	}
	table.edit tfoot th span#newlink {
		float: left;
	}
	table.edit tfoot th span#rowcount {
		float: right;
	}
	table.edit tbody {
	}
	table.edit tbody td {
		background-color: #d9e0d9;
		color: #000;
	}
	table.edit tbody {
	}

	table.list {
	}
	table.list th, table.list td {
		margin: 0px 0px 0px 0px;
		padding: 4px 6px 4px 6px;
	}
	table.list th a {
		color: #efe;
	}
	table.list thead th {
		background-color: #030;
		color: #efe;
	}
	table.list tfoot th {
		background-color: #9a9;
		color: #efe;
	}
	table.list tfoot th span#newlink {
		float: left;
		margin-right: 8px;
	}
	table.list tfoot th span#rowcount {
		float: right;
		margin-left: 8px;
	}
	table.list tbody {
	}
	table.list tbody td {
		background-color: #d9e0d9;
		color: #000;
	}
	table.list tbody {
	}

	-->
	</style>
	<script src="/static/date_chooser/date-functions.js" type="text/javascript"></script>
	<script src="/static/date_chooser/datechooser.js" type="text/javascript"></script>
	<link rel="stylesheet" type="text/css" href="/static/date_chooser/datechooser.css" />
</head>

<body>
<?python
import re
import datetime
Now = datetime.datetime.now()
def beautify(text):
	'''Capitalize the first letter of each word.
	'''
	words = (word.capitalize() for word in text.split('_'))
	text = ' '.join(words)
	return text
?>

<div>
<h2>${title} Search</h2>
<!--form action="save?page=${page}" method="post"-->
<form action="" method="post">
<!--input type="hidden" name="page" value="${page}" /-->
<table class="edit">
	<tbody py:for="detail in searches">
	<?python
	col_type = False
	if fields[detail].has_key('type'):
		col_type = fields[detail]['type']
	value = False
	if fields[detail].has_key('value'):
		value = fields[detail]['value']
	column = False
	if fields[detail].has_key('column'):
		column = fields[detail]['column']
	options = False
	if fields[detail].has_key('options'):
		options = fields[detail]['options']
	add = False
	if fields[detail].has_key('add'):
		add = fields[detail]['add']
	if fields[detail].has_key('search'):
		search = fields[detail]['search']
	description = fields[detail].get('description', beautify(detail))
	label = description
	if fields[detail].has_key('label'):
		column = fields[detail]['label']
	?>
	<tr py:if="not col_type">
		<th>
			${description}
		</th>
		<td>
			<input id="${detail}" name="${detail}"
			value="${value}" />
		</td>
	</tr>

	<tr py:if="col_type == 'select'">
		<th>
			${description}
		</th>
		<td>
			<select id="${detail}" name="${detail}">
				<option py:if="new==True or not value"
				value=""></option>
				<option py:if="new==False and value"
				value="${value.id}" selected="selected">
				<?python
				if hasattr(value, column):
					option_value = getattr(value, column)
				else:
					option_value = 'ERROR! attribute "%s" not found' % column
				?>
				${option_value}
				</option>
				<option py:for="option in options"
				py:if="option != value or new==True"
				value="${option.id}">
				<?python
				if hasattr(option, column):
					option_value = getattr(option, column)
				else:
					option_value = 'ERROR! attribute "%s" not found' % column
				?>
				${option_value}
				</option>
			</select>
		</td>
	</tr>

	<tr py:if="col_type == 'foreign_text'">
		<th>
			${description}
		</th>
		<td>
			<input id="${detail}" name="${detail}"
			value="${value}" />
			<small py:if="search">
				<a href="${search}" target="_blank">
					Search ${label}
				</a>
			</small>
		</td>
	</tr>

	<tr py:if="col_type == 'bool'">
		<th>
			${description}
		</th>
		<td>
			<select id="${detail}" name="${detail}">
				<?python
				if new==False and value:
					value = value
				else:
					value = False
				?>
				<option value="${value}" selected="selected">
				${fields[detail][value]}</option>
				<option value="${option}" py:for="option in (True, False)"
				py:if="option != value">
				${fields[detail][option]}</option>
			</select>
		</td>
	</tr>

	<tr py:if="col_type == 'int'">
		<th>
			${description}
		</th>
		<td>
			<input id="${detail}_first" name="${detail}_first"
			value="${value}" size="8" /> -
			<input id="${detail}_last" name="${detail}_last"
			value="${value}" size="8" />
			<small>Integer (1, 2, 3, ...)</small>
		</td>
	</tr>

	<tr py:if="col_type == 'float'">
		<th>
			${description} (range)
		</th>
		<td>
			<input id="${detail}_first" name="${detail}_first"
			value="${value}" size="8" /> -
			<input id="${detail}_last" name="${detail}_last"
			value="${value}" size="8" />
			<small>Float (1.0, 0.5, -3.9, 6, ...)</small>
		</td>
	</tr>

	<!--tr py:if="col_type == 'text'">
		<th>
			${description}
		</th>
		<td>
			<?python
			text_rows = 6
			text_cols = 32
			if fields[detail].has_key('cols'):
				text_cols = fields[detail]['cols']
			if fields[detail].has_key('rows'):
				text_rows = fields[detail]['rows']
			?>
			<textarea name="${detail}" id="${detail}"
				rows="${text_rows}" cols="${text_cols}">${value}</textarea>
		</td>
	</tr-->

	<tr py:if="col_type == 'hidden'">
		<th>
			${description}
		</th>
		<td>
			${value}
			<input name="${detail}" id="${detail}"
			value="${value}" />
		</td>
	</tr>

	<tr py:if="col_type == dict">
		<th>
			${description}
		</th>
		<td>
			<select name="${detail}" id="${detail}">
				<option py:if="new==True or not value"
				value=""></option>
				<option py:if="new==False and value"
				value="${value}" selected="selected">
				${options[value]}
				</option>
				<option py:for="option in options.keys()"
				py:if="option != value"
				value="${option}">
				${options[option]}
				</option>
			</select>
			<!--small py:if="add">
				<a href="${add}" target="_blank">
					Add ${description}
				</a>
			</small-->
		</td>
	</tr>

	<tr py:if="col_type == list">
		<th>
			${description}
		</th>
		<td>
			<select name="${detail}" id="${detail}">
				<option py:if="new==True or not value"
				value=""></option>
				<option py:if="new==False and value"
				value="${value}" selected="selected">
				${value}
				</option>
				<option py:for="option in options"
				py:if="option != value"
				value="${option}">
				${option}
				</option>
			</select>
			<!--small py:if="add">
				<a href="${add}" target="_blank">
					Add ${description}
				</a>
			</small-->
		</td>
	</tr>

	<tr py:if="col_type == 'date'">
		<?python
		current_date = Now.strftime('%Y-%m-%d')
		if not value:
			#value = current_date
			value = ''
		?>
		<th>
			${description}
		</th>
		<td>
			<input name="${detail}_first" id="${detail}_first"
			value="${value}" size="10" />
			<img src="/static/date_chooser/calendar.gif"
			onclick="showChooser(this, '${detail}_first', '${detail}_first_chooser', 1950, 2010, 'Y-m-d', false);" />
			<div id="${detail}_first_chooser" class="dateChooser select-free"
			   	style="display: none; visibility: hidden; width: 160px;">
			</div>
			-
			<input name="${detail}_last" id="${detail}_last"
			value="${current_date}" size="10" />
			<img src="/static/date_chooser/calendar.gif"
			onclick="showChooser(this, '${detail}_last', '${detail}_last_chooser', 1950, 2010, 'Y-m-d', false);" />
			<div id="${detail}_last_chooser" class="dateChooser select-free"
			   	style="display: none; visibility: hidden; width: 160px;">
			</div>
			<small>Date: ${Now.strftime('%Y-%m-%d')}</small>
		</td>
	</tr>

	<tr py:if="col_type == 'time'">
		<?python
		current_time = Now.strftime('%H:%M:%S')
		if not value:
			value = current_time
		?>
		<th>
			${description}
		</th>
		<td>
			<input name="${detail}_first" id="${detail}_first"
			value="${value}" size="10" /> -
			<input name="${detail}_last" id="${detail}_last"
			value="${value}" size="10" />
			<small>Time: ${Now.strftime('%H:%M:%S')}</small>
		</td>
	</tr>

	</tbody>
</table>
<input type="submit" id="search" name="search" value="Search" style="float: left;"/>
<input type="reset" id="reset" name="reset" value="Clear" style="float: left;"/>
</form>
<form action="" method="post">
	<input type="submit" id="search" name="search" value="Show All"/>
</form>
<br />

<?python
col_cnt = len(columns)
page_last = int(max_row/show)
if page_last != max_row*1.0/show:
	page_last += 1
?>

<span>
<a py:if="page != 1" title="First page"
	href="?page=1">&lt;&lt;First</a>
<a py:if="page != 1" title="Previous page"
	href="?page=${page-1}">&lt;Previous</a>
<a py:if="max_row > page*show" title="Next page"
	href="?page=${page+1}">Next&gt;</a>
<a py:if="max_row > page*show" title="Last page"
	href="?page=${page_last}">Last&gt;&gt;</a>
</span>

<form action="mass_update">
<table class="list">
	<thead><tr>
		<th py:for="column in columns">
		<a href="?sort_page_by=${column}&amp;page=${page}"
			>${fields[column].get('description', beautify(column))}</a>
		</th>
	</tr></thead>

	<tfoot><tr>
		<th colspan="${col_cnt}" style="text-align: right;">
			<span id="newlink">
				<a href="new">new item</a>
<a py:if="page != 1" title="First page"
	href="?page=1">&lt;&lt;First</a>
<a py:if="page != 1" title="Previous page"
	href="?page=${page-1}">&lt;Previous</a>
<a py:if="max_row > page*show" title="Next page"
	href="?page=${page+1}">Next&gt;</a>
<a py:if="max_row > page*show" title="Last page"
	href="?page=${page_last}">Last&gt;&gt;</a>
			</span>
			<span id="rowcount">${row_cnt} item(s)</span>
		</th>
	</tr></tfoot>

	<tbody py:for="row in rows">
		<tr>
			<td py:for="field in columns"
				><?python
				if not fields[field].has_key('type'):
					value = getattr(row, field)
				elif fields[field]['type'] == 'select':
					option = getattr(row, field)
					column = fields[field]['column']
					if hasattr(option, column):
						option_value = getattr(option, column)
					else:
						option_value = ''
					value = option_value
				elif fields[field]['type'] == 'foreign_text':
					option = getattr(row, field)
					column = fields[field]['column']
					if hasattr(option, column):
						option_value = getattr(option, column)
					else:
						#option_value = 'ERROR! attribute "%s" not found in %s' % (column, option)
						option_value = ''
					value = option_value
				elif fields[field]['type'] == 'bool':
					value = fields[field][getattr(row, field)]
				elif fields[field]['type'] == 'time':
					value = str(getattr(row, field))
					value = re.sub('.* ', '', value)
				else:
					value = getattr(row, field)
				?>${value}</td>
		</tr>
		<tr py:if="not details == columns">
		<td colspan="${col_cnt}" class="details">
			<span py:for="field in details" py:if="getattr(row, field) and not field in columns">
				<?python
				col_type = False
				if fields[field].has_key('type'):
					col_type = fields[field]['type']
				?>
				<span py:if="col_type == 'select'">
					<strong>${fields[field].get('description', beautify(field))}:</strong> ${getattr(getattr(row, field), fields[field]['column'])} 
				</span>
				<span py:if="col_type == 'foreign_text'">
					<strong>${fields[field].get('description', beautify(field))}:</strong> ${getattr(getattr(row, field), fields[field]['column'])} 
				</span>
				<span py:if="col_type == 'bool'">
					<strong>${fields[field].get('description', beautify(field))}:</strong> ${fields[field][getattr(row, field)]} 
				</span>
				<span py:if="col_type not in ('select', 'text', 'password', 'foreign_text')">
					<strong>${fields[field].get('description', beautify(field))}:</strong> ${getattr(row, field)} 
				</span>
				<div py:if="col_type == 'text'">
					<strong>${fields[field].get('description', beautify(field))}:</strong> ${getattr(row, field)} 
				</div>
			</span>
		</td>
		</tr>

		<tr py:for="entry in entry_details">
		<td class="details">
			<strong>${entry['label']}</strong>
		</td>
		<td colspan="${col_cnt-1}" class="details">
			<?python
			id = row
			for column in entry['id']:
				id = getattr(id, column, id)
			?>
			${entry['details'].get(id)}
		</td>
		</tr>

		<tr py:for="item_detail in item_details">
			<?python
			results = getattr(row, item_detail['join_on'], [])
			item_dict = {}
			for result in results:
				item_tuple = []
				item_code = (getattr(result, item_detail['id_col'], ''))
				for column_detail in item_detail['tuple']:
					cols = column_detail.split('.')
					value = result
					for col in cols:
						value = getattr(value, col, '')
					item_tuple.append(value)
				item_dict[item_code] = item_detail['fstring'] % tuple(item_tuple)
			?>
			<td>
				${item_detail['label']} Details:
			</td>
			<td colspan="${col_cnt-1}">
				<div py:for="k,v in item_dict.items()">
					<b>${k}</b>: ${v}
				</div>
			</td>
		</tr>

	</tbody>

</table>
</form>

</div>

</body>
</html>
