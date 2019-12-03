<!doctype html>
<html lang="en">
<head>
	<meta charset="UTF-8">
	<meta name="viewport" content="width=devicewidth, initial-scale=1, user-scalable=no">
	<title>{{site.title}}</title>
	<link rel="stylesheet" href="{{site.baseUrl}}/icons.css">
	<link rel="stylesheet" href="{{site.baseUrl}}/style.css">
	<link rel="alternate" type="application/atom+xml" title="CaelUX" href="{{site.baseUrl}}/feed">
</head>
<body>
	<header>
		<span class="header-content">
			<h1 class="header-title">{{site.title}}</h1>
			<a class="header-option home-link" href='{{site.baseUrl}}/'>
				<span class="header-option-alt">Ir para a página principal</span>
			</a>
			<a class="header-option feed-link" href='{{site.baseUrl}}/feed'>
				<span class="header-option-alt">Assine o feed</span>
			</a>
		</span>
	</header>
	<main>
		{{{content}}}
	</main>
</body>
</html>
