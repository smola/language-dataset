<!DOCTYPE html>
<html>
  <head>
    <meta http-equiv="Content-Type" content="text/html;charset=UTF-8"/>
    <meta name="viewport" content="width=device-width, internal-scale=1.0">
    <title>Fragments | Blog</title>

    <link rel="shortcut icon" href="/static/img/favicon.ico">

    <!-- Go to http://www.bootstrapcdn.com/#bootswatchundefined for more themes -->
    <link href="/static/css/bootstrap.min.css" rel="stylesheet">
    <style>
    a{color: #111111;}
    a:hover{color: #444444;}
    a:active{color: #444444;}
    </style>
  </head>

  <body style="padding-top: 70px">
    <nav class="navbar navbar-default navbar-fixed-top">
      <div class="container">
        <div class="navbar-header">
          <a class="navbar-brand" href="/">Fragments</a>
          <button class="navbar-toggle" type="button" data-toggle="collapse" data-target="#navbar-main">
            <span class="icon-bar"></span>
            <span class="icon-bar"></span>
            <span class="icon-bar"></span>
          </button>
        </div>
        <div class="navbar-collapse collapse" id="navbar-main" role="navigation">
          <ul class="navbar-nav nav">
            <li class="active"><a href="/">Home</a></li>
          </ul>
          <p class="navbar-text vcenter">small, unrelated items. sometimes broken</p>
        </div>
      </div>
    </nav>

    <!-- main -->
    <div class="container">
      <div class="col-md-9 middle">
        <h2><a href="/{{ entry.slug }}/">{{ entry.title }}</a></h2>
        <h6>posted {{ entry.date }} by <a href="/about/">Ben Cordero</a></h6>
        {{{ entry.html }}}
      </div>

      <div class="col-md-3 right">
        <h3>Recent posts</h3>
        <ul class="list-unstyled">
        {{#each recent_entries }}
          <li>
            <a href="/{{ this.slug }}/">{{ this.title }}</a>
          </li>
        {{/each}}
        </ul>
      </div>
    </div>
    <!-- endmain -->

    <div class="footer">
      <p>&copy; Copyright 2016 Ben Cordero.</p>
    </div>

    <script type="text/javascript" src="/static/js/jquery.min.js"></script>
    <script type="text/javascript" src="/static/js/bootstrap.min.js"></script>
  </body>
</html>
