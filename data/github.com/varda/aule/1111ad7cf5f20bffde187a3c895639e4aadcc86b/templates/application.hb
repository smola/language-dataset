<div class="navbar navbar-fixed-top">
  <div class="navbar-inner">
    <div class="container">
      <a class="btn btn-navbar" data-toggle="collapse" data-target=".nav-collapse">
        <span class="fa fa-bar fa-lg"></span>
      </a>
      <a class="brand" href="{{uri base}}">Aulë</a>
      <ul class="nav">
        <li><a href="https://github.com/varda/aule">Homepage</a></li>
        <li><a href="https://github.com/varda/varda">Varda</a></li>
        <li><a href="https://github.com/varda/manwe">Manwë</a></li>
      </ul>
      <p id="waiting" class="nav muted">(loading...)</p>
      <div class="nav-collapse collapse">
        <form class="navbar-form pull-right" action="{{uri base 'authenticate'}}" method="post" id="form-authenticate">
          <input class="input-small" type="text" placeholder="Login" name="login">
          <input class="input-small" type="password" placeholder="Password" name="password">
          <button class="btn btn-primary" type="submit"><i class="fa fa-user"></i> Authenticate</button>
        </form>
      </div>
    </div>
  </div>
</div>

<div class="container">
  <div class="row">
    <div id="picker" class="modal span8 offset2" style="display:none;left:auto">
      <div class="modal-header">
        <button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>
        <h3>Choose an object</h3>
      </div>
      <div class="modal-body"></div>
      <div class="modal-footer">
        <button class="btn" data-dismiss="modal" aria-hidden="true">Close</button>
      </div>
    </div>
    <div class="span3" id="navigation"></div>
    <div class="span9">
      <div class="page-header" id="header">
        <h1>Welcome to Aulë</h1>
      </div>
      <div id="main"></div>
      <div id="messages"></div>
    </div>
  </div>
</div>

<footer class="footer">
  <div class="container">
    <p>Aulë is built by <a href="http://martijn.vermaat.name">Martijn
    Vermaat</a> at <a href="http://humgen.nl">Leiden University Medical
    Center</a>.</p>
  </div>
</footer>
