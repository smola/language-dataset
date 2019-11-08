<script type="text/x-handlebars" id="register">
    <header>
      <h1>Login or Register</h1>
    </header>

    <form class="ui form warning error segment">
          <h1>Register</h1>

          <div class="ui error message">
            <div class="header">Registration Error</div>
            <p>There was a problem with your registration</p>
          </div>

          <div class="ui warning message">
            <div class="header">Could you check something!</div>
            <ul class="list">
              <li>You forgot your <b>first name</b></li>
              <li>And also your <b>last name</b></li>
            </ul>
          </div>

          <div class="two error fields">
            <div class="field">
              <label>First Name</label>
                {{input type="text" value=firstName size="50" placeholder="First Name"}}
                <div class="ui red pointing above ui label">Enter a first name</div>
            </div>
            <div class="field">
              <label>Last Name</label>
                {{input type="text" value=lastName size="50" placeholder="Last Name"}}
                <div class="ui red pointing above ui label">Enter a last name</div>
            </div>
          </div>
          <div class="field">
            <label>Email</label>
              {{input type="text" value=email size="50" placeholder="Email Address"}}
          </div>
          <div class="field">
            <label>Password</label>
              {{input type="text" value=password size="50" placeholder="Password"}}
          </div>
          <div class="field">
            <label>Confirm Password</label>
            <input type="password" placeholder="Confirm Password">
          </div>

        <button {{action 'register'}} class="ui blue submit button">Create Account</button>
    </form>
     
    <footer>
        <p>Backdraft Startup App</p>
    </footer>        
</script>