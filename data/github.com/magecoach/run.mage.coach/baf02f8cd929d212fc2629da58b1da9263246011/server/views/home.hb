    <div id="page ">
      <div id="my-clouds">
        <marquee behavior="scroll" direction="right" scrollamount="5">
            <i class="fa fa-cloud" style="background:transparent!important;color:white;font-size:60px"></i>
        </marquee>
      </div>
      <div id="content">
        <!--<h2>Analyze your website speed and performance:</h2>-->

        <form id="analyze-form" method="post" action="/">
          <input id="analyze-url" name="url" type="url" required pattern="https?://.+"  placeholder="http(s)://" title="Add a URL starting with http(s)://">
          <label><span>Email:</span>
             <input id="email" name="email" type="email" class="validate[required,custom[email]] data-input" placeholder="Please enter your email address." required>
          </label>

          <details>
          <summary>Advanced</summary>
          
          <label><span>Browser:</span>
            <select name="browser">
              <option value="chrome" selected>Chrome</option>
              <option value="firefox">Firefox</option>
            </select>
          </label>
	        <label><span>Device type:</span>
            <select name="display">
              <option value="desktop" selected>Desktop</option>
              <option value="mobile">Mobile</option>
            </select>
          </label>
          <label><span>Connection type:</span>
            <select name="connection">
              <option value="3g">3g - 1600/768 300 RTT</option>
              <option value="3gfast">3gfast - 1600/768 150 RTT</option>
              <option value="3gslow">3gslow - 780/330 200 RTT</option>
              <option value="3gem">3gem - 400/400 400 RTT</option>
              <option value="2g">2g - 35/328 1300 RTT</option>
              <option value="cable" selected>cable - 5000/1000 28 RTT</option>
              <option value="native">native</option>
            </select>
          </label>

          {{#if premium}}
          <label><span>Amount of Runs:</span>
            <select name="runs">
              <option value="1" selected>1</option>
              <option value="2">2</option>
              <option value="3">3</option>
            </select>
          </label>
          <label><span>Depth level:</span>
            <select name="depth">
              <option value="1" selected>1</option>
              <option value="2">2</option>
              <option value="3">3</option>
            </select>
          </label>
          <label><span>Max pages:</span>
            <select name="max">
              <option value="1" selected>1</option>
              <option value="5">5</option>
              <option value="10">10</option>
              <option value="15">15</option>
              <option value="20">20</option>
              <option value="25">25</option>
              <option value="50">50</option>
              <option value="75">75</option>
              <option value="100">100</option>
            </select>
          </label>
          <label><span>Multi URL's:</span><textarea name="multiurl" rows="10" cols="100"></textarea></label>
          
<script type="text/javascript">
$(function(){

    $('textarea').on('keypress',function(event){
          var text = $('textarea').val();
          var lines = text.split("\n");
          var currentLine = this.value.substr(0, this.selectionStart).split("\n").length;
          console.log(lines);
          console.log(currentLine);
          console.log(lines[currentLine-1]);
          if(event.keyCode == 13) {
            if (lines.length >= $(this).attr('rows'))
                return false;
          }
          else{
             if(lines[currentLine-1].length >= $(this).attr('cols')) {
                 return false; // prevent characters from appearing
         }
    }
});
});
</script>
          
          {{else}}
          <label><span>Amount of Runs:</span>
            <select name="runs">
              <option value="1" selected>1</option>
              <option value="2">2</option>
              <option value="3">3</option>
            </select>
          </label>
          <label><span>Depth level:</span>
            <select name="depth">
              <option value="1" selected>1</option>
              <option value="2">2</option>
              <option value="3">3</option>
            </select>
          </label>
          <label><span>Max pages:</span>
            <select name="max">
              <option value="1" selected>1</option>
              <option value="5">5</option>
              <option value="10">10</option>
            </select>
          </label>
          {{/if}}

          </details>

          <input type="submit" value="Start analyzing">
        </form>

      </div>
    </div>
