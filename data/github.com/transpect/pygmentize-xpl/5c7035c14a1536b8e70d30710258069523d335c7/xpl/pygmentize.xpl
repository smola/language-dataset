<?xml version="1.0" encoding="UTF-8"?>
<p:declare-step xmlns:p="http://www.w3.org/ns/xproc"
  xmlns:c="http://www.w3.org/ns/xproc-step"
  xmlns:cx="http://xmlcalabash.com/ns/extensions"
  xmlns:html="http://www.w3.org/1999/xhtml"
  xmlns:tr="http://transpect.io"
  xmlns:pos="http://exproc.org/proposed/steps/os"
  version="1.0"
  type="tr:pygmentize"
  name="pygmentize" exclude-inline-prefixes="c">
  
  <p:documentation xmlns="http://www.w3.org/1999/xhtml">
    <p>A pygmentize front-end, using p:exec.</p>
    <p>Requirements: an executable file called 'pygments' in the search path.</p>
    <p>Invocation example: <code>&lt;tr:pygmentize style="manni" language="json" code-string='{"foo":"bar"}'></code></p>
  </p:documentation>
  
  <p:option name="code-string">
    <p:documentation>Only text, no markup. In a future version, we might process callout or highlighting inline elements
    by transforming them to language-specific comments first.</p:documentation>
  </p:option>
  
  <p:option name="language" select="''">
    <p:documentation>If the empty string (not recommended), will be autodetected by pygments. In the future, some normalization
    might be performed on the option value so that it fit pygments’ expectations. But pygments is quite permissive already.</p:documentation>
  </p:option>
  
  <p:option name="style" select="''">
    <p:documentation>Passes its value, if non-empty, as the -S option to pygmentize.</p:documentation>
  </p:option>
  
  <p:output port="result" primary="true">
    <p:documentation>An XHTML div with class="highlight"</p:documentation>
  </p:output>
  <p:serialization port="result" omit-xml-declaration="false"/>

  <p:import href="http://xmlcalabash.com/extension/steps/library-1.0.xpl"/>

<!--  <pos:info name="info"/>
  
  <p:sink/>
-->
  <p:group>
    <p:in-scope-names name="vars"/>

    <p:template name="cmd">
      <p:input port="template">
        <p:inline>
          <cmd>pygmentize {if ($style = '') then '' else concat('-O style=', $style)} {if ($language = '') then '' else concat('-l ', $language)} -f html </cmd>
        </p:inline>
      </p:input>
      <p:input port="source">
        <p:empty/>
      </p:input>
      <p:input port="parameters">
        <p:pipe port="result" step="vars"/>
      </p:input>
    </p:template>
    <cx:message>
      <p:with-option name="message" select="."></p:with-option>
    </cx:message>

    <p:sink/>

    <p:template name="code-input">
      <p:input port="template">
        <p:inline><pre xmlns="http://www.w3.org/1999/xhtml" data-code-language="{$language}">{$code-string}</pre></p:inline>
      </p:input>
      <p:input port="source">
        <p:empty/>
      </p:input>
      <p:input port="parameters">
        <p:pipe port="result" step="vars"/>
      </p:input>
    </p:template>

    <p:try>
      <p:group>
        <p:exec command="env" source-is-xml="false" result-is-xml="true">
          <p:with-option name="args" select="/cmd">
            <p:pipe port="result" step="cmd"/>
          </p:with-option>
        </p:exec>
        <p:filter select="/c:result/*"/>
        <p:namespace-rename from="" to="http://www.w3.org/1999/xhtml" apply-to="elements"/>
      </p:group>
      <p:catch>
        <p:identity>
          <p:input port="source">
            <p:pipe port="result" step="code-input"/>
          </p:input>
        </p:identity>
        <p:add-attribute match="/*" attribute-name="data-pygments" attribute-value="failed"/>
      </p:catch>
    </p:try>
    
    <p:set-attributes match="/*/html:pre">
      <p:input port="attributes">
        <p:pipe port="result" step="code-input"/>
      </p:input>
    </p:set-attributes>
    <p:delete match="/*/html:pre/@data-code-language[. = '']"/>
  </p:group>

</p:declare-step>