<?xml version="1.0" encoding="UTF-8"?>
<p:declare-step xmlns:p="http://www.w3.org/ns/xproc"
  xmlns:c="http://www.w3.org/ns/xproc-step"
  xmlns:tr="http://transpect.io"
  xmlns:cx="http://xmlcalabash.com/ns/extensions"
  xmlns:pxf="http://exproc.org/proposed/steps/file"
  version="1.0" 
  name="epubcheck-command-line" 
  type="tr:epubcheck-command-line">
  
  <p:output port="result"/>
  
  <p:option name="epubcheck-path" select="'../4.2.2/bin/epubcheck.jar'"/>
  <p:option name="epubfile-path"/>
  <p:option name="fallback-version" select="'4.2.2'"/>
  <p:option name="svrl-srcpath" select="'BC_Orphans'"/>
  <p:option name="debug" select="'no'"/>
  <p:option name="debug-dir-uri" select="'debug'"/>
  
  <p:import href="http://xmlcalabash.com/extension/steps/library-1.0.xpl"/>
  <p:import href="http://transpect.io/xproc-util/file-uri/xpl/file-uri.xpl"/>
  
  <tr:file-uri name="jar-file">
    <p:with-option name="filename" select="$epubcheck-path"/>
    <p:input port="catalog">
      <p:document href="http://this.transpect.io/xmlcatalog/catalog.xml"/>
    </p:input>
    <p:input port="resolver">
      <p:document href="http://transpect.io/xslt-util/xslt-based-catalog-resolver/xsl/resolve-uri-by-catalog.xsl"/>
    </p:input>
  </tr:file-uri>
  
  <pxf:info name="jar-info" fail-on-error="false">
    <p:with-option name="href" select="/*/@local-href">
      <p:pipe port="result" step="jar-file"/>
    </p:with-option>
  </pxf:info>
  
  <p:choose name="checked-jar-file">
    <p:xpath-context>
      <p:pipe port="result" step="jar-info"/>
    </p:xpath-context>
    <p:when test="/c:file[@readable = 'true']">
      <p:output port="result">
        <p:pipe port="result" step="orig-result"/>
      </p:output>
      <p:identity name="orig-result">
        <p:input port="source">
          <p:pipe port="result" step="jar-file"/>
        </p:input>
      </p:identity>
      <p:sink/>
    </p:when>
    <p:otherwise>
      <p:output port="result">
        <p:pipe port="result" step="jar-file-fallback"/>
      </p:output>
      
      <cx:message>
        <p:with-option name="message" select="'[WARNING] jar in epubcheck path: ', $epubcheck-path, ' not found. Used fallback version ', $fallback-version,' instead.'"/>
      </cx:message>
      
      <tr:file-uri name="jar-file-fallback">
        <p:with-option name="filename" select="concat('http://transpect.io/epubcheck-idpf/', $fallback-version,'/bin/epubcheck.jar')"/>
        <p:input port="catalog">
          <p:document href="http://this.transpect.io/xmlcatalog/catalog.xml"/>
        </p:input>
        <p:input port="resolver">
          <p:document href="http://transpect.io/xslt-util/xslt-based-catalog-resolver/xsl/resolve-uri-by-catalog.xsl"/>
        </p:input>
      </tr:file-uri>
      <p:sink/>
    </p:otherwise>
  </p:choose>
  
  <p:group name="do-check">
    <p:variable name="jar" select="/*/@os-path">
      <p:pipe port="result" step="checked-jar-file"/>
    </p:variable>
    
    <p:sink/>
    
    <p:exec name="execute-epubcheck" result-is-xml="false" errors-is-xml="false" wrap-error-lines="true"
      wrap-result-lines="true">
      <p:input port="source">
        <p:empty/>
      </p:input>
      <p:with-option name="command" select="'java'"/>
      <p:with-option name="args" select="concat('-jar ', $jar, ' ', $epubfile-path)"/>
    </p:exec>
    
    <p:sink/>
    
    <p:wrap-sequence wrapper="document" wrapper-prefix="cx" wrapper-namespace="http://xmlcalabash.com/ns/extensions">
      <p:input port="source">
        <p:pipe port="errors" step="execute-epubcheck"/>
      </p:input>
    </p:wrap-sequence>
    
    <p:add-attribute match="/cx:document" attribute-name="epubcheck-path">
      <p:with-option name="attribute-value" select="$epubcheck-path"/>
    </p:add-attribute>
    
    <p:add-attribute match="/cx:document" attribute-name="epubfile-path">
      <p:with-option name="attribute-value" select="$epubfile-path"/>
    </p:add-attribute>
    
    <tr:store-debug pipeline-step="epubcheck/epubcheck.out">
      <p:with-option name="active" select="$debug"/>
      <p:with-option name="base-uri" select="$debug-dir-uri"/>
    </tr:store-debug>
    
    <p:xslt name="convert-epubcheck-output">
      <p:input port="stylesheet">
        <p:document href="../xsl/epubcheck.xsl"/>
      </p:input>
      <p:with-param name="svrl-srcpath" select="$svrl-srcpath"/>
      <p:with-param name="epubfile-path" select="$epubfile-path"/>
    </p:xslt>
    
  </p:group>
  
</p:declare-step>
