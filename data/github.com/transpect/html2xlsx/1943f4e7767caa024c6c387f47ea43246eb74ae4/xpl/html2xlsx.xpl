<?xml version="1.0" encoding="UTF-8"?>
<p:declare-step xmlns:p="http://www.w3.org/ns/xproc"
  xmlns:c="http://www.w3.org/ns/xproc-step" 
  xmlns:cx="http://xmlcalabash.com/ns/extensions"
  xmlns:pxp="http://exproc.org/proposed/steps"
  xmlns:cxf="http://xmlcalabash.com/ns/extensions/fileutils"
  xmlns:xdr="http://schemas.openxmlformats.org/drawingml/2006/spreadsheetDrawing"
  xmlns:tr="http://transpect.io"
  version="1.0" 
  name="html2xlsx"
  type="tr:html2xlsx">
  
  <p:documentation>
    Take tables from HTML and patch 
    them into an Excel template file.
    
    Invoke with:
        $ ./calabash/calabash.sh -i source=test/test.xml xpl/html2xlsx.xpl (template=template/template.xlsx)
  </p:documentation>
  
  <p:input port="source"/>
  
  <p:output port="result"/>
  
  <p:output port="sheet" primary="false">
    <p:pipe port="result" step="resolve-content"/>
  </p:output>
    
  <p:option name="template" select="'http://transpect.io/html2xlsx/template/template.xlsx'"/>
  <p:option name="th-style-from-row" select="0"/>
  <p:option name="td-style-from-row" select="0"/>
  <p:option name="keep-rows" select="0"/>
  
  <p:option name="static-rows" select="'no'"/>
  <p:option name="use-html-th" select="'yes'"/>
  <p:option name="out-dir-uri" select="''"/>
  <p:option name="out-file" select="''"/>
  
  <p:option name="debug" select="'yes'"/>
  <p:option name="debug-dir-uri" select="'debug'"/>
  
  <p:import href="http://xmlcalabash.com/extension/steps/library-1.0.xpl"/>
  <p:import href="http://transpect.io/calabash-extensions/unzip-extension/unzip-declaration.xpl"/>
  <p:import href="http://transpect.io/xproc-util/file-uri/xpl/file-uri.xpl"/>
  <p:import href="http://transpect.io/xproc-util/store-debug/xpl/store-debug.xpl"/>
  <p:import href="http://transpect.io/xproc-util/xslt-mode/xpl/xslt-mode.xpl"/>
  <p:import href="overwrite-files.xpl"/>
<!--  <p:import href="http://transpect.io/xproc-util/copy-files/xpl/copy-files.xpl"/>-->

  <p:add-xml-base/>

  <tr:file-uri name="file-uri">
    <p:with-option name="filename" select="/*/@xml:base"/>
  </tr:file-uri>
  
  <tr:store-debug pipeline-step="unzip/file-uri">
    <p:with-option name="active" select="$debug"/>
    <p:with-option name="default-uri" select="$debug-dir-uri"/>
  </tr:store-debug>
  
  <p:sink/>
  
  <p:group name="target-dir-and-name">
    <p:output port="result" primary="true"/>
    <p:identity>
      <p:input port="source">
        <p:inline>
          <file/>
        </p:inline>
      </p:input>
    </p:identity>
    <p:add-attribute match="/*" attribute-name="dir">
      <p:with-option name="attribute-value" select="($out-dir-uri[. ne ''], replace(/c:result/@local-href, '^(.+)/.+\.[a-z]+$', '$1'))[1]">
        <p:pipe port="result" step="file-uri"/>
      </p:with-option>
    </p:add-attribute>
    <p:add-attribute match="/*" attribute-name="name">
      <p:with-option name="attribute-value" select="($out-file[. ne ''], replace(/c:result/@local-href, '^.+/(.+)\.[a-z]+$', '$1'))[1]">
        <p:pipe port="result" step="file-uri"/>
      </p:with-option>
    </p:add-attribute>
  </p:group>
  
  <p:sink/>
  
  <tr:file-uri name="template-file-uri">
    <p:with-option name="filename" select="$template"/>
    <p:input port="catalog">
      <p:document href="../xmlcatalog/catalog.xml"/>
    </p:input>
    <p:input port="resolver">
      <p:document href="http://transpect.io/xslt-util/xslt-based-catalog-resolver/xsl/resolve-uri-by-catalog.xsl"/>
    </p:input>
  </tr:file-uri>
  
  <tr:unzip name="unzip">
    <p:with-option name="zip" select="/*/@local-href"/>
    <p:with-option name="dest-dir" select="replace(/c:result/@os-path, '^(.*)/.*', '$1/')">
      <p:pipe port="result" step="file-uri"/>
    </p:with-option>
    <p:with-option name="overwrite" select="'yes'"/>
  </tr:unzip>
  
  <p:group name="load-template-files">
    <p:output port="worksheet">
      <p:pipe port="result" step="load-template-worksheet"/>
    </p:output>
    <p:output port="relations" sequence="true">
      <p:pipe port="result" step="load-template-worksheet_rels"/>
    </p:output>
    <p:output port="sharedStrings">
      <p:pipe port="result" step="load-template-shared-strings"/>
    </p:output>

    <p:variable name="base" select="/*/@xml:base"/>
    
    <p:load name="load-template-worksheet">
      <p:with-option name="href" select="concat(/*/@xml:base, 'xl/worksheets/sheet1.xml')"/>
    </p:load>

    <tr:store-debug pipeline-step="unzip/worksheet">
      <p:with-option name="active" select="$debug"/>
      <p:with-option name="default-uri" select="$debug-dir-uri"/>
    </tr:store-debug>

    <p:sink/>
    
    <p:try name="load-template-worksheet_rels">
    <p:group>
      <p:output port="result" sequence="true"/>
      <p:load >
        <p:with-option name="href" select="concat($base, 'xl/worksheets/_rels/sheet1.xml.rels')"/>
      </p:load>
    </p:group>
    <p:catch>
      <p:output port="result" sequence="true">
        <p:empty/>
      </p:output>
      <p:identity>
        <p:input port="source">
          <p:inline>
            <bogo/>
          </p:inline>
        </p:input>
      </p:identity>
      <p:sink/>
    </p:catch>
  </p:try>

    <tr:store-debug pipeline-step="unzip/worksheet_rels">
      <p:with-option name="active" select="$debug"/>
      <p:with-option name="default-uri" select="$debug-dir-uri"/>
    </tr:store-debug>

    <p:sink/>

    <p:load name="load-template-shared-strings">
      <p:with-option name="href" select="concat($base, 'xl/sharedStrings.xml')"/>
    </p:load>

    <tr:store-debug pipeline-step="unzip/shared-strings">
      <p:with-option name="active" select="$debug"/>
      <p:with-option name="default-uri" select="$debug-dir-uri"/>
    </tr:store-debug>
    <p:sink/>

  </p:group>
  
  <p:xslt name="convert-framemaker-tables">
    <p:input port="source">
      <p:pipe port="worksheet" step="load-template-files"/>
      <p:pipe port="source" step="html2xlsx"/>
    </p:input>
    <p:input port="stylesheet">
      <p:document href="../xsl/html2xlsx.xsl"/>
    </p:input>
    <p:input port="parameters">
      <p:empty/>
    </p:input>
    <p:with-param name="th-style-from-row" select="$th-style-from-row"/>
    <p:with-param name="td-style-from-row" select="$td-style-from-row"/>
    <p:with-param name="keep-rows"  select="$keep-rows"/>
    <p:with-param name="static-rows" select="$static-rows"/>
    <p:with-param name="use-html-th" select="$use-html-th"/>
  </p:xslt>
  
  <tr:store-debug pipeline-step="excel/worksheet">
    <p:with-option name="active" select="$debug"/>
    <p:with-option name="base-uri" select="$debug-dir-uri"/>
  </tr:store-debug>

  <tr:xslt-mode name="export-relationships" mode="relation">
    <p:input port="stylesheet">
      <p:document href="../xsl/html2xlsx.xsl"/>
    </p:input>
    <p:input port="parameters">
      <p:empty/>
    </p:input>
    <p:with-param name="th-style-from-row" select="$th-style-from-row"/>
    <p:with-param name="td-style-from-row" select="$td-style-from-row"/>
    <p:with-param name="keep-rows"  select="$keep-rows"/>
    <p:with-param name="static-rows" select="$static-rows"/>
    <p:with-param name="use-html-th" select="$use-html-th"/>
    <p:with-option name="debug" select="$debug"/>
    <p:with-option name="debug-dir-uri" select="$debug-dir-uri"/>
    <p:with-option name="store-secondary" select="'no'"/>
    <p:with-option name="fail-on-error" select="'no'"/>
    <p:with-option name="adjust-doc-base-uri" select="'no'"/>
  </tr:xslt-mode>
  
   <tr:store-debug pipeline-step="excel/mode_relation">
    <p:with-option name="active" select="$debug"/>
    <p:with-option name="base-uri" select="$debug-dir-uri"/>
  </tr:store-debug>
  
  <p:xslt name="resolve-content" initial-mode="shared-strings" cx:depends-on="export-relationships">
    <p:input port="source">
      <p:pipe port="result" step="export-relationships"/>
      <p:pipe port="sharedStrings" step="load-template-files"/>
    </p:input>
    <p:input port="stylesheet">
      <p:document href="../xsl/html2xlsx.xsl"/>
    </p:input>
    <p:input port="parameters">
      <p:empty/>
    </p:input>

    <p:with-param name="th-style-from-row" select="$th-style-from-row"/>
    <p:with-param name="td-style-from-row" select="$td-style-from-row"/>
    <p:with-param name="keep-rows" select="$keep-rows"/>
    <p:with-param name="static-rows" select="$static-rows"/>
    <p:with-param name="use-html-th" select="$use-html-th"/>
  </p:xslt>
  
  <tr:store-debug pipeline-step="excel/resolve_content">
    <p:with-option name="active" select="$debug"/>
    <p:with-option name="base-uri" select="$debug-dir-uri"/>
  </tr:store-debug>
  <!--
  <p:sink/>
  
  <tr:store-debug pipeline-step="excel/gen-shared-strings">
    <p:input port="source">
      <p:pipe port="secondary" step="resolve-content"/>
    </p:input>
    <p:with-option name="active" select="$debug"/>
    <p:with-option name="base-uri" select="$debug-dir-uri"/>
  </tr:store-debug>
  -->
  <p:sink/>
  
  <p:wrap-sequence wrapper="Relationships" cx:depends-on="resolve-content">
    <p:with-option name="wrapper-namespace" select="'http://schemas.openxmlformats.org/package/2006/relationships'"/>
    <p:input port="source">   
      <p:pipe port="secondary" step="export-relationships"/>
    </p:input>
  </p:wrap-sequence>
  
  <p:insert position="first-child">
    <p:input port="insertion" select="/*/*:Relationship">
      <p:pipe port="relations" step="load-template-files"/>
    </p:input>
  </p:insert>
  
   <tr:store-debug pipeline-step="excel/input_rels">
    <p:with-option name="active" select="$debug"/>
    <p:with-option name="base-uri" select="$debug-dir-uri"/>
  </tr:store-debug>
  
  <tr:overwrite-files name="overwrite-worksheet_rels">
    <p:with-option name="file" select="concat(/*/@xml:base, 'xl/worksheets/_rels/sheet1.xml.rels')">
      <p:pipe port="result" step="unzip"/>
    </p:with-option>
     <p:with-option name="debug" select="$debug"/>
    <p:with-option name="debug-dir-uri" select="$debug-dir-uri"/>
  </tr:overwrite-files>
  
<!-- for now there were no shared strings created but all shared strings from template convertet to inline strings
    <tr:overwrite-files name="overwrite-shared-strings">
    <p:input port="source">
      <p:pipe port="secondary" step="resolve-content"/>
    </p:input>
    <p:with-option name="file" select="concat(/*/@xml:base, 'xl/sharedStrings.xml')">
      <p:pipe port="result" step="unzip"/>
    </p:with-option>
     <p:with-option name="debug" select="$debug"/>
    <p:with-option name="debug-dir-uri" select="$debug-dir-uri"/>
  </tr:overwrite-files>-->

 <!-- 

  <tr:xslt-mode name="export-drawings" mode="drawings">
    <p:input port="source">
      <p:pipe port="result" step="convert-framemaker-tables"/>
    </p:input>
    <p:input port="stylesheet">
      <p:document href="../xsl/html2xlsx.xsl"/>
    </p:input>
    <p:input port="parameters">
      <p:empty/>
    </p:input>
    <p:with-option name="debug" select="$debug"/>
    <p:with-option name="debug-dir-uri" select="$debug-dir-uri"/>
    <p:with-option name="adjust-doc-base-uri" select="'no'"/>
  </tr:xslt-mode>
  
   <tr:store-debug pipeline-step="excel/drawings">
    <p:with-option name="active" select="$debug"/>
    <p:with-option name="base-uri" select="$debug-dir-uri"/>
  </tr:store-debug>
  
  <p:sink/>
  
  <p:identity>
    <p:input port="source">   
      <p:pipe port="secondary" step="export-drawings"/>
    </p:input>
  </p:identity>
  
  <tr:overwrite-files name="overwrite-drawings">
   <!-\- <p:input port="source">
      <p:pipe port="result" step="convert-framemaker-tables"/>
    </p:input>-\->
    <p:with-option name="file" select="concat(/*/@xml:base, 'xl/drawings/drawing1.xml')">
      <p:pipe port="result" step="file-list"/>
    </p:with-option>
     <p:with-option name="debug" select="$debug"/>
    <p:with-option name="debug-dir-uri" select="$debug-dir-uri"/>
  </tr:overwrite-files>-->
  
  <!--<tr:store-debug pipeline-step="excel/drawings">
    <p:with-option name="active" select="$debug"/>
    <p:with-option name="base-uri" select="$debug-dir-uri"/>
  </tr:store-debug>-->
 
<!--  <p:sink/>-->
  
  <tr:overwrite-files name="overwrite-worksheet" cx:depends-on="overwrite-worksheet_rels">
    <p:input port="source">
<!--      <p:pipe port="result" step="export-relationships"/>-->
      <p:pipe port="result" step="resolve-content"/>
    </p:input>
    <p:with-option name="file" select="concat(/*/@xml:base, 'xl/worksheets/sheet1.xml')">
      <p:pipe port="result" step="unzip"/>
    </p:with-option>
    <p:with-option name="debug" select="$debug"/>
    <p:with-option name="debug-dir-uri" select="$debug-dir-uri"/>
  </tr:overwrite-files>
  
  <p:xslt name="generate-zip-manifest" cx:depends-on="overwrite-worksheet">
    <p:input port="source">
      <p:pipe port="result" step="unzip"/>
    </p:input>
    <p:input port="stylesheet">
      <p:inline>
        <xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
          xmlns:xs="http://www.w3.org/2001/XMLSchema"
          
          version="2.0">
          
          <xsl:param name="base-uri"/>
          
          <xsl:template match="/c:files">
            
            <c:zip-manifest>
              <xsl:apply-templates/>
            </c:zip-manifest>
          </xsl:template>
          
          <xsl:template match="c:file">
            <xsl:variable name="href" select="replace(
                                                     replace(
                                                         (concat(/*/@xml:base, @name)),
                                                         '\[',
                                                         '%5B'),
                                                     '\]',
                                                     '%5D')
                                                     " as="xs:string"/>
            <xsl:message select="$href"/>
            <c:entry name="{@name}" compression-method="deflate" compression-level="default">
              <xsl:attribute name="href" select="$href"/>
            </c:entry>
          </xsl:template>
          
        </xsl:stylesheet>
      </p:inline>
    </p:input>
    <p:with-param name="base-uri" select="/c:result/@local-href">
      <p:pipe port="result" step="file-uri"/>
    </p:with-param>
  </p:xslt>
  
  <tr:store-debug pipeline-step="excel/zip-manifest">
    <p:with-option name="active" select="$debug"/>
    <p:with-option name="base-uri" select="$debug-dir-uri"/>
  </tr:store-debug>
  
  <pxp:zip cx:depends-on="overwrite-worksheet">    
    <p:input port="manifest">
      <p:pipe port="result" step="generate-zip-manifest"/>
    </p:input>
    <p:with-option name="href" select="concat(/*/@*:dir, '/', /*/@*:name)">
      <p:pipe port="result" step="target-dir-and-name"/>
    </p:with-option>
  </pxp:zip>
  
  <p:sink/>
</p:declare-step>