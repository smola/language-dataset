<p:library xmlns:p="http://www.w3.org/ns/xproc"
           xmlns:c="http://www.w3.org/ns/xproc-step"
           xmlns:cx="http://xmlcalabash.com/ns/extensions"
           xmlns:cxf="http://xmlcalabash.com/ns/extensions/fileutils"
           xmlns:pkg="http://expath.org/ns/pkg"
           xmlns:client="http://cxan.org/ns/client"
           pkg:import-uri="#none"
           version="1.0">

   <!--
       A real error step, without any input and with an empty output, and a message option.
   -->
   <p:declare-step type="client:error">
      <p:output port="result" primary="true"/>
      <p:option name="code" required="true"/>
      <p:option name="msg"  required="true"/>
      <p:template>
         <p:input port="source">
            <p:empty/>
         </p:input>
         <p:input port="template">
            <p:inline>
               <msg>{ $msg }</msg>
            </p:inline>
         </p:input>
         <p:with-param name="msg" select="$msg"/>
      </p:template>
      <p:error>
         <p:with-option name="code" select="$code"/>
      </p:error>
   </p:declare-step>

   <!--
       Send an HTTP GET to the CXAN website, at the address $href.
       
       Set the Accept HTTP header to application/xml.  Return the result of
       the HTTP Client step.  The option $href is "absolute" (relative to the
       website root, can start with a slash, or not).
   -->
   <p:declare-step type="client:http-get" name="get">
      <p:option name="href" required="true"/>
      <p:input  port="parameters" kind="parameter" primary="true"/>
      <p:output port="result" primary="true"/>
      <p:wrap-sequence wrapper="wrapper">
         <p:input port="source">
            <p:pipe step="get" port="parameters"/>
         </p:input>
      </p:wrap-sequence>
      <p:group>
         <p:variable name="param"  select="/wrapper/c:param-set/c:param[@name eq 'server']/@value"/>
         <p:variable name="server" select="( $param[.], 'http://cxan.org/' )[1]"/>
         <p:template>
            <p:input port="template">
               <p:inline>
                  <c:request method="get" href="{ $host }{ $path }">
                     <c:header name="Accept" value="application/xml"/>
                  </c:request>
               </p:inline>
            </p:input>
            <p:with-param name="host" select="$server"/>
            <p:with-param name="path" select="
                if ( starts-with($href, '/') ) then substring($href, 2) else $href"/>
         </p:template>
         <p:http-request/>
      </p:group>
   </p:declare-step>

   <!--
       TODO: Handle the case when additional files are provided, besides the
       XAR.  For now, this is an error.  To be supported, we'll need multipart
       request, and we'll need the website to understand it.
       
       TODO: Is it normal I use the EXPath HTTP Client in XSLT, instead of the
       standard XProc HTTP step?  Is it because of a limitation of the XProc
       step?  If yes, document it...  It makes the CXAN client dependent on the
       EXPath HTTP Client, implemented as a 3d-party extension for Saxon: try to
       use the XProc step instead...
       
       TODO: Define the output of this step...  And don't do any formating here,
       only a meaningful XML document with the outcome of the PUT.
       
       TODO: Implement a user-credentials mechanism...
   -->
   <p:declare-step type="client:http-put" name="put">
      <p:option name="id"    required="true"/>
      <p:option name="xar"   required="true"/>
      <p:option name="files" required="true"/>
      <p:input  port="parameters" kind="parameter" primary="true"/>
      <p:output port="result" primary="true"/>
      <p:wrap-sequence wrapper="wrapper">
         <p:input port="source">
            <p:pipe step="put" port="parameters"/>
         </p:input>
      </p:wrap-sequence>
      <p:group>
         <p:variable name="param"  select="/wrapper/c:param-set/c:param[@name eq 'server']/@value"/>
         <p:variable name="server" select="( $param[.], 'http://cxan.org/' )[1]"/>
         <p:xslt template-name="main">
            <p:input port="source">
               <p:empty/>
            </p:input>
            <p:input port="stylesheet">
               <p:inline>
                  <xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                                  xmlns:xs="http://www.w3.org/2001/XMLSchema"
                                  xmlns:http="http://expath.org/ns/http-client"
                                  exclude-result-prefixes="#all"
                                  version="2.0">

                     <xsl:import href="http://expath.org/ns/http-client.xsl"/>

                     <xsl:param name="host"  as="xs:string"/>
                     <xsl:param name="id"    as="xs:string"/>
                     <xsl:param name="xar"   as="xs:string"/>
                     <xsl:param name="files" as="xs:string?"/>

                     <xsl:function name="client:make-body">
                        <xsl:param name="filename" as="xs:string"/>
                        <xsl:variable name="basename" select="tokenize($filename, '/')[last()]"/>
                        <http:header name="Content-Disposition"
                                     value='attachment; filename="{ $basename }"'/>
                        <http:body media-type="application/octet" src="{ $filename }"/>
                     </xsl:function>

                     <xsl:template name="main">
                        <xsl:variable name="req" as="element()">
                           <http:request href="{ $host }pkg/{ $id }" method="put">
                              <http:header name="Accept" value="application/xml"/>
                              <xsl:choose>
                                 <xsl:when test="empty($files[.])">
                                    <xsl:sequence select="client:make-body($xar)"/>
                                 </xsl:when>
                                 <xsl:otherwise>
                                    <http:multipart media-type="multipart/mixed"
                                                    boundary="soMe-bOunDarY-ThAt-WOnT-bE-ThErE">
                                       <xsl:sequence select="client:make-body($xar)"/>
                                       <xsl:for-each select="tokenize($files, '&#10;')">
                                          <xsl:sequence select="client:make-body(.)"/>
                                       </xsl:for-each>
                                    </http:multipart>
                                 </xsl:otherwise>
                              </xsl:choose>
                           </http:request>
                        </xsl:variable>
                        <xsl:variable name="res" select="http:send-request($req)"/>
                        <xsl:sequence select="$res"/>
                        <!-- TODO: User output should not be handled here... -->
                        <!--stdout>
                           <xsl:choose>
                              <!- - TODO: In addition to the HTTP status, take a look at the
                                   response payload (is it a "success" element?) - ->
                              <xsl:when test="$res[1]/number(@status) eq 200">
                                 <line>
                                    <xsl:text>Package </xsl:text>
                                    <xsl:value-of select="$xar"/>
                                    <xsl:text> successfuly uploaded.</xsl:text>
                                 </line>
                              </xsl:when>
                              <xsl:otherwise>
                                 <line>
                                    <xsl:text>Error uploading package </xsl:text>
                                    <xsl:value-of select="$xar"/>
                                    <xsl:text>.</xsl:text>
                                 </line>
                                 <line>See logs for details.</line>
                              </xsl:otherwise>
                           </xsl:choose>
                           <res-1>
                              <xsl:copy-of select="$res[1]"/>
                           </res-1>
                           <res-2>
                              <xsl:copy-of select="$res[2]"/>
                           </res-2>
                        </stdout-->
                     </xsl:template>

                  </xsl:stylesheet>
               </p:inline>
            </p:input>
            <p:with-param name="host"  select="$server"/>
            <p:with-param name="id"    select="$id"/>
            <p:with-param name="xar"   select="$xar"/>
            <p:with-param name="files" select="$files"/>
         </p:xslt>
      </p:group>
   </p:declare-step>

</p:library>
