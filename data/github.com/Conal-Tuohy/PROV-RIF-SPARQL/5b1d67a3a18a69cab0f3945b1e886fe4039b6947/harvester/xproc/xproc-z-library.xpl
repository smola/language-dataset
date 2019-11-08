<p:library version="1.0" xmlns:p="http://www.w3.org/ns/xproc" xmlns:c="http://www.w3.org/ns/xproc-step" xmlns:z="https://github.com/Conal-Tuohy/XProc-Z" xmlns:fn="http://www.w3.org/2005/xpath-functions">
	<p:import href="http://xmlcalabash.com/extension/steps/library-1.0.xpl"/>
	
	<!-- serve static resources from inside the "static" folder-->
	<p:pipeline type="z:static">
		<z:parse-request-uri/>
		<p:template name="request-spec">
			<p:input port="template">
				<!-- sanitize path by removing ".." segments -->
				<p:inline>
					<c:request method="get" href="{
						concat(
							'../static/', 
							substring-after(
								replace(
									/c:param-set/c:param[@name='path']/@value, 
									'\.\.',
									''
								),
								'/static/'
							)
						)
					}"/>
				</p:inline>
			</p:input>
		</p:template>
		<p:try>
			<p:group name="serve-file">
				<p:http-request/>
				<p:template name="http-response">
					<p:input port="template">
						<p:inline>
							<c:response status="200">
								<c:header name="X-Powered-By" value="XProc using XML Calabash"/>
								<c:header name="Server" value="XProc-Z"/>
								<c:header name="Cache-Control" value="max-age=3600"/>
								{/c:body}
							</c:response>
						</p:inline>
					</p:input>
				</p:template>
			</p:group>
			<p:catch name="not-found">
				<z:not-found/>
			</p:catch>
		</p:try>
	</p:pipeline>
	
	<p:pipeline type="z:parse-request-uri">
		<p:option name="unproxify" select=" 'false' "/>
		<p:xslt>
			<p:with-param name="unproxify" select="$unproxify"/>
			<p:input port="stylesheet">
				<p:inline>
					<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:c="http://www.w3.org/ns/xproc-step">
						<xsl:param name="unproxify"/>
						<xsl:variable name="x-forwarded-host" select="/c:request/c:header[@name='x-forwarded-host']/@value"/>
						<xsl:template match="/">
							<c:param-set>
								<xsl:analyze-string 
									select="/c:request/@href" 
									regex="^(.*:)//([^:/]+)(:[0-9]+)?(.*)(\?.*)?$">
									<!-- TODO check above regex - is "[^:/]+" correct? -->
									<xsl:matching-substring>
										<c:param name="scheme" value="{regex-group(1)}"/>
										<xsl:choose>
											<xsl:when test="$unproxify='true' and $x-forwarded-host">
												<c:param name="host" value="{$x-forwarded-host}"/>
												<c:param name="port" value=""/>
												<c:param name="unproxified" value="true"/>
											</xsl:when>
											<xsl:otherwise>
												<c:param name="host" value="{regex-group(2)}"/>
												<c:param name="port" value="{regex-group(3)}"/>
											</xsl:otherwise>
										</xsl:choose>
										<c:param name="path" value="{regex-group(4)}"/>
										<c:param name="query" value="{regex-group(5)}"/>
									</xsl:matching-substring>
								</xsl:analyze-string>
							</c:param-set>
						</xsl:template>
					</xsl:stylesheet>
				</p:inline>
			</p:input>
			<p:input port="parameters">
				<p:empty/>
			</p:input>
		</p:xslt>
	</p:pipeline>
	
	<p:declare-step type="z:make-http-response" name="make-http-response">
		<p:documentation>
			Wraps its input in a c:response element in order to make an HTTP response.
		</p:documentation>
		<p:input port="source"/>
		<p:output port="result"/>
		<p:option name="status" select=" '200' "/>
		<p:option name="content-type" select=" 'application/xml' "/>
		<p:in-scope-names name="parameters"/>
		<p:template name="http-response">
			<p:input port="source">
				<p:pipe step="make-http-response" port="source"/>
			</p:input>
			<p:input port="template">
				<p:inline>
					<c:response status="{$status}">
						<c:header name="X-Powered-By" value="XProc using XML Calabash"/>
						<c:header name="Server" value="XProc-Z"/>
						<c:body content-type="{$content-type}">{/*}</c:body>
					</c:response>
				</p:inline>
			</p:input>
			<p:input port="parameters">
				<p:pipe step="parameters" port="result"/>
			</p:input>
		</p:template>
	</p:declare-step>
	
	<p:pipeline type="z:add-response-header">
		<p:option name="header-name" required="true"/>
		<p:option name="header-value" required="true"/>
		<p:insert match="/c:response" position="first-child">
			<p:input port="insertion">
				<p:inline>
					<c:header/>
				</p:inline>
			</p:input>
		</p:insert>
		<p:add-attribute match="/c:response/c:header[1]" attribute-name="name">
			<p:with-option name="attribute-value" select="$header-name"/>
		</p:add-attribute>
		<p:add-attribute match="/c:response/c:header[1]" attribute-name="value">
			<p:with-option name="attribute-value" select="$header-value"/>
		</p:add-attribute>
	</p:pipeline>
	
	<p:pipeline type="z:not-found">
		<p:identity>
			<p:input port="source">
				<p:inline>
					<c:response status="404">
						<c:header name="X-Powered-By" value="XProc using XML Calabash"/>
						<c:header name="Server" value="XProc-Z"/>
						<c:body content-type="application/xhtml+xml">
							<html xmlns="http://www.w3.org/1999/xhtml">
								<head>
									<title>Not Found</title>
								</head>
								<body>
									<h1>Not Found</h1>
									<p>The requested resource was not found.</p>
								</body>
							</html>
						</c:body>
					</c:response>
				</p:inline>
			</p:input>
		</p:identity>
	</p:pipeline>
	
	<p:pipeline type="z:parse-parameters">
		<p:xslt>
			<p:input port="stylesheet">
				<p:inline>
					<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:c="http://www.w3.org/ns/xproc-step" xmlns:xs="http://www.w3.org/2001/XMLSchema">
						<xsl:template match="/">
							<c:multipart>
								<xsl:call-template name="urldecode">
									<xsl:with-param name="urlencoded" select="/c:request/c:body[@content-type='application/x-www-form-urlencoded']"/>
								</xsl:call-template>
								<xsl:call-template name="urldecode">
									<xsl:with-param name="urlencoded" select="substring-after(/c:request/@href, '?')"/>
								</xsl:call-template>
							</c:multipart>
						</xsl:template>
						<xsl:template name="urldecode">
							<xsl:param name="urlencoded"/>
							<xsl:if test="$urlencoded">
								<xsl:analyze-string select="translate($urlencoded, '+', ' ')" regex="([^&amp;]+)">
									<xsl:matching-substring>
										<xsl:variable name="name">
											<xsl:call-template name="decode">
												<xsl:with-param name="text" select="substring-before(., '=')"/>
											</xsl:call-template>
										</xsl:variable>
										<xsl:variable name="value">
											<xsl:call-template name="decode">
												<xsl:with-param name="text" select="substring-after(., '=')"/>
											</xsl:call-template>
										</xsl:variable>
										<c:body id="{$name}">
											<xsl:value-of select="$value"/>
										</c:body>
									</xsl:matching-substring>
								</xsl:analyze-string>
							</xsl:if>
						</xsl:template>
						<xsl:template name="decode">
							<xsl:param name="text"/>
							<!-- unescape and decode UTF-8 -->
							<!-- http://en.wikipedia.org/wiki/UTF-8#Description -->
							<xsl:variable name="ascii" select="substring-before(concat($text, '%'), '%')"/>
							<xsl:value-of select="$ascii"/>
							<xsl:variable name="encoded" select="substring-after($text, $ascii)"/>
							<xsl:if test="$encoded">
								<xsl:variable name="hex-digit-1">
									<xsl:call-template name="get-hex-value">
										<xsl:with-param name="hex-digit" select="substring($encoded, 2, 1)"/>
									</xsl:call-template>
								</xsl:variable>
								<xsl:variable name="hex-digit-2">
									<xsl:call-template name="get-hex-value">
										<xsl:with-param name="hex-digit" select="substring($encoded, 3, 1)"/>
									</xsl:call-template>
								</xsl:variable>
								<xsl:choose>
									<!-- 1 byte character (=ASCII) -->
									<xsl:when test="$hex-digit-1 &lt; 12">
										<xsl:value-of select="codepoints-to-string(xs:integer($hex-digit-1 * 16 + $hex-digit-2))"/>
										<xsl:call-template name="decode">
											<xsl:with-param name="text" select="substring($encoded, 4)"/>
										</xsl:call-template>
									</xsl:when>
									<xsl:otherwise>
										<!-- more than a single byte character -->
										<xsl:variable name="hex-digit-3">
											<xsl:call-template name="get-hex-value">
												<xsl:with-param name="hex-digit" select="substring($encoded, 5, 1)"/>
											</xsl:call-template>
										</xsl:variable>
										<xsl:variable name="hex-digit-4">
											<xsl:call-template name="get-hex-value">
												<xsl:with-param name="hex-digit" select="substring($encoded, 6, 1)"/>
											</xsl:call-template>
										</xsl:variable>
										<xsl:choose>
											<!-- binary 1100xxxx  ⇒ 2 byte character -->
											<xsl:when test="$hex-digit-1 = 12 or $hex-digit-1 = 13 ">
												<!-- UTF-8-decode -->
												<xsl:variable name="code-point" select="
													xs:integer(
														($hex-digit-1 - 12) * 1024 + 
														$hex-digit-2 * 64 + 
														($hex-digit-3 - 8) * 16 +
														($hex-digit-4)
													)
												"/>
												<xsl:value-of select="codepoints-to-string($code-point)"/>
												<xsl:call-template name="decode">
													<xsl:with-param name="text" select="substring($encoded, 7)"/>
												</xsl:call-template>
											</xsl:when>
											<xsl:otherwise>
												<!-- 3 or 4 byte character -->
												<xsl:variable name="hex-digit-5">
													<xsl:call-template name="get-hex-value">
														<xsl:with-param name="hex-digit" select="substring($encoded, 8, 1)"/>
													</xsl:call-template>
												</xsl:variable>
												<xsl:variable name="hex-digit-6">
													<xsl:call-template name="get-hex-value">
														<xsl:with-param name="hex-digit" select="substring($encoded, 9, 1)"/>
													</xsl:call-template>
												</xsl:variable>
												<xsl:choose>
													<!-- binary 1110xxxx ⇒ 3 byte character -->
													<xsl:when test="$hex-digit-1 = 14 ">
														<!-- UTF-8-decode -->
														<!-- 1110 xxxx 	10xx xxxx 	10xx xxxx -->
														<xsl:variable name="code-point" select="
															xs:integer(
																$hex-digit-2 * 4096 +
																($hex-digit-3 - 8) * 1024 +
																$hex-digit-4 * 64 + 
																($hex-digit-5 - 8) * 16 +
																($hex-digit-6)
															)
														"/>
														<xsl:value-of select="codepoints-to-string($code-point)"/>
														<xsl:call-template name="decode">
															<xsl:with-param name="text" select="substring($encoded, 10)"/>
														</xsl:call-template>
													</xsl:when>
													<xsl:when test="$hex-digit-1 =15 "><!-- 1111xxxx  ⇒ 4 byte character -->
														<xsl:variable name="hex-digit-7">
															<xsl:call-template name="get-hex-value">
																<xsl:with-param name="hex-digit" select="substring($encoded, 11, 1)"/>
															</xsl:call-template>
														</xsl:variable>
														<xsl:variable name="hex-digit-8">
															<xsl:call-template name="get-hex-value">
																<xsl:with-param name="hex-digit" select="substring($encoded, 12, 1)"/>
															</xsl:call-template>
														</xsl:variable>
														<!-- UTF-8-decode -->
														<!-- 1111	0xxx 	10xx	xxxx 	10xx	xxxx 	10xx	xxxx -->
														<xsl:variable name="code-point" select="
															xs:integer(
																$hex-digit-2 * 262144 +
																($hex-digit-3 - 8) * 65536 +
																$hex-digit-4 * 4096 + 
																($hex-digit-5 - 8) * 1024 +
																$hex-digit-6 * 64 + 
																($hex-digit-7 - 8) * 16 +
																($hex-digit-8)
															)
														"/>
														<xsl:value-of select="codepoints-to-string($code-point)"/>
														<xsl:call-template name="decode">
															<xsl:with-param name="text" select="substring($encoded, 13)"/>
														</xsl:call-template>
													</xsl:when>
												</xsl:choose>
											</xsl:otherwise>
										</xsl:choose>
									</xsl:otherwise>
								</xsl:choose>
							</xsl:if>
						</xsl:template>
						<xsl:template name="get-hex-value">
							<xsl:param name="hex-digit"/>
							<xsl:variable name="values" select="'0123456789ABCDEF'"/>
							<xsl:value-of select="string-length(substring-before($values, $hex-digit))"/>
						</xsl:template>
					</xsl:stylesheet>
				</p:inline>
			</p:input>
			<p:input port="parameters">
				<p:empty/>
			</p:input>
		</p:xslt>
	</p:pipeline>	
	
	<!-- shorthand for executing an XSLT  -->
	<p:declare-step type="z:transform" name="transform">
		
		<p:input port="source"/>
		<p:output port="result"/>
		<p:input port="parameters" kind="parameter"/>
		
		<p:option name="xslt" required="true"/>
		
		<p:load name="load-stylesheet">
			<p:with-option name="href" select="$xslt"/>
		</p:load>
		
		<p:xslt name="execute-xslt">
			<p:input port="source">
				<p:pipe step="transform" port="source"/>
			</p:input>
			<p:input port="stylesheet">
				<p:pipe step="load-stylesheet" port="result"/>
			</p:input>
		</p:xslt>
	</p:declare-step>
	
	<p:declare-step type="z:get-file-upload" name="get-file-upload">
		<p:input port="source"/>
		<p:output port="result"/>
		<p:option name="field-name" required="true"/>
		<p:filter>
			<p:with-option name="select" select="
				concat(
					'/c:request/c:multipart/c:body[starts-with(@disposition,',
					codepoints-to-string(39),
					'form-data; name=',
					codepoints-to-string(34),
					$field-name,
					codepoints-to-string(34),
					codepoints-to-string(39),
					')]/*'
				)
			"/>	
		</p:filter>
	</p:declare-step>

	<p:declare-step type="z:zip-sequence" name="zip-sequence">
		<p:input port="source" sequence="true"/>
		<p:output port="result"/>
		<p:input port="parameters" kind="parameter"/>
		<!-- create a zip manifest  -->
		<!-- convert each document in the sequence into a c:entry of a c:zip-manifest -->
		<p:for-each>
			<p:template>
				<p:input port="template">
					<p:inline>
						<c:entry name="{substring-after(base-uri(), 'file:/')}" href="{base-uri()}"/>
					</p:inline>
				</p:input>
			</p:template>
		</p:for-each>
		<!-- wrap entries into a manifest -->
		<p:wrap-sequence wrapper="c:zip-manifest" name="manifest"/>
		<!-- get global parameters to find a safe place to write a temp file -->
		<p:parameters name="global-parameters">
			<p:input port="parameters">
				<p:pipe step="zip-sequence" port="parameters"/>
			</p:input>
		</p:parameters>
		<p:group>
			<!-- We need an absolute URI for the temporary zip file, based on the "realPath" parameter -->
			<p:variable name="zip-file-name" select="
				concat(
					'file:', 
					/c:param-set/c:param[@name='realPath'][@namespace='tag:conaltuohy.com,2015:servlet-context']/@value,
					'/zip-sequence.zip'
				)
			">
				<p:pipe step="global-parameters" port="result"/>
			</p:variable>
			<!-- zip up the sequence of documents according to the manifest and stash it in the temporary file -->
			<zip name="zip" xmlns="http://exproc.org/proposed/steps" command="create">
				<p:with-option name="href" select="$zip-file-name"/>
				<p:input port="source">
					<p:pipe step="zip-sequence" port="source"/>
				</p:input>
				<p:input port="manifest">
					<p:pipe step="manifest" port="result"/>
				</p:input>
			</zip>
			<!-- create a request document to read the temporary file back in -->
			<p:identity>
				<p:input port="source">
					<p:inline>
						<c:request method="get"/>
					</p:inline>
				</p:input>
			</p:identity>
			<p:add-attribute match="/c:request" attribute-name="href">
				<p:with-option name="attribute-value" select="$zip-file-name"/>
			</p:add-attribute>
			<!-- Read ZIP file back in. NB explicit dependency on preceding step -->
			<p:http-request cx:depends-on="zip" xmlns:cx="http://xmlcalabash.com/ns/extensions"/>
		</p:group>
	</p:declare-step>
</p:library>
