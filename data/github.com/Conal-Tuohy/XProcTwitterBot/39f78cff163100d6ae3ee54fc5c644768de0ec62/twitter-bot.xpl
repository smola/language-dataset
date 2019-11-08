<p:declare-step version="1.0" 
	name="bot"
	type="twitter:bot"
	xmlns:p="http://www.w3.org/ns/xproc" 
	xmlns:c="http://www.w3.org/ns/xproc-step" 
	xmlns:cx="http://xmlcalabash.com/ns/extensions"
	xmlns:fn="http://www.w3.org/2005/xpath-functions"
	xmlns:twitter="tag:conaltuohy.com,2015:twitter"
	xmlns:digitalnz="tag:conaltuohy.com,2015:digitalnz"
	xmlns:paperspast="tag:conaltuohy.com,2015:paperspast"
	xmlns:utility="tag:conaltuohy.com,2015:utility"
	xmlns:xs="http://www.w3.org/2001/XMLSchema"
	xmlns:j="http://marklogic.com/json" 
	xmlns:html="http://www.w3.org/1999/xhtml">
	<p:import href="http://xmlcalabash.com/extension/steps/library-1.0.xpl"/>
	
	<p:input port="parameters" kind="parameter"/>

	<digitalnz:tweet-anniversary-illustration name="one-hundred-and-twenty-five" years-ago="125"/>
	<digitalnz:tweet-anniversary-illustration name="one-hundred" years-ago="100"/>
	<digitalnz:tweet-anniversary-illustration name="seventy-five" years-ago="75"/>
	
	<p:declare-step type="utility:dump" name="dump">
		<p:input port="source" sequence="true"/>
		<p:output port="result" sequence="true">
			<p:pipe step="dump" port="source"/>
		</p:output>
		<p:option name="href"/>
		<p:for-each>
			<p:store>
				<p:with-option name="href" select="$href"/>
			</p:store>
		</p:for-each>
	</p:declare-step>
	
	<p:declare-step type="digitalnz:get-anniversary-illustration" name="get-anniversary-illustration">
		<p:option name="years-ago" required="true"/>
		<p:input port="parameters" kind="parameter"/>
		<p:output port="result" sequence="true"/>
		<!-- get a page from n years ago containing an illustration -->
		<p:template>
			<p:with-param name="date" select="
				substring(
					string(
						current-date() - xs:yearMonthDuration(
							concat(
								'P', 
								$years-ago, 
								'Y'
							)
						)
					), 
					1, 
					10
				)
			"/>
			<p:input port="source"><p:empty/></p:input>
			<p:input port="template">
				<p:inline>
					<query>
						<text></text>
						<per_page>100</per_page><!-- maximum=100, default=20 -->
						<and>
							<date>{$date}</date>
							<category>Images</category>
							<content_partner>National Library of New Zealand</content_partner>
							<primary_collection>Papers Past</primary_collection>
						</and>
					</query>
				</p:inline>
			</p:input>
		</p:template>
		<digitalnz:search name="search-results"/>
		<p:filter name="illustration" select="/search/results/result[last()]"/>
	</p:declare-step>

	<p:declare-step type="digitalnz:tweet-anniversary-illustration" name="tweet-illustrations">
		<p:option name="years-ago" required="true"/>
		<p:input port="parameters" kind="parameter"/>
		<twitter:get-configuration/>
		<p:group>
			<p:variable 
				name="max-url-length" 
				select="number(/c:body/j:json/j:short_005furl_005flength_005fhttps)"/>
				<!--
			<cx:message>
				<p:with-option name="message" select="concat('max-url-length: ', $max-url-length)"/>
			</cx:message>
			-->
			<digitalnz:get-anniversary-illustration>
				<p:with-option name="years-ago" select="$years-ago"/>
			</digitalnz:get-anniversary-illustration>
			<p:for-each name="illustration">
				<!-- download the image -->
				<paperspast:get-image>
					<p:with-option name="href" select="/result/landing-url"/>
				</paperspast:get-image>
				<!-- upload it to Twitter -->
				<twitter:upload-media/>
				<!-- construct a tweet that references the image uploaded to Twitter -->
				<p:group>
					<p:variable name="media-id" select="substring-after(substring-before(/c:body, ','), ':')"/>
					<p:identity>
						<p:input port="source">
							<p:pipe step="illustration" port="current"/>
						</p:input>
					</p:identity>
					<p:group>
						<!-- Strip leading "Untitled Illustration", and insert a zero-width space after every "." 
						in the headline, to defeat Twitter's URL recognition -->
						<p:variable name="hashtag" select="concat(' #', $years-ago, 'years ')"/>
						<p:variable name="headline" select="
							replace(
								replace(
									/result/title, 
									'^Untitled Illustration ', ''
								), 
								'\.', '.&#8203;'
							)
						"/>
						<!-- compute maximum length of headline the max tweet length -->
						<p:variable name="max-headline-length" select="140 - string-length($hashtag) - $max-url-length"/>
						<!-- status text is the headline, truncated if necessary, and if so, with an ellipsis, followed by the page URI -->
						<p:variable name="status" select="
							concat(
								(
									if (string-length($headline) &gt; $max-headline-length) then concat(
										substring(
											$headline, 
											1, 
											$max-headline-length - 1
										),
										'…'
									)
									else $headline
								),
								$hashtag,
								/result/source-url
							)
						"/>
						<twitter:tweet>
							<p:with-option name="status" select="$status"/>
							<p:with-option name="media-ids" select="$media-id"/>
						</twitter:tweet>
						<p:sink/>
					</p:group>
				</p:group>
			</p:for-each>
		</p:group>
	</p:declare-step>
	
	<p:declare-step type="twitter:upload-media">
		<p:input port="parameters" kind="parameter"/>
		<p:input port="source"/>
		<p:output port="result"/>
		<p:add-attribute match="/*" attribute-name="disposition">
			<p:with-option name="attribute-value" select="
				concat(
					'form-data; name=',
					codepoints-to-string(34),
					'media_data',
					codepoints-to-string(34)
				)
			"/>
		</p:add-attribute>
		<p:add-attribute match="/*" attribute-name="content-type" attribute-value="application/octet-stream"/>
		<p:wrap match="/*" wrapper="c:multipart"/>
		<p:add-attribute match="/*" attribute-name="content-type" attribute-value="multipart/form-data"/>
		<p:add-attribute match="/*" attribute-name="boundary" attribute-value="___"/>
		<p:wrap match="/*" wrapper="c:request"/>
		<p:add-attribute match="/*" attribute-name="override-content-type" attribute-value="text/json"/>
		<p:add-attribute match="/*" attribute-name="method" attribute-value="POST"/>
		<p:add-attribute match="/*" attribute-name="href" attribute-value="https://upload.twitter.com/1.1/media/upload.json"/>
		<twitter:sign-request/>
		<p:http-request/>
	</p:declare-step>
	

	<p:declare-step type="paperspast:get-image">
		<p:input port="source"/>
		<p:output port="result"/>
		<!-- href specifies the "landing page" of the image -->
		<p:option name="href" required="true"/>
		<p:identity>
			<p:input port="source">
				<p:inline>
					<c:request method="GET"/>
				</p:inline>
			</p:input>
		</p:identity>
		<p:add-attribute match="/c:request" attribute-name="href">
			<p:with-option name="attribute-value" select="$href"/>
		</p:add-attribute>
		<p:http-request/>
		<p:unescape-markup content-type="text/html"/>
		<!-- now find the url of the image, within this landing page, and use it to download the image -->
		<p:group>
			<p:variable name="image-url" select="concat('https://paperspast.natlib.govt.nz', (//html:img[@alt='Article image'])[1]/@src)"/>
			<p:identity>
				<p:input port="source">
					<p:inline>
						<c:request method="GET"/>
					</p:inline>
				</p:input>
			</p:identity>
			<p:add-attribute match="/c:request" attribute-name="href">
				<p:with-option name="attribute-value" select="$image-url"/>
			</p:add-attribute>
			<p:http-request/>
		</p:group>
	</p:declare-step>

	
	

	
			<!-- code to trim all except "a" and "d" URI parameters from a paperspast article page, which we should because
			we might want to tweet this URL--><!--

		<p:group>
			<p:variable name="result-page-uri"
				select="(//html:div[@class='search-results']/html:p/html:a[not(string(.)='Untitled')])[1]/@href"/>
			<p:www-form-urldecode>
				<p:with-option name="value" select="substring-after($result-page-uri, '?')"/>
			</p:www-form-urldecode>

			<p:group>
				<p:variable name="short-page-uri" select="
					concat(
						'http://paperspast.natlib.govt.nz',
						substring-before($result-page-uri, '?'),
						'?a=', encode-for-uri(/c:param-set/c:param[@name='a']/@value),
						'&amp;d=', encode-for-uri(/c:param-set/c:param[@name='d']/@value)
					)
				"/>
			</p:group>
		</p:group>
-->
	

	<p:declare-step type="twitter:followers">
		<p:input port="parameters" kind="parameter"/>
		<p:output port="result"/>
		<p:identity name="list-followers">
			<p:input port="source">
				<p:inline>
					<c:request 
						method="GET" 
						href="https://api.twitter.com/1.1/followers/list.json">
					</c:request>
				</p:inline>
			</p:input>
		</p:identity>	
		<twitter:sign-request/>
		<p:http-request/>
	</p:declare-step>
	
	<p:declare-step type="twitter:get-configuration">
		<p:input port="parameters" kind="parameter"/>
		<p:output port="result"/>
		<p:identity name="config-request">
			<p:input port="source">
				<p:inline>
					<c:request 
						method="GET" 
						href="https://api.twitter.com/1.1/help/configuration.json">
					</c:request>
				</p:inline>
			</p:input>
		</p:identity>	
		<twitter:sign-request/>
		<p:http-request/>
		<p:unescape-markup content-type="application/json" encoding="base64" charset="UTF-8"/>
	</p:declare-step>
	
	<p:declare-step type="twitter:tweet">
		<p:input port="parameters" kind="parameter"/>
		<p:output port="result"/>
		<p:option name="status" required="true"/>
		<p:option name="media-ids"/>
		<p:in-scope-names name="parameters"/>
		<p:xslt name="status-update-request">
			<p:input port="source">
				<p:inline>
					<c:request 
						override-content-type="text/json"
						method="POST" 
						href="https://api.twitter.com/1.1/statuses/update.json"/>
				</p:inline>
			</p:input>
			<p:input port="parameters">
				<p:pipe step="parameters" port="result"/>
			</p:input>
			<p:input port="stylesheet">
				<p:inline>
					<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:c="http://www.w3.org/ns/xproc-step">
						<xsl:param name="status"/>
						<xsl:param name="media-ids"/>
						<xsl:template match="c:request">
							<xsl:copy>
								<xsl:copy-of select="@*"/>
								<c:body content-type="application/x-www-form-urlencoded">
									<xsl:value-of select="
										concat(
											'status=', 
											encode-for-uri($status)
									)"/>
									<xsl:if test="$media-ids">
										<xsl:value-of select="
											concat(
												'&amp;media_ids=',
												encode-for-uri($media-ids)
											)
										"/>
									</xsl:if>
								</c:body>
							</xsl:copy>
						</xsl:template>
					</xsl:stylesheet>
				</p:inline>
			</p:input>
		</p:xslt>
		<twitter:sign-request/>
		<p:http-request/>
	</p:declare-step>
	
	<p:declare-step type="twitter:www-form-urldecode">
		<!-- does not choke on an empty value -->
		<p:output port="result"/>
		<p:option name="value" required="true"/>
		<p:choose>
			<p:when test="string-length($value)=0">
				<p:identity>
					<p:input port="source">
						<p:inline><c:param-set/></p:inline>
					</p:input>
				</p:identity>
			</p:when>
			<p:otherwise>
				<p:www-form-urldecode>
					<p:with-option name="value" select="$value"/>
				</p:www-form-urldecode>
			</p:otherwise>
		</p:choose>
	</p:declare-step>

	<p:declare-step type="twitter:sign-request" name="sign-request">
		<!-- a c:request document -->
		<p:input port="source"/>
		<p:output port="result"/>
		<p:input port="parameters" kind="parameter"/>
		<p:parameters name="credentials">
			<p:input port="parameters">
				<p:pipe port="parameters" step="sign-request"/>
			</p:input>
		</p:parameters>
		<p:group>
			<p:variable name="consumer-key" select="/c:param-set/c:param[@name='consumer-key']/@value">
				<p:pipe step="credentials" port="result"/>
			</p:variable>
			<p:variable name="consumer-secret" select="/c:param-set/c:param[@name='consumer-secret']/@value">
				<p:pipe step="credentials" port="result"/>
			</p:variable>
			<p:variable name="access-token" select="/c:param-set/c:param[@name='access-token']/@value">
				<p:pipe step="credentials" port="result"/>
			</p:variable>
			<p:variable name="access-token-secret" select="/c:param-set/c:param[@name='access-token-secret']/@value">
				<p:pipe step="credentials" port="result"/>
			</p:variable>
			
			<!-- Generate OAuth parameter set -->
			<p:variable name="oauth_signature_method" select=" 'HMAC-SHA1' "/>
			<p:variable name="duration-since-unix-epoch" select="
				fn:current-dateTime() - xs:dateTime('1970-01-01T00:00:00Z')
			"/>
			<p:variable name="oauth_consumer_key" select="$consumer-key"/>
			<p:variable name="oauth_nonce" select="p:system-property('p:episode')"/>
			<p:variable name="oauth_timestamp" select="
				string(
					xs:integer(fn:seconds-from-duration($duration-since-unix-epoch)) +
					fn:minutes-from-duration($duration-since-unix-epoch) * 60 +
					fn:hours-from-duration($duration-since-unix-epoch) * 60 * 60 + 
					fn:days-from-duration($duration-since-unix-epoch) * 24 * 60 * 60
				)
			"/>
			<p:variable name="oauth_token" select="$access-token"/>
			<p:variable name="oauth_version" select=" '1.0' "/>
			<p:variable name="url-parameter-string" select="substring-after(/c:request/@href, '?')">
				<p:pipe step="sign-request" port="source"/>
			</p:variable>
			<p:variable name="post-parameter-string" select="/c:request/c:body[@content-type='application/x-www-form-urlencoded']">
				<p:pipe step="sign-request" port="source"/>
			</p:variable>
			<!-- read variables into a param-set -->
			<p:in-scope-names name="oauth-and-other-variables"/>
			<!-- throw out all but the oauth_* parameters -->
			<p:delete name="oauth-parameters"
				match="/c:param-set/c:param[not(starts-with(@name, 'oauth_'))]">
				<p:input port="source">
					<p:pipe step="oauth-and-other-variables" port="result"/>
				</p:input>
			</p:delete>
			<!-- Read the URL parameters into a parameter set -->
			<p:www-form-urldecode name="url-parameters">
				<p:with-option name="value" select="$url-parameter-string"/>
			</p:www-form-urldecode>
			<!-- Read the POST parameters into a parameter set -->
			<p:www-form-urldecode name="post-parameters">
				<p:with-option name="value" select="$post-parameter-string"/>	
			</p:www-form-urldecode>
			<!-- merge the OAuth, POST, and URL parameters into a single set -->
			<p:wrap-sequence wrapper="c:param-set">
				<p:input port="source" select="/c:param-set/c:param">
					<p:pipe step="url-parameters" port="result"/>
					<p:pipe step="post-parameters" port="result"/>
					<p:pipe step="oauth-parameters" port="result"/>
				</p:input>
			</p:wrap-sequence>
			<!-- URI encode the parameters' keys and values -->
			<p:string-replace match="@name | @value" replace="encode-for-uri(.)"/>
			<!-- sort the parameters -->
			<p:xslt name="sorted-parameters">
				<p:input port="parameters">
					<p:empty/>
				</p:input>
				<p:input port="stylesheet">
					<p:inline>
						<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0"
							xmlns:c="http://www.w3.org/ns/xproc-step">
							<xsl:template match="/c:param-set">
								<xsl:copy>
									<xsl:for-each select="c:param">
										<xsl:sort select="@name"/>
										<xsl:sort select="@value"/>
										<xsl:copy-of select="."/>
									</xsl:for-each>
								</xsl:copy>
							</xsl:template>
						</xsl:stylesheet>
					</p:inline>
				</p:input>
			</p:xslt>
			<p:group>
				<!-- encode parameters into a parameter string -->
				<p:variable name="parameter-string" select="
					string-join(
						for $param in /c:param-set/c:param return concat($param/@name, '=', $param/@value),
						'&amp;'
					)
				"/>
				<!-- encode the request method, base URI, and parameter string into a "signature base string" to be signed -->
				<p:variable name="signature-base-string" select="
					concat(
						/c:request/@method,
						'&amp;',
						encode-for-uri(
							substring-before(
								concat(/c:request/@href, '?'),
								'?'
							)
						),
						'&amp;',
						encode-for-uri($parameter-string)
					)
				">
					<p:pipe step="sign-request" port="source"/>
				</p:variable>
				<!-- assemble the signing key -->
				<p:variable name="signing-key" select="concat(
					encode-for-uri($consumer-secret), 
					'&amp;', 
					encode-for-uri($access-token-secret)
				)"/>
				<!-- create a document to store the signature -->
				<p:identity>
					<p:input port="source">
						<p:inline>
							<signature value=""/>
						</p:inline>
					</p:input>
				</p:identity>
				<!-- hash the signature base string with the signing key, storing the resulting signature -->
				<p:hash name="signature" match="/signature/@value" algorithm="cx:hmac">
					<p:with-param name="cx:accessKey" select="$signing-key"/>
					<p:with-option name="value" select="$signature-base-string"/>
				</p:hash>
				<p:group>
					<p:variable name="oauth_signature" select="/signature/@value"/>
					<p:in-scope-names name="all-variables"/>
					<!-- URI encode all the values of the OAuth parameters -->
					<p:string-replace name="oauth-header-components" match="@value" replace="encode-for-uri(.)">
						<p:input port="source">
							<p:pipe step="all-variables" port="result"/>
						</p:input>
					</p:string-replace>
					<!-- Format the OAuth parameters into an Authorization request header -->
					<p:template name="authorization-header">
						<p:input port="parameters">
							<p:pipe step="oauth-header-components" port="result"/>
						</p:input>
						<p:input port="template">
							<p:inline>
								<c:header name="Authorization"
									value='OAuth oauth_consumer_key="{$oauth_consumer_key}", oauth_nonce="{$oauth_nonce}", oauth_signature="{$oauth_signature}", oauth_signature_method="{$oauth_signature_method}", oauth_timestamp="{$oauth_timestamp}", oauth_token="{$oauth_token}", oauth_version="{$oauth_version}"'
								/>
							</p:inline>
						</p:input>
					</p:template>
					<!-- insert the Authorization header into the HTTP request -->
					<p:insert position="first-child">
						<p:input port="source">
							<p:pipe step="sign-request" port="source"/>
						</p:input>
						<p:input port="insertion">
							<p:pipe step="authorization-header" port="result"/>
						</p:input>
					</p:insert>
					<p:add-attribute match="/*" attribute-name="twitter:parameter-string">
						<p:with-option name="attribute-value" select="$parameter-string"/>
					</p:add-attribute>
				</p:group>
			</p:group>
		</p:group>
	</p:declare-step>	
	<p:declare-step type="digitalnz:search" name="search">
		<!-- digital NZ API key is passed as a parameter called "digitalnz-api-key" -->
		<p:input port="parameters" kind="parameter"/>
		<p:input port="source"/>
		<!-- source document is a query in XML format expressing the query language described here: http://www.digitalnz.org/developers/api-docs-v3/search-records-api-v3
		
		The root element must be <query>.
		The <query> element may have child elements <and> <or> and <without> which are logical operators grouping facet restrictions.
		The child elements of those operators are the names of facets (e.g. <content_partner>); their text contents are the values of those facets.
		Other child elements of <query> are other the query parameters, i.e. <text>, <page>, <per_page>, <facets>, <facets_page>, <facets_per_page>, <sort>, <direction>, and <geo_bbox>
		-->
		<p:output port="result">
			<p:pipe step="digitalnz-response" port="result"/>
		</p:output>
		<p:xslt name="digitalnz-request">
			<p:input port="stylesheet">
				<p:inline>
					<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0"
						xmlns:c="http://www.w3.org/ns/xproc-step">
						<xsl:param name="digitalnz-api-key"/>
						<xsl:template match="/query">
							<c:request method="GET">
								<xsl:attribute name="href">
									<xsl:text>http://api.digitalnz.org/v3/records.xml?api_key=</xsl:text>
									<xsl:value-of select="$digitalnz-api-key"/>
									<xsl:apply-templates select="*"/>
								</xsl:attribute>
							</c:request>
						</xsl:template>
						<xsl:template match="/query/*[*]">
							<xsl:apply-templates select="*"/>
						</xsl:template>
						<xsl:template match="/query/*/*">
							<xsl:value-of select="concat(
								'&amp;',
								local-name(..),
								'[',
								local-name(.),
								']=',
								encode-for-uri(.)
							)"/>
						</xsl:template>
						<xsl:template match="/query/*[not(*)]">
							<xsl:value-of select="concat(
								'&amp;',
								local-name(.),
								'=',
								encode-for-uri(.)
							)"/>
						</xsl:template>
					</xsl:stylesheet>
				</p:inline>
			</p:input>
		</p:xslt>
		<!-- invoke query -->
		<p:http-request name="digitalnz-response"/>
		<!-- 
		debugging output
		<p:store href="tmp-response.xml">
			<p:input port="source">
				<p:pipe step="digitalnz-response" port="result"/>
			</p:input>
		</p:store>
		<p:store href="tmp-request.xml">
			<p:input port="source">
				<p:pipe step="digitalnz-request" port="result"/>
			</p:input>
		</p:store>
		-->
	</p:declare-step>
	
</p:declare-step>
