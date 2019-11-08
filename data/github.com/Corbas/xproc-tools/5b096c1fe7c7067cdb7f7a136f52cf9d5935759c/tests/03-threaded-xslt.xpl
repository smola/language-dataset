<?xml version="1.0" encoding="UTF-8"?>
<p:declare-step xmlns:p="http://www.w3.org/ns/xproc"
	xmlns:ccproc="http://www.corbas.co.uk/ns/xproc/steps" name="tester"
	xmlns:c="http://www.w3.org/ns/xproc-step" version="1.0">
	
	<p:documentation>Test processing of additional parameters.</p:documentation>
	
	<p:serialization port="result" indent="true"/>
	
	<p:input port="manifest">
		<p:document href="manifests/03-manifest-metadata.xml"></p:document>
	</p:input>
	
	<p:input port="source">
		<p:document href="data/test-03.xml"/>
	</p:input>

	<p:output port="result">
			<p:pipe port="result" step="threader"></p:pipe>
	</p:output>
	
	<p:import href="../src/load-sequence-from-file.xpl"/>
	<p:import href="../src/threaded-xslt.xpl"/>
	
	<ccproc:load-sequence-from-file name="loader">
		<p:input port="source">
			<p:pipe port="manifest" step="tester"/>
		</p:input>
	</ccproc:load-sequence-from-file>
	
	<ccproc:threaded-xslt name="threader">
		<p:input port="stylesheets">
			<p:pipe port="result" step="loader"></p:pipe>
		</p:input>
		<p:input port="source">
			<p:pipe port="source" step="tester"></p:pipe>
		</p:input>
		<p:input port="parameters">
			<p:empty/>
		</p:input>
	</ccproc:threaded-xslt>
	


</p:declare-step>