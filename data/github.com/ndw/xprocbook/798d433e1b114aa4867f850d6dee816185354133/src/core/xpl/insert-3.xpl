<p:pipeline xmlns:p="http://www.w3.org/ns/xproc"
            version="1.0">

  <p:documentation xmlns="http://docbook.org/ns/docbook">
    <para>As the first child:</para>
  </p:documentation>

  <p:pipeinfo>
    <doc>
      <div>
        <p>Play.</p>
      </div>
    </doc>
  </p:pipeinfo>

  <p:insert match="/doc/div" position="first-child">
    <p:input port="insertion">
      <p:inline>
        <p>Work.</p>
      </p:inline>
      <p:inline>
        <p>Sleep.</p>
      </p:inline>
      <p:inline>
        <p>Eat.</p>
      </p:inline>
    </p:input>
  </p:insert>
</p:pipeline>
