xquery version "1.0-ml";

import module namespace atompub="http://www.marklogic.com/modules/atompub"
       at "/AtomPub/atompub.xqy";

(:
import module namespace alert="http://marklogic.com/xdmp/alert"
       at "/MarkLogic/alert.xqy";
:)

declare default function namespace "http://www.w3.org/2005/xpath-functions";

declare namespace mla="http://www.marklogic.com/ns/atompub";
declare namespace app="http://www.w3.org/2007/app";
declare namespace atom="http://www.w3.org/2005/Atom";
declare namespace f="http://www.marklogic.com/ns/local-functions";

declare option xdmp:mapping "false";

declare variable $path as xs:string external;
declare variable $content-type as xs:string external;
declare variable $is-xml as xs:boolean external;
declare variable $body as document-node() external;

declare variable $CONFIG := doc("/etc/configuration.xml")/mla:configuration;
declare variable $binary-collection := "http://www.marklogic.com/collections/atompub/binary";
declare variable $entry-collection := "http://www.marklogic.com/collections/atompub/atom";

declare function f:post-entry() as element(mla:response) {
  let $slug := xdmp:get-request-header("Slug")
  let $uri := atompub:new-uri($path, $slug)
  let $alt-link
    := <link xmlns="http://www.w3.org/2005/Atom"
             rel="alternate"
             href="{concat($CONFIG/mla:root,$uri)}"/>
  let $edit-link
    := <link xmlns="http://www.w3.org/2005/Atom"
             rel="edit"
             href="{concat($CONFIG/mla:edit-root,$uri,'.atom')}"/>
  let $svc-link
    := <link xmlns="http://www.w3.org/2005/Atom"
             rel="service"
	     type="application/atomsvc+xml"
             href="{concat($CONFIG/mla:edit-root,$CONFIG/mla:weblog-path,
                           xdmp:get-current-user(),'/service.xml')}"/>
  return
    <mla:response>
      <mla:code>201</mla:code>
      <mla:message>Created</mla:message>
      <mla:uri>{concat($CONFIG/mla:edit-root, $uri, ".atom")}</mla:uri>
      <mla:body>{f:post($uri, $body/*, ($alt-link, $edit-link, $svc-link))}</mla:body>
    </mla:response>
};

declare function f:post-media() as element(mla:response) {
  let $slug := xdmp:get-request-header("Slug")
  let $uri := atompub:new-uri($path, $slug)
  let $ext := concat(".", atompub:mime-type-extension($content-type))
  let $content
    := <content xmlns="http://www.w3.org/2005/Atom"
                type="{$content-type}"
		src="{concat($CONFIG/mla:root,$uri,$ext)}"/>
  let $edit-media-link
    := <link xmlns="http://www.w3.org/2005/Atom"
             rel="edit-media"
	     href="{concat($CONFIG/mla:edit-root,$uri,$ext)}"/>
  let $edit-link
    := <link xmlns="http://www.w3.org/2005/Atom"
             rel="edit"
	     href="{concat($CONFIG/mla:edit-root,$uri,'.atom')}"/>
  let $extra := ($content, $edit-media-link, $edit-link)
  let $editor := concat("weblog-editor-",xdmp:get-current-user())
  let $entry :=
    <entry xmlns="http://www.w3.org/2005/Atom">
      <title>{if (empty($slug)) then "Default Title" else $slug}</title>
      <id>{concat($CONFIG/mla:root,$uri,'.atom')}</id>
      <author><name>{xdmp:get-current-user()}</name></author>
      <published>{atompub:now()}</published>
      <updated>{atompub:now()}</updated>
      <summary type="text">This is a media entry</summary>
    </entry>
  return
    (xdmp:document-insert(concat($uri,$ext), $body,
                          (xdmp:permission("weblog-reader", "read"),
			   xdmp:permission($editor, "update")),
			   (concat($binary-collection,$path))),
     atompub:debug(concat("inserted ", $uri, $ext, $editor)),
     <mla:response>
       <mla:code>201</mla:code>
       <mla:message>Created</mla:message>
       <id>{concat($CONFIG/mla:root,$uri,'.atom')}</id>
       <mla:uri>{concat($CONFIG/mla:edit-root, $uri, ".atom")}</mla:uri>
       <mla:body>{f:post($uri, $entry, $extra)}</mla:body>
     </mla:response>,
     xdmp:document-set-property(concat($uri, ".atom"), $content),
     xdmp:document-set-property(concat($uri, ".atom"), $edit-media-link),
     xdmp:document-set-property(concat($uri, ".atom"), $edit-link),
     xdmp:document-set-property(concat($uri, $ext),
                            <mla:content-type>{$content-type}</mla:content-type>)
)
};

declare function f:post($uri as xs:string,
			$entry as element(atom:entry),
		        $extra as element()*)
        as element(atom:entry)
{
  let $augentry :=
    <entry xmlns="http://www.w3.org/2005/Atom">
      { $entry/@* }
      { $entry/*[not(self::atom:id)
                 and not(self::atom:published)
		 and not(self::atom:updated)
		 and not(self::app:edited)
		 and not(self::atom:link)] }
      <id>{concat($CONFIG/mla:root, $uri,".atom")}</id>
      <published>{atompub:now()}</published>
      <updated>{atompub:now()}</updated>
      <app:edited>{atompub:now()}</app:edited>
      { $entry/atom:link[@rel != 'edit'
                         and @rel != 'edit-media'
                         and @rel != 'alternate'
                         and @rel != 'self'] }
      { $extra }
    </entry>
  let $editor := concat("weblog-editor-",xdmp:get-current-user())
  return
    (xdmp:document-insert(concat($uri,".atom"),
                          $augentry,
			  (xdmp:permission("weblog-reader", "read"),
			   xdmp:permission($editor, "update")),
                          (concat($entry-collection,$path))),
(:
     alert:invoke-matching-actions("my-alert-config-uri", $augentry, <options/>),
:)
     $augentry)
};

if ($is-xml)
then
  f:post-entry()
else
  f:post-media()
