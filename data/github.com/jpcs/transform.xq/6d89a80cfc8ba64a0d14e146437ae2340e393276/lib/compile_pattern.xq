xquery version "3.0";

(:
 : Copyright (c) 2011 John Snelson
 :
 : Licensed under the Apache License, Version 2.0 (the "License");
 : you may not use this file except in compliance with the License.
 : You may obtain a copy of the License at
 :
 :     http://www.apache.org/licenses/LICENSE-2.0
 :
 : Unless required by applicable law or agreed to in writing, software
 : distributed under the License is distributed on an "AS IS" BASIS,
 : WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 : See the License for the specific language governing permissions and
 : limitations under the License.
 :)

module namespace pat = "http://snelson.org.uk/functions/patterns";

import module namespace p="patterns" at "patterns.xq";

declare default function namespace "http://www.w3.org/2005/xpath-functions";
declare namespace tfm = "http://snelson.org.uk/functions/transform";

declare function pat:compile-pattern(
  $pattern as xs:string,
  $resolver as function(xs:string) as xs:QName
) as function(*)
{
  let $parse := p:parse-Pattern($pattern)
  let $preds := $parse/PathPattern ! pat:compile-path(*,$resolver)
  return
    if(some $f in $preds satisfies $f instance of function(node()) as xs:boolean) then
      pat:node-type(pat:or($preds))
    else typeswitch($preds)
      case empty-sequence() return error(xs:QName("tfm:BADPATTERN"),
        "Invalid pattern: " || $parse)

      case function(*) return $preds

      case (function(element()) as xs:boolean)+ return
        pat:element-type(pat:or($preds))
      case (function(attribute()) as xs:boolean)+ return
        pat:attribute-type(pat:or($preds))
      case (function(document-node()) as xs:boolean)+ return
        pat:document-type(pat:or($preds))
      case (function(comment()) as xs:boolean)+ return
        pat:comment-type(pat:or($preds))
      case (function(text()) as xs:boolean)+ return
        pat:text-type(pat:or($preds))
      case (function(processing-instruction()) as xs:boolean)+ return
        pat:pi-type(pat:or($preds))

      default return
        pat:node-type(pat:or($preds))
};

declare %private function pat:or($preds)
{
  function($n) {
    some $f in $preds satisfies try { $f($n) } catch * { false() }
  }
};

declare %private function pat:compile-path($path,$resolver)
{
  typeswitch($path)
    case $root as element(RootPattern) return
      fold-left(pat:compile-step(?,?,$resolver),
        function($n as document-node()) as xs:boolean {
          empty($n/..)
        },
        $root/RelativePathPattern/(PatternStep|DescendantPattern))
    case $root as element(DescendantRootPattern) return
      fold-left(pat:compile-step(?,?,$resolver),
        function($n) {
          exists(root($n)/self::document-node())
        },
        $root/RelativePathPattern/(PatternStep|DescendantPattern))
    case element(RelativePathPattern) return
      fold-left(pat:compile-step(?,?,$resolver), (),
        $path/(PatternStep|DescendantPattern))
    default return error(xs:QName("tfm:BADPATH"),
      "Invalid path: " || $path)
};

declare %private function pat:node-type($f as function(node()) as xs:boolean)
{
  $f
};

declare %private function pat:document-type($f as function(document-node()) as xs:boolean)
{
  $f
};

declare %private function pat:element-type($f as function(element()) as xs:boolean)
{
  $f
};

declare %private function pat:attribute-type($f as function(attribute()) as xs:boolean)
{
  $f
};

declare %private function pat:comment-type($f as function(comment()) as xs:boolean)
{
  $f
};

declare %private function pat:text-type($f as function(text()) as xs:boolean)
{
  $f
};

declare %private function pat:pi-type($f as function(processing-instruction()) as xs:boolean)
{
  $f
};

declare %private function pat:compile-step($prev, $step, $resolver)
{
  typeswitch($step)
    case element(DescendantPattern) return
      function($n) {
        exists($n) and
          (empty($prev) or (some $a in $n/ancestor-or-self::* satisfies $prev($a)))
      }
    case element(PatternStep) return
      if($step/AttributeAxis or $step/NodeTest/KindTest/AttributeTest)
      then
        let $f := pat:compile-nodetest($prev,$step/NodeTest/*,pat:attribute-type#1,$resolver)
        return
          if($f instance of function(attribute()) as xs:boolean) then $f
          else function($n) { false() } (: You only get attributes on the attribute axis :)
      else pat:compile-nodetest($prev,$step/NodeTest/*,pat:element-type#1,$resolver)
    default return error(xs:QName("tfm:BADSTEP"),
      "Invalid step: " || $step)
};

declare %private function pat:compile-nodetest($prev, $nt, $default-type, $resolver)
{
  typeswitch($nt)
    case element(KindTest) return
      typeswitch($nt/*)
        case element(DocumentTest) return
          function($n as document-node()) as xs:boolean {
            empty($prev) or $prev($n/..)
          }
        case element(AnyKindTest) return
          function($n as node()) as xs:boolean {
            empty($prev) or $prev($n/..)
          }
        case element(TextTest) return
          function($n as text()) as xs:boolean {
            empty($prev) or $prev($n/..)
          }
        case element(CommentTest) return
          function($n as comment()) as xs:boolean {
            empty($prev) or $prev($n/..)
          }
        case $pt as element(PITest) return
          pat:compile-pitest($prev, $pt, $resolver)
        case $at as element(AttributeTest) return
          pat:attribute-type(pat:compile-nametest($prev,$at/(QName|Star),$resolver))
        case $et as element(ElementTest) return
          pat:element-type(pat:compile-nametest($prev,$et/(QName|Star),$resolver))
        default return error(xs:QName("tfm:BADKINDTEST"),
          "Invalid kind test: " || $nt/*)
    case element(NameTest) return
      $default-type(pat:compile-nametest($prev,$nt/*,$resolver))
    default return error(xs:QName("tfm:BADNODETEST"),
      "Invalid node test: " || $nt)
};

declare %private function pat:compile-pitest($prev, $pt, $resolver)
{
  typeswitch($pt/(NCName|StringLiteral))
    case empty-sequence() return
      function($n as processing-instruction()) as xs:boolean {
        empty($prev) or $prev($n/..)
      }
    case $name as element(NCName) return
      function($n as processing-instruction()) as xs:boolean {
        local-name($n) eq $name and
          (empty($prev) or $prev($n/..))
      }
    case $name as element(StringLiteral) return
      let $name := pat:unescape-string($name)
      return function($n as processing-instruction()) as xs:boolean {
        local-name($n) eq $name and
          (empty($prev) or $prev($n/..))
      }
    default return error(xs:QName("tfm:BADNODETEST"),
      "Invalid node test: " || $pt)
};

declare %private function pat:unescape-string($val as xs:string)
  as xs:string
{
  let $replace := substring($val,1,1)
  let $regex := if($replace eq "'") then "('')|([^']+)" else '("")|([^"]+)'
  let $val := substring($val,2,string-length($val)-2)  
  return string-join(
    for $match in analyze-string($val, $regex)/*
    return 
      if($match/*:group/@nr = 1) then $replace
      else string($match)
  ,"")
};

declare %private function pat:compile-nametest($prev, $qn, $resolver)
{
  typeswitch($qn)
    case element(NCNameColonStar) return
      let $prefix := substring-before($qn,"*")
      let $ns := namespace-uri-from-QName($resolver($prefix || "fake"))
      return function($n) {
        namespace-uri($n) eq $ns and
          (empty($prev) or $prev($n/..))
      }
    case element(StarColonNCName) return
      let $localname := substring-after($qn,"*:")
      return function($n) {
        local-name($n) eq $localname and
          (empty($prev) or $prev($n/..))
      }
    case element(Star) | empty-sequence() return
      function($n) {
        empty($prev) or $prev($n/..)
      }
    case element(QName) return
      let $name := $resolver($qn)
      return function($n) {
        node-name($n) eq $name and
          (empty($prev) or $prev($n/..))
      }
    default return error(xs:QName("tfm:BADNAMETEST"),
      "Invalid name test: " || $qn)
};
