use("ispec")
use("byel_helper.ik")

use("xml.ik")
describe(XML,
  it("should be able to render a basic document",
    (XML render('(a b c))) should == "<a><b><c /></b></a>"
  )
  it("should be able to render attributes",
    (XML render('(a b(a: :b, b: c) c))) should == #[<a><b a=":b" b="c"><c /></b></a>]
  )
  it("should be able to render text blocks",
    (XML render('(urlset url "http://example.com"))) should == "<urlset><url>http://example.com</url></urlset>"
    theMessage = Message fromText(#[''(`doctype("xml") foo(xmlns: "http://example.com/foobar") "There should be text here." bar "And also here")]) evaluateOn(XML, XML)
    (XML render(theMessage)) should == #[<?xml version="1.0" encoding="utf-8"?>\n<foo xmlns="http://example.com/foobar">There should be text here.<bar>And also here</bar></foo>]
  )
  describe("should autoclose elements in the right places", {pending: true})
  it("should correctly handle the ^ rewriting step",
    (XML render('(^(asdFr(qw: "er")) fred))) should == #[<asd-fr qw="er"><fred /></asd-fr>]
    (XML render('(weew ^(asdFr(reab: "foofoo", qw: "er")) ^fred))) should == #[<weew><asd-fr reab="foofoo" qw="er"><fred /></asd-fr></weew>]
    (XML render('(weew ^(asdFr({"r-eab" => "foofoo"}, qw: "er")) ^fRed))) should == #[<weew><asd-fr r-eab="foofoo" qw="er"><f-red /></asd-fr></weew>]
  )
  it("should correctly handle naive xml comments",
    (XML render(
      '(electro
         //bass sound(gravel: "true") "wubwubwub"
         bass sound(gravel: "false")  "oontzoontzoontz")
    )) should == #[<electro><!-- <bass><sound gravel="true">wubwubwub</sound></bass> --><bass><sound gravel="false">oontzoontzoontz</sound></bass></electro>]
  )
)
