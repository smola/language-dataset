base = "http://flaviusb.net/"
style = dsyntax("Add style sheet link in place.",
  [location]
  ''(''link(rel: "stylesheet", type: "text/css", href: `location))
)
''(
`doctype("xml")
`doctype("xhtml")
html(xmlns: "http://www.w3.org/1999/xhtml", lang: "en") (head
  (title "Index of all tweets")
  meta(charset: "utf-8")
  `style("#{`base}reset.css")
  `style("#{`base}style.css")
  `style("#{`base}syntax.css")
  `style("#{`base}mono.css")
  `style("#{`base}container.css")
  link(rel: "shortcut icon", href: "#{`base}favicon.png", type: "image/png")
  link(href: "http://flaviusb.net/tweets/atom0.atom", type: "application/atom+xml", rel: "alternate", title: "Tweet Atom Feed"))
  (body 
    (ul(class: "posts") "#{`data[:rendered_posts]}")
    (p
      a(href: "http://flaviusb.net") "Home"
      " &#160; | &#160; "
      a(rel: "index", href: "http://flaviusb.net/tweets/") "Tweets"
      " &#160; | &#160; "
      a(href: "http://github.com/flaviusb") "Code")))
