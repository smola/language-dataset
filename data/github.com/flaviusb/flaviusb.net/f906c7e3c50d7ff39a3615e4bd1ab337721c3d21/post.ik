base = "http://flaviusb.net/"
style = dsyntax("Add style sheet link in place.",
  [location]
  ''(''link(rel: "stylesheet", type: "text/css", href: `location))
)
''(
`doctype("xml")
`doctype("xhtml")
html(xmlns: "http://www.w3.org/1999/xhtml", lang: "en") (head
  (title "#{`data[:title]}")
  meta(charset: "utf-8")
  `style("#{`base}reset.css")
  `style("#{`base}style.css")
  `style("#{`base}syntax.css")
  `style("#{`base}container.css")
  `style("#{`base}mono.css")
  `style("http://fonts.googleapis.com/css?family=Inconsolata")
  link(href: "#{`base}atom.xml", type: "application/atom+xml", rel: "alternate", title: "Blog Atom Feed")  
  link(rel: "shortcut icon", href: "#{`base}favicon.png", type: "image/png"))
  (body(class: "highlight post")
    (h1 "#{`data[:title]}")
    (div "#{`data[:content]}")
    (p
      a(href: "http://flaviusb.net") "Home"
      "   |   "
      a(rel: "index", href: "http://flaviusb.net/blog") "Blog"
      "   |   "
      a(href: "http://github.com/flaviusb") "Code")))
