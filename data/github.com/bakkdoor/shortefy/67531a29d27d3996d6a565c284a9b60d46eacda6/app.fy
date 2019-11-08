require("sha1")
require: "sinatra"
require: "html"
require: "redis"

R = Redis Client new

# CONFIGURATION

configure: 'production with: { disable: 'show_errors }
configure: ['production, 'development] with: {
  enable: 'logging
}

set: 'port to: 3000

# HELPER METHODS

# basic html layout
def with_layout: body {
  HTML new: @{
    html: @{
      head: @{
        title: "Shortefy v0.0.1"
      }
      body: |h| {
        h h1: "Shortefy"
        body call: [h]
      }
    }
  } to_s
}

def with_link: id do: block else: else_block ({ "" }) {
  if: (R get: $ key: id) then: block else: else_block
}

def key: id {
  "shortefy:#{id}"
}

def count_key: id {
  key: id + ":count"
}

def incr_counter: id {
  R incr: $ count_key: id
}

def counter: id {
  R get: (count_key: id) . to_i
}

# PAGE ROUTES

get: "/" do: {
  with_layout: @{
    form: { action: "/new" method: "post" } with: @{
      fieldset: @{
        label: { for: "link" } with: "Link"
        br
        input: { type: "text" id: "link" name: "link" value: "http://" }
        br
        input: { type: "submit" value: "SAVE" }
      }
    }
  }
}

post: "/new" do: {
  link = params['link]
  id = SHA1 new(link) to_s [[0, 10]]
  R set: (key: id, link)
  redirect: "/show/#{id}"
}

get: "/show/:id" do: |id| {
  with_link: id do: |link| {
    with_layout: @{
      h1: "ID: #{id}"
      h2: "Clicks: #{counter: id}"
      h2: @{
        a: { href: link } with: link
      }
    }
  } else: {
    redirect: "/"
  }
}

get: "/:id" do: |id| {
  with_link: id do: |link| {
    incr_counter: id
    redirect: link
  }
}

not_found: {
  with_layout: @{
    h1: "Sorry, this page does not exist :("
  }
}
