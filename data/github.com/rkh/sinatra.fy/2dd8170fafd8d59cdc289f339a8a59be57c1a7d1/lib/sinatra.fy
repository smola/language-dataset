require("sinatra")

class Sinatra Base {
  def self def: name with: block {
    metaclass ruby: 'define_method args: [name] with_block: block
    Sinatra Delegator delegate(name)
  }

  def self configure: env with: block {
    args = [env] flatten()
    configure(env, &block)
  }

  def self configure: block {
    configure(&block)
  }

  def self wrap: method {
    def: "#{method}:"    with: |b   | { ruby: method           with_block: b }
    def: "#{method}:do:" with: |a, b| { ruby: method args: [a] with_block: b }
  }

  def self wrapAll: methods {
    methods each: |m| { wrap: m }
  }

  wrapAll: [
    'get, 'put, 'post, 'delete, 'head, 'patch, 'before, 'after, 'helpers,
    'not_found, 'template, 'layout
  ]

  def self enable: options {
    enable(*options) flatten()
  }

  def self disable: options {
    disable(*options) flatten()
  }

  def self set: option to: value {
    set(option, value)
  }

  Sinatra Delegator delegate('enable:, 'disable:, 'set:to:,
                             'configure:, 'configure:with:)

  forwards_unary_ruby_methods

  alias_method: 'redirect: for_ruby: 'redirect
  alias_method: 'to: for_ruby: 'to
}

class Sinatra Request {
  forwards_unary_ruby_methods
}

enable: 'run
