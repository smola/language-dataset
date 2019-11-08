class Template {
  @@templates = <[]>
  @@caching = true
  @@path_prefix = nil

  def initialize: @filename {
    { @filename = "#{@@path_prefix}/#{@filename}" } if: @@path_prefix
  }

  def read_contents {
    @contents = File read: @filename
  }

  def render: locals (<[]>) {
    { read_contents } unless: @contents
    rendered_contents = @contents
    locals each: |name val| {
      val = val || { "" }
      pattern = "#" + "{" + name + "}"
      match rendered_contents {
        case Regexp new(pattern) ->
          rendered_contents = rendered_contents replace: pattern with: val
      }
    }
    rendered_contents
  }

  def Template path_prefix: prefix {
    @@path_prefix = prefix
  }

  def Template caching: caching {
    @@caching = caching
  }

  def Template [filename] {
    if: @@caching then: {
      if: (@@templates[filename]) then: |t| {
        t
      } else: {
        t = Template new: filename
        t read_contents
        @@templates at: filename put: t
        t
      }
    } else: {
      Template new: filename
    }
  }
}