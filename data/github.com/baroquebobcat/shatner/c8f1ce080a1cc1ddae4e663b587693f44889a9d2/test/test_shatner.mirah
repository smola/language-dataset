package org.shatner

import org.shatner.ShatnerBase
import javax.servlet.http.*
import java.util.HashMap

import java.util.concurrent.Callable
import java.io.PrintWriter
import java.io.ByteArrayOutputStream

import org.junit.After
import org.junit.Before
import org.junit.Test
import org.junit.Assert


class SomeApp < ShatnerBase
  def initialize : void
    super
    2
  end

  get '/' do
    'bananas'
  end
end

class SomeAppWoInitializer < ShatnerBase
  get '/' do
    'bananas'
  end
end

#class SomeAppWithView < ShatnerBase
#  get '/' do
#    edb :index
#  end
#end

class SomeAppWithPost < ShatnerBase
  post '/' do
    'bananas'
  end
end
  
class FakeServletRequest; implements HttpServletRequest
  def initialize
  end
  def getPathInfo : String
    "/"
  end
end


class FakeServletResponse; implements HttpServletResponse
def initialize
  @output_stream = ByteArrayOutputStream.new
end
def getWriter
  PrintWriter.new output_stream
end
def output_stream
@output_stream
end
end

class SinatraCloneTest
/*  macro def test(name, &block)
    test_name = "test_" + name.string_value.replaceAll(" ", "_")
m = quote { def `"bar"`;puts "aoeu";end}
#puts MethodDefinition(m).child_nodes
puts m
import org.junit.Test
    node = quote do
      import org.junit.Test
      $Test
      def test_name #`test_name`:void
       # `block.body`
"aoeu"
      end
    end
puts node
puts node.child_nodes
puts "==============================="
#quote {nil}
node
  end
#
  test "doGet prints hello world" do
*/

  $After
  def after
    @app = ShatnerBase(nil)
    @req = FakeServletRequest.new
    @resp = FakeServletResponse.new
  end
  
  $Test
  def test_doGet_prints_bananas : void
    set_app SomeApp.new

    get '/'
    
    Assert.assertEquals "bananas\n", resp.output_stream.toString
  end

  $Test
  def test_doPost_prints_bananas : void
    set_app SomeAppWithPost.new
    
    post '/'
  
    Assert.assertEquals "bananas\n", resp.output_stream.toString
  end

#  $Test
#  def test_some_app_with_view_prints_bananas : void
#    set_app SomeAppWithView.new
#  end
  
  
  def get url:String
    @req = FakeServletRequest.new
    @resp = FakeServletResponse.new

    @app.doGet(@req, @resp)
  end
    
  def post url:String
    @req = FakeServletRequest.new
    @resp = FakeServletResponse.new

    @app.doPost(@req, @resp)
  end
    
  def resp
    @resp
  end
  
  def set_app app:ShatnerBase
    @app = app
  end
end