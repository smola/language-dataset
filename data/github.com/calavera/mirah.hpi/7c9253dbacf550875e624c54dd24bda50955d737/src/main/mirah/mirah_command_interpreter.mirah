package hudson.plugins.mirah

import hudson.*
import hudson.model.*
import hudson.tasks.*

import net.sf.json.JSONObject
import org.kohsuke.stapler.*
import org.mirah.hudson.descriptors.*

class MirahInterpreter < CommandInterpreter
  $Extension
  def self.getExtension
    @@extension ||= DescriptorImpl.new(MirahInterpreter.class)
  end

  $DataBoundConstructor
  def initialize(command:String, verbose:boolean)
    super(command)
    @verbose = verbose
  end

  def buildCommandLine(script:FilePath)
    verbose_arg = @verbose ? '--verbose' : ''
    list = ['mirah', verbose_arg, script.getRemote]

    strings = String[list.size]
    list.size.times { |i| strings[i] = String(list.get(i)) }
    strings
  end

  def isVerbose
    @verbose
  end

  def getFileExtension
    ".mirah"
  end

  def getDescriptor
    @@extension
  end

  def getContents
    getCommand
  end

  class DescriptorImpl < BuilderStepDescriptor
    def initialize(clazz:Class)
      super(clazz)
    end

    def newInstance(req:StaplerRequest, data:JSONObject):Builder
      MirahInterpreter.new(data.getString('command'), data.getBoolean('verbose'))
    end

    def getDisplayName
      "Execute Mirah script"
    end
  end
end
