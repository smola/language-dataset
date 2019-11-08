package org.dellamorte.raum.toolbox.fontMeshCreator

import org.dellamorte.raum.engine.Loader
import java.io.File

class FontType
  def self.init(loader:Loader)
    @@ldr = loader
  end
  
  def initialize(font:String):void
    @textureAtlas = @@ldr.loadTexture(font)
    @loader = TextMeshCreator.new(File.new("res/fonts/" + font + ".fnt"))
  end

  def getTextureAtlas():int
    return @textureAtlas
  end

  def loadText(text:GUIText):TextMeshData
    return @loader.createTextMesh(text)
  end

end
