package com.example.sudoku

import android.content.Context
import android.graphics.Paint
import android.graphics.Paint.Style
import android.graphics.Rect
import android.os.Bundle
import android.os.Parcelable
import android.view.KeyEvent
import android.view.MotionEvent
import android.view.View
import android.view.animation.AnimationUtils
import android.util.Log

class PuzzleView < View
  def self.id; 42; end
  def initialize(context:Context)
    super context
    @selX = 0 
    @selY = 0 
    @game = Game(context)
    @selRect = Rect.new
    setFocusable true
    setFocusableInTouchMode true
    setId(PuzzleView.id)
    Log.d("PuzzleView", "constructed")
  end

  def onSaveInstanceState
    p = super
    bundle = Bundle.new
    bundle.putInt("selX", @selX)
    bundle.putInt("selY", @selY)
    bundle.putParcelable("viewState", p)
    bundle
  end

  def onRestoreInstanceState(state:Parcelable)
    bundle = Bundle(state)
    select(bundle.getInt("selX"), bundle.getInt("selY"))
    super bundle.getParcelable("viewState")
  end

  def onKeyDown(key, event)
    return select(@selX, @selY -1) if (key == KeyEvent.KEYCODE_DPAD_UP)
    return select(@selX, @selY +1) if (key == KeyEvent.KEYCODE_DPAD_DOWN)
    return select(@selX-1, @selY) if (key == KeyEvent.KEYCODE_DPAD_LEFT)
    return select(@selX+1, @selY) if (key == KeyEvent.KEYCODE_DPAD_RIGHT)
    return setSelectedTile(0) if (key == KeyEvent.KEYCODE_0 or key == KeyEvent.KEYCODE_SPACE)
    return setSelectedTile(1) if (key == KeyEvent.KEYCODE_1)
    return setSelectedTile(2) if (key == KeyEvent.KEYCODE_2)
    return setSelectedTile(3) if (key == KeyEvent.KEYCODE_3)
    return setSelectedTile(4) if (key == KeyEvent.KEYCODE_4)
    return setSelectedTile(5) if (key == KeyEvent.KEYCODE_5)
    return setSelectedTile(6) if (key == KeyEvent.KEYCODE_6)
    return setSelectedTile(7) if (key == KeyEvent.KEYCODE_7)
    return setSelectedTile(8) if (key == KeyEvent.KEYCODE_8)
    return setSelectedTile(9) if (key == KeyEvent.KEYCODE_9)
    return super key, event
  end

  def onTouchEvent(event)
    if (event.getAction != MotionEvent.ACTION_DOWN)
      return super event
    end
    select(int(event.getX / @width), int(event.getY / @height))
    @game.showKeypadOrError(@selX, @selY);
    Log.d("PuzzleView", "onTouchEvent: #{@selX}, #{@selY}")
    true
  end

  def select(x:int, y:int)
    invalidate(@selRect)
    @selX = Math.min(Math.max(x, 0),8)
    @selY = Math.min(Math.max(y, 0),8)
    getRect(@selX, @selY, @selRect)
    invalidate(@selRect)
    true
  end

  def setSelectedTile(tile:int)
    if (@game.setTileIfValid(@selX, @selY, tile))
      invalidate
    else
      Log.d("PuzzleView", "setSelectedTile: invalid #{tile}")
      startAnimation(AnimationUtils.loadAnimation(@game, R.anim.shake))
    end
    true
  end

  def onSizeChanged(w, h, oldw, oldh)
    Log.d("PuzzleView", "size changed: #{w}x#{h}")
    @width = int(w/9.0)
    @height = int(h/9.0)
    getRect(@selX, @selY, @selRect)
    super w, h, oldw, oldh
  end

  def getRect(x:int, y:int, rect:Rect)
    rect.set(int(x*@width), int(y*@height), int(x*@width + @width), int(y*@height + @height))
  end

  def onDraw(canvas)
    Log.d("PuzzleView", "drawing bg")
    # draw background
    background = loadColor R.color.puzzle_background
    canvas.drawRect(0,0, getWidth, getHeight, background)

    Log.d("PuzzleView", "drawing lines")
    #draw board
    #line colors
    dark = loadColor R.color.puzzle_dark 
    hilite = loadColor R.color.puzzle_hilite 
    light = loadColor R.color.puzzle_light 

    #draw lines
    i = 0
    while (i < 9)
      #minor lines
      canvas.drawLine(0, i*@height, getWidth, i*@height, light)
      canvas.drawLine(0, i*@height+1, getWidth, i*@height+1, hilite)
      canvas.drawLine(i*@width, 0, i*@width, getHeight, light)
      canvas.drawLine(i*@width+1, 0, i*@width+1, getHeight, hilite)
      #major lines
      if (i % 3 == 0)
        canvas.drawLine(0, i*@height, getWidth, i*@height, dark)
        canvas.drawLine(0, i*@height+1, getWidth, i*@height+1, hilite)
        canvas.drawLine(i*@width, 0, i*@width, getHeight, dark)
        canvas.drawLine(i*@width+1, 0, i*@width+1, getHeight, hilite)
      end
      i+=1
    end

    #draw numbers
    Log.d("PuzzleView", "drawing numbers")
    foreground = loadColor(R.color.puzzle_foreground)
    foreground.setStyle(Style.FILL)
    foreground.setTextSize(float(@height * 0.75))
    foreground.setTextScaleX(@width/@height)
    foreground.setTextAlign(Paint.Align.CENTER)

    fm = foreground.getFontMetrics
    x = @width / 2
    y = @height / 2 - (fm.ascent + fm.descent) / 2
    i = 0
    while (i < 9)
      j = 0 
      while (j < 9)
        canvas.drawText(@game.getTileString(i, j), i * @width + x, j*@height + y, foreground)
        j+=1
      end
      i+=1
    end

    #draw hints
    if (Prefs.getHints(getContext))
      colors = [ loadColor(R.color.puzzle_hint_0),
                 loadColor(R.color.puzzle_hint_1),
                 loadColor(R.color.puzzle_hint_2) ]
      r = Rect.new
      i = 0
      while (i<9)
        j = 0
        while (j<9)
          used =@game.getUsedTiles(i,j)
          moves_left = 9 - used.size
          if (moves_left < colors.size)
            getRect(i, j, r)
            canvas.drawRect(r,Paint(colors.get(moves_left)))
          end
          j+=1
        end
        i+=1 
      end
    end
    
    #draw selection
    selected = loadColor(R.color.puzzle_selected)
    canvas.drawRect(@selRect, selected)
  end

  def loadColor(key:int)
    paint = Paint.new
    paint.setColor(getResources.getColor(key))
    paint
  end
end

