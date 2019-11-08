package com.example.sudoku

import android.app.Activity
import android.app.AlertDialog
import android.content.DialogInterface
import android.content.DialogInterface.OnClickListener as DI_OCL
import android.content.Intent
import android.util.Log
import android.view.View.OnClickListener


class Sudoku < Activity
  implements OnClickListener
  def onCreate(state)
    super state
    setContentView R.layout.main

    findViewById(R.id.continue_button).setOnClickListener self
    findViewById(R.id.new_game_button).setOnClickListener self
    findViewById(R.id.about_button).setOnClickListener self
    findViewById(R.id.exit_button).setOnClickListener self
  end

  def onResume
    super 
    Music.play(self, R.raw.main)
  end

  def onPause
    super 
    Music.stop(self)
  end

  def onCreateOptionsMenu(menu)
    super menu
    inflater = getMenuInflater
    inflater.inflate(R.menu.menu, menu) 
    true
  end

  def onOptionsItemSelected(item)
    if (item.getItemId == R.id.settings)
      startActivity(Intent.new(self, Prefs.class))
      return true
    end
    false
  end

  def onClick(v)
    view_id = v.getId
    if (view_id == R.id.about_button) 
      startActivity(Intent.new(self, About.class))
    elsif (view_id == R.id.continue_button)
      startGame(getResources.getInteger(R.integer.continue_difficutly))
    elsif (view_id == R.id.new_game_button) 
      openNewGameDialog
    elsif (view_id == R.id.exit_button) 
      finish
    end
  end

  def openNewGameDialog
    this=self
    builder = AlertDialog.Builder.new(self) 
    builder.setTitle(R.string.new_game_title)
    builder.setItems(R.array.difficulty) { |dialog, i| 
      this.startGame(i)
    }
    builder.show
  end

  def startGame(diff:int):void
      Log.d("SUDOKU", "clicked on #{diff}")
      intent = Intent.new(self, Game.class)
      intent.putExtra(getResources.getString(R.string.difficulty_key), diff)
      startActivity(intent)
  end
end
