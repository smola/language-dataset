package com.example.sudoku

import android.content.Context
import android.preference.PreferenceActivity
import android.preference.PreferenceManager

class Prefs < PreferenceActivity
  def onCreate(state)
    super state
    addPreferencesFromResource(R.xml.settings)
  end

  def self.getMusic(context:Context)
    PreferenceManager.getDefaultSharedPreferences(context).getBoolean("music", true)
  end

  def self.getHints(context:Context)
    PreferenceManager.getDefaultSharedPreferences(context).getBoolean("hints", true)
  end
end
