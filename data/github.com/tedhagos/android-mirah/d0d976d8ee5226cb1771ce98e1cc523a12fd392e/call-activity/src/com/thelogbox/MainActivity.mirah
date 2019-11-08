package com.thelogbox

import android.app.Activity
import android.os.Bundle
import android.widget.LinearLayout
import android.view.View
import android.view.View.OnClickListener
import android.content.Intent
import android.widget.Button
import android.widget.TextView
import android.content.Context

class MainActivity < Activity
  def onCreate state 
    super state
    setContentView MainView.new self
  end
end

class ChildActivity < Activity 
	def onCreate state
		super state
		setContentView ChildView.new self
	end

	class ChildView < LinearLayout		
		textview = nil

		def initialize context:Context
			super context
			@ctx = context
			@textview = TextView.new context
			@textview.setText "Child View"
			addView @textview
		end
	end	

end

class MainView < LinearLayout 
	implements OnClickListener

	button = nil

	def initialize context:Context
		super context
		@ctx = context
		@button = Button.new context
		@button.setOnClickListener self
		@button.setText "Launch Child Activity"

		addView @button
	end	

	def onClick view:View
		intent = Intent.new @ctx,ChildActivity.class
		@ctx.startActivity intent
	end
end

