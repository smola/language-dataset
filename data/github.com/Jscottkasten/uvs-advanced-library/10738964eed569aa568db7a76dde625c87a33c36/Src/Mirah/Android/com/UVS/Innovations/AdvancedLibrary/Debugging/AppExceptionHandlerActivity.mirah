###################################################################################
# Copyright (C) 2015, 2016, 2017 by UVS Innovations Corporation.                  #
# All rights reserved.                                                            #
#                                                                                 #
# Redistribution and use in source and binary forms, with or without              #
# modification, are permitted provided that the following conditions are met:     #
#                                                                                 #
# 1. Redistributions of source code must retain the above copyright notice, this  #
#    list of conditions and the following disclaimer.                             #
# 2. Redistributions in binary form must reproduce the above copyright notice,    #
#    this list of conditions and the following disclaimer in the documentation    #
#    and/or other materials provided with the distribution.                       #
#                                                                                 #
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND #
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED   #
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE          #
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR #
# ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES  #
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;    #
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND     #
# ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT      #
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS   #
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                    #
#                                                                                 #
###################################################################################

package com.UVS.Innovations.AdvancedLibrary.Debugging

#
# This class extends FragmentActivity to use
# the v4 support framework so the app may
# run on platforms below 3.0.  We specifically
# target a low end at 2.1.
#
import android.support.v4.app.FragmentActivity

#
# Android stuff.
#
import android.content.Context
import android.content.Intent
import android.net.Uri
import android.os.Bundle
import android.os.Environment
import android.view.View
import android.view.View.OnClickListener
import android.view.ViewGroup
import android.view.ViewGroup.LayoutParams as VGLP
import android.widget.Button
import android.widget.LinearLayout
import android.widget.LinearLayout.LayoutParams as LLLP
import android.widget.ScrollView
import android.widget.TextView

#
# Std library support.
# 
import java.io.File

import java.lang.Class
import java.lang.Object
import java.lang.String
import java.util.ArrayList
import java.util.List
import java.util.HashMap
import java.util.Map




#####
#
# Class for displaying a crash notification window and allowing
# the user to email a crash report.
#
# NOTE: Unlike other modules, we don't log our API calls here as
# it adds to the dump we are trying to report.
#
class AppExceptionHandlerActivity < android.support.v4.app.FragmentActivity

  implements OnClickListener

  DEFAULT_EMAIL   = "unknown@unknown.com"
  DEFAULT_SUBJECT = "URGENT: User reported crash dump."
  DEFAULT_TEXT    = "\n\nPlease add comments about the crash here:\n\n"

  def self.EXCEPTION_KEY:String; return "EXCEPTION_KEY"; end
  def self.EMAIL_ADDR_KEY:String; return "EMAIL_ADDRESS_KEY"; end
  def self.EMAIL_SUBJ_KEY:String; return "EMAIL_SUBJECT_KEY"; end
  def self.EMAIL_TEXT_KEY:String; return "EMAIL_TEXT_KEY"; end

  #protected
  def onCreate (state:Bundle):void
    super state

    # Recover node info.
    intent      = self.getIntent
    msg         = intent.getStringArrayListExtra EXCEPTION_KEY
    @email_addr = intent.getStringExtra          EMAIL_ADDR_KEY
    @email_subj = intent.getStringExtra          EMAIL_SUBJ_KEY
    @email_text = intent.getStringExtra          EMAIL_TEXT_KEY

    @email_addr = DEFAULT_EMAIL   unless @email_addr
    @email_subj = DEFAULT_SUBJECT unless @email_subj
    @email_text = DEFAULT_TEXT    unless @email_text

    # Generate the view.
    view = LinearLayout.new self
    view.setOrientation LinearLayout.VERTICAL
    view.setLayoutParams(
      LLLP.new(VGLP.FILL_PARENT, VGLP.FILL_PARENT, 0)
    )

    sview = ScrollView.new self
    sview.setLayoutParams(
      LLLP.new(VGLP.FILL_PARENT, 0, 1)
    )
    view.addView sview

    content = LinearLayout.new self
    content.setOrientation LinearLayout.VERTICAL
    content.setLayoutParams(
      LLLP.new(VGLP.FILL_PARENT, VGLP.FILL_PARENT, 0)
    )
    sview.addView content

    @button = Button.new self
    @button.setText "Send Crash Report"
    @button.setLayoutParams(
      LLLP.new(VGLP.FILL_PARENT, VGLP.WRAP_CONTENT, 0)
    )
    view.addView @button
    @button.setOnClickListener self

    if msg
      @stack_dump = StringBuilder.new 4096

      msg.each do |line:String|
        text = TextView.new self
        text.setText line
        text.setLayoutParams(
          LLLP.new(VGLP.FILL_PARENT, VGLP.WRAP_CONTENT, 0)
        )
        content.addView text

        @stack_dump.append line + "\n"
      end
    else
      text = TextView.new self
      text.setText "Had a crash, but no crash report was saved!"
      text.setLayoutParams(
        LLLP.new(VGLP.FILL_PARENT, VGLP.WRAP_CONTENT, 0)
      )
      content.addView(text)
    end

    self.setContentView(view)
  end

  #
  # From the View::OnClickListener interface.
  #
  def onClick (view:View):void
    if (view == @button) and @stack_dump
#      # Find the crash dump file.
#      settings     = AppSettings.get_reference self
#      dloads       = Environment.getExternalStoragePublicDirectory Environment.DIRECTORY_DOWNLOADS
#      export_file  = File.new dloads, settings.debug_logs_file
    
      # Send the email intent.
      mail_to      = String[1]
      mail_to[0]   = @email_addr

      share_intent = Intent.new Intent.ACTION_SEND
      share_intent.setData Uri.parse("mailto")
      share_intent.putExtra Intent.EXTRA_EMAIL, mail_to
      share_intent.putExtra Intent.EXTRA_SUBJECT, @email_subj
      share_intent.putExtra Intent.EXTRA_TEXT, (@stack_dump.toString + "\n\n" + @email_text)
      share_intent.setType "message/rfc822"
#      share_intent.putExtra Intent.EXTRA_STREAM, Uri.fromFile(export_file)
#      share_intent.addFlags Intent.FLAG_GRANT_READ_URI_PERMISSION
#      share_intent.addFlags Intent.FLAG_ACTIVITY_FORWARD_RESULT
      share_intent.addFlags Intent.FLAG_ACTIVITY_PREVIOUS_IS_TOP
      chooser = Intent.createChooser share_intent, "Email Crash Report"

      self.startActivity chooser
    end
  end

end # class
