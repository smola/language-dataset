package org.zerosum.upordown

import android.app.Activity
import android.app.Dialog
import android.app.AlertDialog
import android.content.Context
import android.util.Log
import android.view.View
import android.widget.EditText
import android.widget.Button

import java.net.URL
import java.net.SocketTimeoutException

import org.jsoup.Jsoup
import org.jsoup.nodes.Document

class StatusActivity < Activity
  def onCreate(state)
    super state
    setContentView R.layout.main

    Log.d 'StatusActivity', 'init'
    @url = EditText findViewById(R.id.url_txt)
    @submit = Button findViewById(R.id.submit_btn)

    setListeners
  end

  def getUrl
    @url.getText.toString
  end

  def setListeners
    this = self
    @submit.setOnClickListener do |v|
      Log.d 'StatusActivity', 'click'
      status = this.checkSiteStatus(this.getUrl)
      this.showResult status
    end
  end

  def showResult(message:String)
    alert = AlertDialog.Builder.new(self)
    alert.setTitle 'Site Test Results'
    alert.setMessage message
    alert.setPositiveButton('OK') do |dialog, which|
      dialog.dismiss
    end

    alert.show
  end

  def checkSiteStatus(address:String):String
    return "Please specify a URL to test" if address.equals('')

    begin
      doc = Jsoup.connect("http://downforeveryoneorjustme.com/" + address).get
      res = doc.select("#container").first.text

      Log.d 'StatusActivity', 'Full response from server is: ' + res
      res.substring(0, res.indexOf('Check another'))
    rescue SocketTimeoutException => ex
      "Unable to contact the server. How ironic!"
    end
  end
end
