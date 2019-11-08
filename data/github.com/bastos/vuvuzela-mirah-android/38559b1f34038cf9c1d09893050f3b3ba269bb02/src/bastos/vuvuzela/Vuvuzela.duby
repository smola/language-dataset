import "android.app.Activity"
import "android.media.MediaPlayer"
import "android.os.Bundle"
import "android.view.View"
import "android.widget.Button"
import "android.widget.ImageView"
import "bastos.vuvuzela.R"
import "OnClickListener", "android.view.View$OnClickListener"
import "android.util.Log"
import "android.content.Context"

class Vuvuzela < Activity
  
  def onCreate(savedInstanceState:Bundle)
    super(savedInstanceState)
    setContentView(R.layout.main);
    @context = getBaseContext()
    vuvuzela = findViewById(R.id.ImageView01)
    listener = VuvuzelaClickListener.new(@context)
    vuvuzela.setOnClickListener(listener)
  end
  
  class VuvuzelaClickListener
    implements OnClickListener
    def initialize(context:Context)
      @context = context
    end
    def onClick(view:View)
      Log.i("test", "Just a test")
      mp = MediaPlayer.create(Context(@context), R.raw.vuvuzela);
      mp.start();
    end
  end

end


