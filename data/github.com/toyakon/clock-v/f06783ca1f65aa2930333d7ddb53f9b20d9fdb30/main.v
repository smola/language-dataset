import gx
import gl
import gg
import glfw
import time
import freetype

const (
    win_w = 300
    win_h = 100
)
struct context {
    gg &gg.GG
mut:
    ft &freetype.Context
}

const (
    text_cfg = gx.TextCfg{
        size:18
        color:gx.rgb(0, 0, 0)
    }
)

fn main() {
    glfw.init()
    mut ctx := &context {
        gg: gg.new_context(gg.Cfg {
            width: win_w
            height: win_h
            use_ortho: true
            create_window: true
            window_title: 'Clock'
            window_user_ptr: ctx
        })
        ft: freetype.new_context (gg.Cfg{
            width: win_w
            height: win_h
            use_ortho: true
            font_size: 18
            scale: 2
            font_path:'/usr/share/fonts/truetype/noto/NotoMono-Regular.ttf'
        })
    }
    go update()
    for {
        gg.clear(gx.White)
        ctx.ft.draw_text(0, 50, time.now().format_ss(), text_cfg)
        ctx.gg.render()
        if ctx.gg.window.should_close() {
            ctx.gg.window.destroy()
            return
        }
    }
}

fn update() {
    for {
        gg.post_empty_event()
        time.sleep(1)
    }
}
