// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// Ported to SDL2 V wrapper by Nicolas Sauzede 2019

import rand
import time
import math
import vsdl

const (
	Title = 'V Tetris'
	FontName = 'RobotoMono-Regular.ttf'
	BlockSize = 20 // pixels
	FieldHeight = 20 // # of blocks
	FieldWidth = 10
	TetroSize = 4
	WinWidth = BlockSize * FieldWidth
	WinHeight = BlockSize * FieldHeight
	TimerPeriod = 250 // ms
	TextSize = 16
)

const (
	// Tetros' 4 possible states are encoded in binaries
	BTetros = [
		// 0000 0
		// 0000 0
		// 0110 6
		// 0110 6
		[66, 66, 66, 66],
		// 0000 0
		// 0000 0
		// 0010 2
		// 0111 7
		[27, 131, 72, 232],
		// 0000 0
		// 0000 0
		// 0011 3
		// 0110 6
		[36, 231, 36, 231],
		// 0000 0
		// 0000 0
		// 0110 6
		// 0011 3
		[63, 132, 63, 132],
		// 0000 0
		// 0011 3
		// 0001 1
		// 0001 1
		[311, 17, 223, 74],
		// 0000 0
		// 0011 3
		// 0010 2
		// 0010 2
		[322, 71, 113, 47],
		// Special case since 15 can't be used
		// 1111
		[1111, 9, 1111, 9],
	]
	// Each tetro has its unique color
	Colors = [
		SdlColor{byte(0), byte(0), byte(0), byte(0)},        // unused ?
		SdlColor{byte(253), byte(32), byte(47), byte(0)},    // lightred quad
		SdlColor{byte(0), byte(110), byte(194), byte(0)},    // lightblue triple
		SdlColor{byte(170), byte(170), byte(0), byte(0)},    // darkyellow short topright
		SdlColor{byte(170), byte(0), byte(170), byte(0)},    // purple short topleft
		SdlColor{byte(50), byte(90), byte(110), byte(0)},    // darkgrey long topleft
		SdlColor{byte(0), byte(170), byte(0), byte(0)},      // lightgreen long topright
		SdlColor{byte(170), byte(85), byte(0), byte(0)},     // brown longest
		SdlColor{byte(0), byte(170), byte(170), byte(0)},    // unused ?
	]
)

// TODO: type Tetro [TetroSize]struct{ x, y int }
struct Block {
	mut:
	x int
	y int
}

enum GameState {
        paused running gameover
}

struct AudioSample {
        wav_buffer &byte
        wav_length u32
}

struct AudioContext {
mut:
//        audio_pos *byte
        audio_pos voidptr
        audio_len u32
        wav_spec SdlAudioSpec
        samples [3]AudioSample
}

struct SdlContext {
mut:
//      VIDEO
	w int
	h int
	window          voidptr
	renderer        voidptr
	screen          &SdlSurface
	texture         voidptr
//      AUDIO
        actx AudioContext
}

struct Game {
mut:
	// Score of the current game
	score        int
	// Count consecutive lines for scoring
	lines        int
	// State of the current game
	state    GameState
	// Quit game ?
	should_close     bool
	// Position of the current tetro
	pos_x        int
	pos_y        int
	// field[y][x] contains the color of the block with (x,y) coordinates
	// "-1" border is to avoid bounds checking.
	// -1 -1 -1 -1
	// -1  0  0 -1
	// -1  0  0 -1
	// -1 -1 -1 -1
	field       [][]int
	// TODO: tetro Tetro
	tetro       []Block
	// TODO: tetros_cache []Tetro
	tetros_cache []Block
	// Index of the current tetro. Refers to its color.
	tetro_idx    int
	// Index of the next tetro. Refers to its color.
	tetro_next    int
	// Index of the rotation (0-3)
	rotation_idx int
	// SDL2 context for drawing
	sdl             SdlContext
	// TTF context for font drawing
	font            voidptr
}

fn acb(userdata voidptr, stream &byte, _len int) {
        mut ctx := &AudioContext(userdata)
        C.memset(stream, 0, _len)
        if ctx.audio_len == u32(0) {
                return
        }
        mut len := u32(_len)
        if len > ctx.audio_len { len = ctx.audio_len }
        C.memcpy(stream, ctx.audio_pos, len)
        ctx.audio_pos += len
        ctx.audio_len -= len
}

fn (sdl mut SdlContext) set_sdl_context(w int, h int, title string) {
	C.SDL_Init(C.SDL_INIT_VIDEO | C.SDL_INIT_AUDIO)
	C.atexit(C.SDL_Quit)
	C.TTF_Init()
	C.atexit(C.TTF_Quit)
	bpp := 32
	C.SDL_CreateWindowAndRenderer(w, h, 0, &sdl.window, &sdl.renderer)
	C.SDL_SetWindowTitle(sdl.window, title.str)
	sdl.w = w
	sdl.h = h
	sdl.screen = &SdlSurface(C.SDL_CreateRGBSurface(0, w, h, bpp, 0x00FF0000, 0x0000FF00, 0x000000FF, 0xFF000000))
	sdl.texture = C.SDL_CreateTexture(sdl.renderer, C.SDL_PIXELFORMAT_ARGB8888, C.SDL_TEXTUREACCESS_STREAMING, w, h)
	
        C.SDL_LoadWAV('sounds/block.wav', &sdl.actx.wav_spec, &sdl.actx.samples[0].wav_buffer, &sdl.actx.samples[0].wav_length)
        C.SDL_LoadWAV('sounds/line.wav', &sdl.actx.wav_spec, &sdl.actx.samples[1].wav_buffer, &sdl.actx.samples[1].wav_length)
        C.SDL_LoadWAV('sounds/double.wav', &sdl.actx.wav_spec, &sdl.actx.samples[2].wav_buffer, &sdl.actx.samples[2].wav_length)
        sdl.actx.wav_spec.callback = acb
        sdl.actx.wav_spec.userdata = &sdl.actx
        sdl.actx.audio_len = u32(0)
        sdl.actx.audio_pos = voidptr(0)
        if C.SDL_OpenAudio(&sdl.actx.wav_spec, 0) < 0 {
                println('couldn\'t open audio')
        } else {
                C.SDL_PauseAudio(0)
        }
}

fn main() {
	mut game := &Game{}
	game.sdl.set_sdl_context(WinWidth, WinHeight, Title)
	game.font = C.TTF_OpenFont(FontName.str, TextSize)
	game.init_game()
	go game.run() // Run the game loop in a new thread
	for {
		game.draw_scene()
//		game.handle_events()            // CRASHES if done in function ???
		ev := SdlEvent{}
		for 0 < C.SDL_PollEvent(&ev) {
			switch ev._type {
				case C.SDL_QUIT:
					game.should_close = true
					break
				case C.SDL_KEYDOWN:
					// global keys
					switch int(ev.key.keysym.sym) {
						case C.SDLK_ESCAPE:
							game.should_close = true
							break
						case C.SDLK_SPACE:
							switch game.state {
								case .running:
									game.state = .paused
								case .paused:
									game.state = .running
								case .gameover:
									game.init_game()
									game.state = .running
							}
					}
					if game.state != .running {
						break
					}
					// keys while game is running
					switch int(ev.key.keysym.sym) {
						case C.SDLK_UP:
							// Rotate the tetro
							old_rotation_idx := game.rotation_idx
							game.rotation_idx++
							if game.rotation_idx == TetroSize {
								game.rotation_idx = 0
							}
							game.get_tetro()
							if !game.move_right(0) {
								game.rotation_idx = old_rotation_idx
								game.get_tetro()
							}
							if game.pos_x < 0 {
								game.pos_x = 1
							}
						case C.SDLK_LEFT:
							game.move_right(-1)
						case C.SDLK_RIGHT:
							game.move_right(1)
						case C.SDLK_DOWN:
							game.move_tetro() // drop faster when the player presses <down>
					}
			}
		}
		if game.should_close {
			break
		}
		C.SDL_Delay(20)         // short delay between redraw
	}
	if game.font != voidptr(0) {
		C.TTF_CloseFont(game.font)
	}
}

fn (g &Game) draw_scene() {
	rect := SdlRect {0,0,g.sdl.w,g.sdl.h}
	col := C.SDL_MapRGB(g.sdl.screen.format, 255, 255, 255)
	C.SDL_FillRect(g.sdl.screen, &rect, col)

	g.draw_tetro()
	g.draw_field()

	C.SDL_UpdateTexture(g.sdl.texture, 0, g.sdl.screen.pixels, g.sdl.screen.pitch)
	C.SDL_RenderClear(g.sdl.renderer)
	C.SDL_RenderCopy(g.sdl.renderer, g.sdl.texture, 0, 0)

	g.draw_score()

	C.SDL_RenderPresent(g.sdl.renderer)
}

fn (g mut Game) init_game() {
	g.parse_tetros()
	rand.seed(time.now().uni)
	g.generate_tetro()
	g.field = []array_int // TODO: g.field = [][]int
	// Generate the field, fill it with 0's, add -1's on each edge
	for i := 0; i < FieldHeight + 2; i++ {
		mut row := [0].repeat(FieldWidth + 2)
		row[0] = - 1
		row[FieldWidth + 1] = - 1
		g.field << row
	}
	mut first_row := g.field[0]
	mut last_row := g.field[FieldHeight + 1]
	for j := 0; j < FieldWidth + 2; j++ {
		first_row[j] = - 1
		last_row[j] = - 1
	}
	g.score = 0
	g.state = .running
}

fn (g mut Game) parse_tetros() {
	for b_tetros in BTetros {
		for b_tetro in b_tetros {
			for t in parse_binary_tetro(b_tetro) {
				g.tetros_cache << t
			}
		}
	}
}

fn (g mut Game) run() {
	for {
		if g.state == .running {
			g.move_tetro()
			n := g.delete_completed_lines()
			if n > 0 {
				g.lines += n
			} else {
				if g.lines > 0 {
					if g.lines > 1 {
						g.sdl.actx.audio_pos = g.sdl.actx.samples[2].wav_buffer
						g.sdl.actx.audio_len = g.sdl.actx.samples[2].wav_length
					} else if g.lines == 1 {
						g.sdl.actx.audio_pos = g.sdl.actx.samples[1].wav_buffer
						g.sdl.actx.audio_len = g.sdl.actx.samples[1].wav_length
					}
					g.score += 10 * g.lines * g.lines
					g.lines = 0
				}
			}
		}
		time.sleep_ms(TimerPeriod)      // medium delay between game step
	}
}

fn (g mut Game) move_tetro() {
	// Check each block in current tetro
	for block in g.tetro {
		y := block.y + g.pos_y + 1
		x := block.x + g.pos_x
		// Reached the bottom of the screen or another block?
		// TODO: if g.field[y][x] != 0
		//if g.field[y][x] != 0 {
		row := g.field[y]
		if row[x] != 0 {
			// The new tetro has no space to drop => end of the game
			if g.pos_y < 2 {
				g.state = .gameover
				return
			}
			// Drop it and generate a new one
			g.drop_tetro()
			g.generate_tetro()
			g.sdl.actx.audio_pos = g.sdl.actx.samples[0].wav_buffer
			g.sdl.actx.audio_len = g.sdl.actx.samples[0].wav_length
			return
		}
	}
	g.pos_y++
}

fn (g mut Game) move_right(dx int) bool {
	// Reached left/right edge or another tetro?
	for i := 0; i < TetroSize; i++ {
		tetro := g.tetro[i]
		y := tetro.y + g.pos_y
		x := tetro.x + g.pos_x + dx
		row := g.field[y]
		if row[x] != 0 {
			// Do not move
			return false
		}
	}
	g.pos_x += dx
	return true
}

fn (g &Game) delete_completed_lines() int {
	mut n := 0
	for y := FieldHeight; y >= 1; y-- {
		n += g.delete_completed_line(y)
	}
	return n
}

fn (g &Game) delete_completed_line(y int) int {
	for x := 1; x <= FieldWidth; x++ {
		f := g.field[y]
		if f[x] == 0 {
			return 0
		}
	}
	// Move everything down by 1 position
	for yy := y - 1; yy >= 1; yy-- {
		for x := 1; x <= FieldWidth; x++ {
			mut a := g.field[yy + 1]
			b := g.field[yy]
			a[x] = b[x]
		}
	}
	return 1
}

// Draw a rand tetro index
fn (g mut Game) rand_tetro() int {
	cur := g.tetro_next
	g.tetro_next = rand.next(BTetros.len)
	return cur
}

// Place a new tetro on top
fn (g mut Game) generate_tetro() {
	g.pos_y = 0
	g.pos_x = FieldWidth / 2 - TetroSize / 2
	g.tetro_idx = g.rand_tetro()
	g.rotation_idx = 0
	g.get_tetro()
}

// Get the right tetro from cache
fn (g mut Game) get_tetro() {
	idx := g.tetro_idx * TetroSize * TetroSize + g.rotation_idx * TetroSize
	g.tetro = g.tetros_cache.slice(idx, idx + TetroSize)
}

fn (g &Game) drop_tetro() {
	for i := 0; i < TetroSize; i++ {
		tetro := g.tetro[i]
		x := tetro.x + g.pos_x
		y := tetro.y + g.pos_y
		// Remember the color of each block
		// TODO: g.field[y][x] = g.tetro_idx + 1
		mut row := g.field[y]
		row[x] = g.tetro_idx + 1
	}
}

fn (g &Game) draw_tetro() {
	for i := 0; i < TetroSize; i++ {
		tetro := g.tetro[i]
		g.draw_block(g.pos_y + tetro.y, g.pos_x + tetro.x, g.tetro_idx + 1)
	}
}

fn (g &Game) draw_block(i, j, color_idx int) {
	rect := SdlRect {(j - 1) * BlockSize, (i - 1) * BlockSize,
		BlockSize - 1, BlockSize - 1}
	scol := Colors[color_idx]
	rr := scol.r
	gg := scol.g
	bb := scol.b
	col := C.SDL_MapRGB(g.sdl.screen.format, rr, gg, bb)
	C.SDL_FillRect(g.sdl.screen, &rect, col)
}

fn (g &Game) draw_field() {
	for i := 1; i < FieldHeight + 1; i++ {
		for j := 1; j < FieldWidth + 1; j++ {
			f := g.field[i]
			if f[j] > 0 {
				g.draw_block(i, j, f[j])
			}
		}
	}
}

fn (g &Game) draw_text(x int, y int, text string, rr int, gg int, bb int) {
	tcol := SdlColor {byte(3), byte(2), byte(1), byte(0)}
	tsurf := C.TTF_RenderText_Solid(g.font, text.str, tcol)
	ttext := C.SDL_CreateTextureFromSurface(g.sdl.renderer, tsurf)
	texw := 0
	texh := 0
	C.SDL_QueryTexture(ttext, 0, 0, &texw, &texh)
	dstrect := SdlRect { x, y, texw, texh }
	C.SDL_RenderCopy(g.sdl.renderer, ttext, 0, &dstrect)
	C.SDL_DestroyTexture(ttext)
	C.SDL_FreeSurface(tsurf)
}

fn (g &Game) draw_score() {
	if g.font != voidptr(0) {
		g.draw_text(1, 2, 'score: ' + g.score.str() + ' nxt=' + g.tetro_next.str(), 0, 0, 0)
		if g.state == .gameover {
			g.draw_text(1, WinHeight / 2 + 0 * TextSize, 'Game Over', 0, 0, 0)
			g.draw_text(1, WinHeight / 2 + 2 * TextSize, 'SPACE to restart', 0, 0, 0)
		} else if g.state == .paused {
			g.draw_text(1, WinHeight / 2 + 0 * TextSize, 'Game Paused', 0, 0, 0)
			g.draw_text(1, WinHeight / 2 + 2 * TextSize, 'SPACE to resume', 0, 0, 0)
		}
	}
}

fn parse_binary_tetro(t_ int) []Block {
	mut t := t_
	res := [Block{}].repeat(4)
	mut cnt := 0
	horizontal := t == 9// special case for the horizontal line
	for i := 0; i <= 3; i++ {
		// Get ith digit of t
		p := int(math.pow(10, 3 - i))
		mut digit := int(t / p)
		t %= p
		// Convert the digit to binary
		for j := 3; j >= 0; j-- {
			bin := digit % 2
			digit /= 2
			if bin == 1 || (horizontal && i == TetroSize - 1) {
				// TODO: res[cnt].x = j
				// res[cnt].y = i
				mut point := &res[cnt]
				point.x = j
				point.y = i
				cnt++
			}
		}
	}
	return res
}
