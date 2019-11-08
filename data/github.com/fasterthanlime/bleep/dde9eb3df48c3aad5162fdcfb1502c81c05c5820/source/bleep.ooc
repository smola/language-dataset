
use sdl2, sdl2-mixer
import sdl2/[Core, Mixer]

use deadlogger
import deadlogger/Log

import structs/[ArrayList, HashMap]
import io/File

Bleep: class {

    logger := static Log getLogger("bleep")
    musicStopListeners := ArrayList<BleepListener> new()
    musicCache := HashMap<String, MixMusic> new()
    chunkCache := HashMap<String, MixChunk> new()

    name: String

    instance: static This

    init: func {
        SDL init(SDL_INIT_EVERYTHING)

        if (Mix openAudio(44100, MixFormat default_, 2, 1024)) {
            err := Mix getError() toString()
            logger error("Couldn't initialize SDL mixer: #{err}")
            raise("Error initializing SDL mixer: #{err}")
        }

        if (instance) {
            logger error("Can't initialize two instances of bleep")
            raise("Can't initialize two instances of bleep")
        }

        instance = this
        Mix hookMusicFinished(_musicFinishedShim)
    }

    _musicFinishedShim: static func {
        // SDL2_mixer is stupid and ugly and doesn't accept
        // a user data pointer to pass to the callback for
        // when the music has stopped, so we have to do this
        // ugly hack.
        instance _musicFinished()
    }

    _musicFinished: func {
        for (l in musicStopListeners) {
            l f()
        }
    }

    allocateChannels: func (numChannels: Int) {
        Mix allocateChannels(numChannels)
    }

    playMusic: func (path: String, loops: Int) {
        logger info("Loading music #{path}")
        absolutePath := File new(path) getAbsolutePath()
        mus := musicCache get(absolutePath)
        if (!mus) {
            mus = Mix loadMus(path)

            if (!mus) {
                logger error("Couldn't load music #{path}: #{Mix getError() toString()}")
                return
            }

            musicCache put(absolutePath, mus)
        }

        mus play(loops)
    } 

    stopMusic: func {
        Mix haltMusic()
    }

    fadeMusic: func (milliseconds: Int) {
        Mix fadeOutMusic(milliseconds)
    }

    setMusicPos: func (pos: Double) {
        Mix setMusicPosition(pos)
    }

    musicPlaying?: func -> Bool {
        Mix playingMusic()
    }

    musicPaused?: func -> Bool {
        Mix pausedMusic()
    }

    onMusicStop: func (f: Func) -> BleepListener {
        bl := BleepListener new(f)
        musicStopListeners add(bl)
        bl
    }

    unsubscribeOnMusicStop: func (bl: BleepListener) {
        musicStopListeners remove(bl)
    }

    /**
     * Set the volume of all channels, or of the specified
     * channel (if second arg given)
     */
    setVolume: func (volume: Float, channel := -1) {
        Mix volume(channel, (volume * 128.0) as Int)
    }

    setVolumeMusic: func (volume: Float) {
        Mix volumeMusic((volume * 128.0) as Int)
    }

    getVolume: func (channel := -1) -> Float {
        Mix volume(channel, -1) as Float / 128.0
    }

    getVolumeMusic: func -> Float {
        Mix volumeMusic(-1) as Float / 128.0
    }

    /**
     * Load a sample in WAV format
     * If there's an error (file can't be found/read, invalid format),
     * null will be returned instead.
     */
    loadSample: func (path: String) -> Sample {
        logger info("Loading sample #{path}")
        absolutePath := File new(path) getAbsolutePath()
        chunk := chunkCache get(absolutePath)

        if (!chunk) {
            chunk = Mix loadWav(path)
            if (!chunk) {
                logger error("Couldn't load sample #{path}: #{Mix getError() toString()}")
                return null
            }
            chunkCache put(absolutePath, chunk)
        }

        Sample new(chunk)
    }

    destroy: func {
        Mix closeAudio()
    }

}

Sample: class {

    chunk: MixChunk
    channel := -1

    init: func (=chunk)

    setVolume: func (volume: Float) {
        chunk volume((volume * 128.0) as Int)
    }

    getVolume: func -> Float {
        (chunk volume(-1) as Float) / 128.0
    }

    play: func (loops: Int, channel := -1) {
        channel = chunk play(channel, loops)
    }

    stop: func {
        if (channel != -1 && (Mix getChunk(channel) == chunk)) {
            Mix haltChannel(channel)
            channel = -1
        }
    }

    free: func {
        chunk free()
    }

}

BleepListener: class {
    f: Func

    init: func (=f)
}

