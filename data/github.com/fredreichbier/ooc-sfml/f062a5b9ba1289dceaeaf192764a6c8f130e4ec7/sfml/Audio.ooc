include SFML/Audio

use csfml-audio

SoundStatus: cover from sfSoundStatus

Listener: cover {
    setGlobalVolume: extern(sfListener_SetGlobalVolume) static func (volume: Float)
    getGlobalVolume: extern(sfListener_GetGlobalVolume) static func -> Float
    setPosition: extern(sfListener_SetPosition) static func (posX: Float, posY: Float, posZ: Float)
    getPosition: extern(sfListener_GetPosition) static func (posX: Float*, posY: Float*, posZ: Float*)
    setTarget: extern(sfListener_SetTarget) static func (targetX: Float, targetY: Float, targetZ: Float)
    getTarget: extern(sfListener_GetTarget) static func (targetX: Float*, targetY: Float*, targetZ: Float*)
}

Music: cover from sfMusic* {
    new: extern(sfMusic_CreateFromFile) static func ~fromFile (filename: Char*) -> Music
    new: extern(sfMusic_CreateFromMemory) static func ~fromMemory (data: Char*, sizeInBytes: SizeT) -> Music
    destroy: extern(sfMusic_Destroy) func
    setLoop: extern(sfMusic_SetLoop) func (loop: Bool)
    getLoop: extern(sfMusic_GetLoop) func -> Bool
    getDuration: extern(sfMusic_GetDuration) func -> Float
    play: extern(sfMusic_Play) func
    pause: extern(sfMusic_Pause) func
    stop: extern(sfMusic_Stop) func
    getChannelsCount: extern(sfMusic_GetChannelsCount) func -> UInt
    getSampleRate: extern(sfMusic_GetSampleRate) func -> UInt
    getStatus: extern(sfMusic_GetStatus) func -> SoundStatus
    getPlayingOffset: extern(sfMusic_GetPlayingOffset) func -> Float
    setPitch: extern(sfMusic_SetPitch) func (pitch: Float)
    setVolume: extern(sfMusic_SetVolume) func (volume: Float)
    setPosition: extern(sfMusic_SetPosition) func (x: Float, y: Float, z: Float)
    setRelativeToListener: extern(sfMusic_SetRelativeToListener) func (relative: Bool)
    setMinDistance: extern(sfMusic_SetMinDistance) func (minDistance: Float)
    setAttenuation: extern(sfMusic_SetAttenuation) func (attenuation: Float)
    getPitch: extern(sfMusic_GetPitch) func -> Float
    getVolume: extern(sfMusic_GetVolume) func -> Float
    getPosition: extern(sfMusic_GetPosition) func (x: Float*, y: Float*, z: Float*)
    isRelativeToListener: extern(sfMusic_IsRelativeToListener) func -> Bool
    getMinDistance: extern(sfMusic_GetMinDistance) func -> Float
    getAttenuation: extern(sfMusic_GetAttenuation) func -> Float
}

Sound: cover from sfSound* {
    new: extern(sfSound_Create) static func -> Sound
    destroy: extern(sfSound_Destroy) func
    play: extern(sfSound_Play) func
    pause: extern(sfSound_Pause) func
    stop: extern(sfSound_Stop) func
    setBuffer: extern(sfSound_SetBuffer) func (buffer: SoundBuffer)
    getBuffer: extern(sfSound_GetBuffer) func -> SoundBuffer
    setLoop: extern(sfSound_SetLoop) func (loop: Bool)
    getLoop: extern(sfSound_GetLoop) func -> Bool
    getStatus: extern(sfSound_GetStatus) func -> SoundStatus
    setPitch: extern(sfSound_SetPitch) func (pitch: Float)
    setVolume: extern(sfSound_SetVolume) func (volume: Float)
    setPosition: extern(sfSound_SetPosition) func (x: Float, y: Float, z: Float)
    setRelativeToListener: extern(sfSound_SetRelativeToListener) func (relative: Bool)
    setMinDistance: extern(sfSound_SetMinDistance) func (minDistance: Float)
    setAttenuation: extern(sfSound_SetAttenuation) func (attenuation: Float)
    setPlayingOffset: extern(sfSound_SetPlayingOffset) func (timeOffset: Float)
    getPitch: extern(sfSound_GetPitch) func -> Float
    getVolume: extern(sfSound_GetVolume) func -> Float
    getPosition: extern(sfSound_GetPosition) func (x: Float*, y: Float*, z: Float*)
    isRelativeToListener: extern(sfSound_IsRelativeToListener) func -> Bool
    getMinDistance: extern(sfSound_GetMinDistance) func -> Float
    getAttenuation: extern(sfSound_GetAttenuation) func -> Float
    getPlayingOffset: extern(sfSound_GetPlayingOffset) func -> Float
}

SoundBuffer: cover from sfSoundBuffer* {
    new: extern(sfSoundBuffer_CreateFromFile) static func ~fromFile (filename: Char*) -> SoundBuffer
    new: extern(sfSoundBuffer_CreateFromMemory) static func ~fromMemory (data: Char*, sizeInBytes: SizeT) -> SoundBuffer
    new: extern(sfSoundBuffer_CreateFromSamples) static func ~fromSamples (samples: Int16*, samplesCount: SizeT, channelsCount: UInt, sampleRate: UInt) -> SoundBuffer
    destroy: extern(sfSoundBuffer_Destroy) func
    saveToFile: extern(sfSoundBuffer_SaveToFile) func (filename: Char*) -> Bool
    getSamples: extern(sfSoundBuffer_GetSamples) func -> Int16*
    getSamplesCount: extern(sfSoundBuffer_GetSamplesCount) func -> SizeT
    getSampleRate: extern(sfSoundBuffer_GetSampleRate) func -> UInt
    getChannelsCount: extern(sfSoundBuffer_GetChannelsCount) func -> UInt
    getDuration: extern(sfSoundBuffer_GetDuration) func -> Float
}

SoundRecorder: cover from sfSoundRecorder* {
    new: extern(sfSoundRecorder_Create) static func (onStart: Func, onProcess: Func, onStop: Func, userData: Void*) -> SoundRecorder
    destroy: extern(sfSoundRecorder_Destroy) func
    start: extern(sfSoundRecorder_Start) func (sampleRate: UInt)
    stop: extern(sfSoundRecorder_Stop) func
    getSampleRate: extern(sfSoundRecorder_GetSampleRate) func -> UInt
    canCapture: extern(sfSoundRecorder_CanCapture) static func -> Bool
}

SoundBufferRecorder: cover from sfSoundBufferRecorder* {
    new: extern(sfSoundBufferRecorder_Create) static func -> SoundBufferRecorder
    destroy: extern(sfSoundBufferRecorder_Destroy) func
    start: extern(sfSoundBufferRecorder_Start) func (sampleRate: UInt)
    stop: extern(sfSoundBufferRecorder_Stop) func
    getSampleRate: extern(sfSoundBufferRecorder_GetSampleRate) func -> UInt
    getBuffer: extern(sfSoundBufferRecorder_GetBuffer) func -> SoundBuffer
}

