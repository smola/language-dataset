public class MIDIFlowerPetal
{
    // Description
    //     This is an abstract superclass. You don't instantiate it -- rather,
    //     you inherit from it and implement the MIDI message(s) you wish to
    //     respond to.
    //     Due to its simplicity, MIDI Note Off is implemented for you.

    // data + accessor
    // ----------------------------------------------------------------- //

    Event _events[MIDI.KEY_COUNT];

    function Event[] events()
    {
        return _events;
    }

    // MIDI messages
    // ----------------------------------------------------------------- //

    // Description
    //     Respond to a MIDI Note Off message.
    //     If you override this method, you must include:
    //         this.events()[key].signal();
    //     in your implementation.
    // Parameter
    //     key -- MIDI key, i.e. MIDI note number. Range: 0 - 127.
    //     velocity -- MIDI key velocity. Range: 0 - 127.
    // Return
    //     void
    function void noteOff(int key, int velocity)
    {
        this.events()[key].signal();
    }

    // Description
    //     Respond to a MIDI Note On message.
    //     If you override this method, you must include:
    //         this.events()[key] => now;
    //     in your implementation.
    // Parameter
    //     key -- MIDI key, i.e. MIDI note number. Range: 0 - 127.
    //     velocity -- MIDI key velocity. Range: 0 - 127.
    // Return
    //     void
    function void noteOn(int key, int velocity) {}

    // Description
    //     Respond to a MIDI Key Pressure message.
    //     This is per-key aftertouch, also known as polyphonic aftertouch.
    // Parameter
    //     key -- MIDI key, i.e. MIDI note number. Range: 0 - 127.
    //     pressure -- MIDI key pressure. Range: 0 - 127.
    // Return
    //     void
    function void keyPressure(int key, int pressure) {}

    // Description
    //     Respond to a MIDI Control Change message.
    // Parameter
    //     controller -- MIDI controller number. Range: 0 - 119.
    //     value -- MIDI controller value. Range: 0 - 127.
    // Return
    //     void
    function void controlChange(int controller, int value) {}

    // Description
    //     Respond to a MIDI Program Change message.
    // Parameter
    //     program -- MIDI program number. Range: 0 - 127.
    // Return
    //     void
    function void programChange(int program) {}

    // Description
    //     Respond to a MIDI Channel Pressure message.
    //     This is per-channel aftertouch.
    // Parameter
    //     pressure -- MIDI channel pressure. Range: 0 - 127.
    // Return
    //     void
    function void channelPressure(int pressure) {}

    // Description
    //     Respond to a MIDI Pitch Bend message.
    //     LSB and MSB vary by manufacturer.
    // Parameter
    //     LSB -- 7-bit integer value. Range: 0 - 127.
    //     MSB -- 7-bit integer value. Range: 0 - 127.
    // Return
    //     void
    function void pitchBend(int LSB, int MSB) {}

    // Description
    //     Respond to a MIDI System message, including System Exclusive.
    // Parameter
    //     message -- MIDI message.
    // Return
    //     void
    function void system(MidiMsg @message) {}
}
