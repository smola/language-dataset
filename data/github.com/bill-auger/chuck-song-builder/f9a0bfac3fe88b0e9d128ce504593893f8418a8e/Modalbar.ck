public class Modalbar extends SongPart
{
  1.0            => float ON_RATIO ;
  1.0 - ON_RATIO => float OFF_RATIO ;


  ModalBar instrument ;


  fun void setInstrument(ModalBar aModalBar) { aModalBar @=> instrument ; }

  fun dur playNote(int noteN)
  {
    if (frequencies[noteN] >= 20.0)
    {
      frequencies[noteN] => instrument.freq ;
      1.0                => instrument.strike ;
      1.0                => instrument.noteOn ;
    }
    return durations[noteN] * ON_RATIO ;

//<<< "playNote() frequency=" , frequencies[noteN] , " duration=" , durations[noteN] >>> ;
  }

  fun dur stopNote(int noteN) { return durations[noteN] * OFF_RATIO ; }
}
