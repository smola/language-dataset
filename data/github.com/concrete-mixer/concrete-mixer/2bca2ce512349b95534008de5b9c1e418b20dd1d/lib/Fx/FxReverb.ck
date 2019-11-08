/*----------------------------------------------------------------------------
    Concrète Mixer - an ambient sound jukebox for the Raspberry Pi

    Copyright (c) 2014-2016 Stuart McDonald  All rights reserved.
        https://github.com/concrete-mixer/concrete-mixer

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
    U.S.A.
-----------------------------------------------------------------------------*/

public class FxReverb extends Fx {
    GVerb gverb;
    JCRev jcrev;

    Chooser c;

    if ( Config.rpi ) {
        input => jcrev => output;
    }
    // Use something gruntier if pi not being used
    else {
        input => gverb => output;

        // TODO: randomize somewhat
        // for reference, the following are GVerb defaults:
        //      const float maxroomsize = 300.0f;
        //      float roomsize = 30.0f;
        //      float revtime = 5.0f;
        //      float damping = 0.8f;
        //      float spread = 15.0f;
        //      float inputbandwidth = 0.5f;
        //      float drylevel = 0.6f; //-1.9832f;
        //      float earlylevel = 0.4f; //-1.9832f;
        //      float taillevel = 0.5f;
        // See also https://ccrma.stanford.edu/~spencer/ckdoc/chugins.html#GVerb

        c.getInt(1, 2) => int choice;
        0 => gverb.dry;

        // biiig reverb
        if ( choice == 1 ) {
            5.0::second => gverb.revtime;
            50.0 => gverb.roomsize;
            0.2 => gverb.damping;
            1.0 => gverb.tail;
            0 => gverb.early;
        }

        // tight reverb
        if ( choice == 2 ) {
            0.2::second => gverb.revtime;
            0.0 => gverb.damping;
            10.0 => gverb.roomsize;
            1.0 => gverb.tail;
            1.0 => gverb.early;
            1.0 => gverb.bandwidth;
        }

    }

    fun string idString() { return "FxReverb"; }
}
