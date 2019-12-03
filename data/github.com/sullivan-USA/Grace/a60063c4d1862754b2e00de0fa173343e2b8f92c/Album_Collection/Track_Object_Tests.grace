//Ryan Sullivan
//New Beginnings
//Album Collection Lab
//Date: 8-6-15

//The purpose of this program is to test that the functions in theTrack_Object.grace file produces 
//the expected results.

dialect "minitest"
import "trackLab" as trackFile
//imports the trackLab file for using the newTrack method of creating track objects.

testSuite {
    def track1 = trackFile.newTrack.trackArtist "AFI" trackTitle "No Dave Party" 
        trackYear 1996
    def listQuery1 = list.with("AFI", "No Dave Party")
    def listQuery2 = list.with("AFI", "None")
    def listQuery3 = list.with("None", "None")
    def stringQuery1 = "AFI"
    def stringQuery2 = "Nothing"
    
//Track Tests
    test "artist retrieval" by {
        assert (track1.trackArtist) shouldBe "AFI"
    }    
    
    test "title retrieval" by {
        assert (track1.trackTitle) shouldBe "No Dave Party"
    }
    
    test "year retrieval" by {
        assert (track1.trackYear) shouldBe 1996
    }
    
//Keyword and Phrase Tests
    test "containsAllKeywords - both" by {
        assert (track1.containsAllKeywords(listQuery1)) shouldBe (true)
    }
    
    test "containsAllKeywords - one" by {
        assert (track1.containsAllKeywords(listQuery2)) shouldBe (false)
    }
    
    test "containsAllKeywords - none" by {
        assert (track1.containsAllKeywords(listQuery3)) shouldBe (false)
    }
    
    test "containsAnyKeywords - one" by {
        assert (track1.containsAnyKeywords(listQuery2)) shouldBe (true)
    }
    
    test "containsAnyKeywords - none" by {
        assert (track1.containsAnyKeywords(listQuery3)) shouldBe (false)
    }

    test "containsPhrase - true" by {
        assert (track1.containsPhrase(stringQuery1)) shouldBe (true)
    }    
    
    test "containsPhrase - false" by {
        assert (track1.containsPhrase(stringQuery2)) shouldBe (false)
    }    
}
