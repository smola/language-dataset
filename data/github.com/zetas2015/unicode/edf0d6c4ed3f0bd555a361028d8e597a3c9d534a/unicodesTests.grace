import "gUnit" as gU
import "unicodes" as unicodes

def uc = unicodes.unicode

def unicodesTest = object {
  class forMethod(m) {
    inherits gU.testCaseNamed(m)

    method testName0x1B44 {
      assert (uc.name(0x1B44)) shouldBe "BALINESE ADEG ADEG"
    }

    method testName0x1F6BB {
      assert (uc.name(0x1F6BB)) shouldBe "RESTROOM"
    }

    method testName0x0C4D {
      assert (uc.name(0x0C4D)) shouldBe "TELUGU SIGN VIRAMA"
    }

    method testName0x06E6 {
      assert (uc.name(0x06E6)) shouldBe "ARABIC SMALL YEH"
    }

    method testName0xFE7B {
      assert (uc.name(0xFE7B)) shouldBe "ARABIC KASRA MEDIAL FORM"
    }

    method testName0x1F6B1 {
      assert (uc.name(0x1F6B1)) shouldBe "NON-POTABLE WATER SYMBOL"
    }

    method testName0xA652 {
      assert (uc.name(0xA652)) shouldBe "CYRILLIC CAPITAL LETTER IOTIFIED YAT"
    }

    method testName0x1FF8 {
      assert (uc.name(0x1FF8)) shouldBe "GREEK CAPITAL LETTER OMICRON WITH VARIA"
    }

    method testName0x1380 {
      assert (uc.name(0x1380)) shouldBe "ETHIOPIC SYLLABLE SEBATBEIT MWA"
    }

    method testName0x118CE {
      assert (uc.name(0x118CE)) shouldBe "WARANG CITI SMALL LETTER YUJ"
    }

    method testName0x118C7 {
      assert (uc.name(0x118C7)) shouldBe "WARANG CITI SMALL LETTER UU"
    }

    method testName0x1F6BD {
      assert (uc.name(0x1F6BD)) shouldBe "TOILET"
    }

    method testName0xE01D4 {
      assert (uc.name(0xE01D4)) shouldBe "VARIATION SELECTOR-229"
    }

    method testName0x2AC9 {
      assert (uc.name(0x2AC9)) shouldBe "SUBSET OF ABOVE ALMOST EQUAL TO"
    }

    method testName0x1A64 {
      assert (uc.name(0x1A64)) shouldBe "TAI THAM VOWEL SIGN TALL AA"
    }

    method testName0x29BF {
      assert (uc.name(0x29BF)) shouldBe "CIRCLED BULLET"
    }

    method testName0xA115 {
      assert (uc.name(0xA115)) shouldBe "YI SYLLABLE DUX"
    }

    method testName0x1B4B {
      assert (uc.name(0x1B4B)) shouldBe "BALINESE LETTER ASYURA SASAK"
    }

    method testName0xA657 {
      assert (uc.name(0xA657)) shouldBe "CYRILLIC SMALL LETTER IOTIFIED A"
    }

    method testName0x1D6D2 {
      assert (uc.name(0x1D6D2)) shouldBe "MATHEMATICAL BOLD SMALL RHO"
    }

    method testCharacter0x1106E {
      assert (uc.character("BRAHMI DIGIT EIGHT")) shouldBe 0x1106E
    }

    method testCharacter0xA32C {
      assert (uc.character("YI SYLLABLE SSI")) shouldBe 0xA32C
    }

    method testCharacter0x1D6D5 {
      assert (uc.character("MATHEMATICAL BOLD SMALL TAU")) shouldBe 0x1D6D5
    }

    method testCharacter0x037A {
      assert (uc.character("GREEK YPOGEGRAMMENI")) shouldBe 0x037A
    }

    method testCharacter0x0C4A {
      assert (uc.character("TELUGU VOWEL SIGN O")) shouldBe 0x0C4A
    }

    method testCharacter0xFE7B {
      assert (uc.character("ARABIC KASRA MEDIAL FORM")) shouldBe 0xFE7B
    }

    method testCharacter0x1FF9 {
      assert (uc.character("GREEK CAPITAL LETTER OMICRON WITH OXIA")) shouldBe 0x1FF9
    }

    method testCharacter0xA119 {
      assert (uc.character("YI SYLLABLE DUR")) shouldBe 0xA119
    }

    method testCharacter0x2F91 {
      assert (uc.character("KANGXI RADICAL WEST")) shouldBe 0x2F91
    }

    method testCharacter0x0F97 {
      assert (uc.character("TIBETAN SUBJOINED LETTER JA")) shouldBe 0x0F97
    }

    method testCharacter0x1053F {
      assert (uc.character("CAUCASIAN ALBANIAN LETTER INYA")) shouldBe 0x1053F
    }

    method testCharacter0x30D2 {
      assert (uc.character("KATAKANA LETTER HI")) shouldBe 0x30D2
    }

    method testCharacter0x1297 {
      assert (uc.character("ETHIOPIC SYLLABLE NWA")) shouldBe 0x1297
    }

    method testCharacter0x1208C {
      assert (uc.character("CUNEIFORM SIGN E OVER E NUN OVER NUN")) shouldBe 0x1208C
    }

    method testCharacter0xA656 {
      assert (uc.character("CYRILLIC CAPITAL LETTER IOTIFIED A")) shouldBe 0xA656
    }

    method testCharacter0x2F95 {
      assert (uc.character("KANGXI RADICAL VALLEY")) shouldBe 0x2F95
    }

    method testCharacter0xF968 {
      assert (uc.character("CJK COMPATIBILITY IDEOGRAPH-F968")) shouldBe 0xF968
    }

    method testCharacter0x06EE {
      assert (uc.character("ARABIC LETTER DAL WITH INVERTED V")) shouldBe 0x06EE
    }

    method testCharacter0x0F96 {
      assert (uc.character("TIBETAN SUBJOINED LETTER CHA")) shouldBe 0x0F96
    }

    method testCharacter0x18BB {
      assert (uc.character("CANADIAN SYLLABICS NOY")) shouldBe 0x18BB
    }

    method testMC0x1FFD {
      assert (uc.majorCategory(0x1FFD)) shouldBe "S"
    }

    method testMC0x1290 {
      assert (uc.majorCategory(0x1290)) shouldBe "L"
    }

    method testMC0xFF86 {
      assert (uc.majorCategory(0xFF86)) shouldBe "L"
    }

    method testMC0x1D6DD {
      assert (uc.majorCategory(0x1D6DD)) shouldBe "L"
    }

    method testMC0x06EC {
      assert (uc.majorCategory(0x06EC)) shouldBe "M"
    }

    method testMC0x0F9C {
      assert (uc.majorCategory(0x0F9C)) shouldBe "M"
    }

    method testMC0x2F92B {
      assert (uc.majorCategory(0x2F92B)) shouldBe "L"
    }

    method testMC0x1D6D1 {
      assert (uc.majorCategory(0x1D6D1)) shouldBe "L"
    }

    method testMC0x1D6DC {
      assert (uc.majorCategory(0x1D6DC)) shouldBe "L"
    }

    method testMC0x0A08 {
      assert (uc.majorCategory(0x0A08)) shouldBe "L"
    }

    method testMC0x138B {
      assert (uc.majorCategory(0x138B)) shouldBe "L"
    }

    method testMC0x2045 {
      assert (uc.majorCategory(0x2045)) shouldBe "P"
    }

    method testMC0xA11C {
      assert (uc.majorCategory(0xA11C)) shouldBe "L"
    }

    method testMC0x1D689 {
      assert (uc.majorCategory(0x1D689)) shouldBe "L"
    }

    method testMC0x00C5 {
      assert (uc.majorCategory(0x00C5)) shouldBe "L"
    }

    method testMC0x16A1E {
      assert (uc.majorCategory(0x16A1E)) shouldBe "L"
    }

    method testMC0x2F927 {
      assert (uc.majorCategory(0x2F927)) shouldBe "L"
    }

    method testMC0x129F {
      assert (uc.majorCategory(0x129F)) shouldBe "L"
    }

    method testMC0x1168F {
      assert (uc.majorCategory(0x1168F)) shouldBe "L"
    }

    method testMC0x1106B {
      assert (uc.majorCategory(0x1106B)) shouldBe "N"
    }

    method testGC0x118C1 {
      assert (uc.category(0x118C1)) shouldBe "Ll"
    }

    method testGC0x2045 {
      assert (uc.category(0x2045)) shouldBe "Ps"
    }

    method testGC0x13EE {
      assert (uc.category(0x13EE)) shouldBe "Lo"
    }

    method testGC0x17D3 {
      assert (uc.category(0x17D3)) shouldBe "Mn"
    }

    method testGC0x13EF {
      assert (uc.category(0x13EF)) shouldBe "Lo"
    }

    method testGC0x1168B {
      assert (uc.category(0x1168B)) shouldBe "Lo"
    }

    method testGC0x1F772 {
      assert (uc.category(0x1F772)) shouldBe "So"
    }

    method testGC0x0C46 {
      assert (uc.category(0x0C46)) shouldBe "Mn"
    }

    method testGC0x0371 {
      assert (uc.category(0x0371)) shouldBe "Ll"
    }

    method testGC0x2F94 {
      assert (uc.category(0x2F94)) shouldBe "So"
    }

    method testGC0x1208D {
      assert (uc.category(0x1208D)) shouldBe "Lo"
    }

    method testGC0xA115 {
      assert (uc.category(0xA115)) shouldBe "Lo"
    }

    method testGC0x1A65 {
      assert (uc.category(0x1A65)) shouldBe "Mn"
    }

    method testGC0x1106A {
      assert (uc.category(0x1106A)) shouldBe "Nd"
    }

    method testGC0xFF8B {
      assert (uc.category(0xFF8B)) shouldBe "Lo"
    }

    method testGC0x11369 {
      assert (uc.category(0x11369)) shouldBe "Mn"
    }

    method testGC0x118CF {
      assert (uc.category(0x118CF)) shouldBe "Ll"
    }

    method testGC0x1FFC {
      assert (uc.category(0x1FFC)) shouldBe "Lt"
    }

    method testGC0xE01D5 {
      assert (uc.category(0xE01D5)) shouldBe "Mn"
    }

    method testGC0xA65F {
      assert (uc.category(0xA65F)) shouldBe "Ll"
    }

    method testNoSuchCharacter1 {
      assert {uc.name(0xFFFFFF)} shouldRaise (unicodes.NoSuchCharacter)
    }

    method testNoSuchCharacter2 {
      assert {uc.character("I AM NOT A UNICODE")} shouldRaise (unicodes.NoSuchCharacter)
    }

    method testAmbiguousName {
      assert {uc.character("<control>")} shouldRaise (unicodes.AmbiguousName)
    }

  }
}


def unicodesTests = gU.testSuite.fromTestMethodsIn(unicodesTest)
print "testing"
unicodesTests.runAndPrintResults