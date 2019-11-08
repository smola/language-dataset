identification division.
function-id. IntegerInWords.

environment division.
configuration section.
    repository.
    function all intrinsic
    function ThreeDigitNumberInWords.

data division.
local-storage section.
01 NumberToConvert pic 999999999 value zeros.

01 WorkingValuesTable.
    02 Units pic 999 value zeroes.
    02 UnitsInWords pic x(30) value spaces.
    02 Thousands pic 999 value zeroes.
    02 ThousandsInWords pic x(30) value spaces.
    02 Millions pic 999 value zeroes.
    02 MillionsInWords pic x(30) value spaces.

linkage section.
01 InputValue pic x any length.
01 NumberInWords  pic x(255).

procedure division using InputValue returning NumberInWords.
Main section.
    initialize NumberToConvert, WorkingValuesTable
    move InputValue to NumberToConvert
    move NumberToConvert(1:3) to Millions
    move NumberToConvert(4:3) to Thousands
    move NumberToConvert(7:3) to Units

    move ThreeDigitNumberInWords(Units) to UnitsInWords
    move ThreeDigitNumberInWords(Thousands) to ThousandsInWords
    move ThreeDigitNumberInWords(Millions) to MillionsInWords

    evaluate Millions also Thousands also Units
    when 0 also 0 also > 0
        move UnitsInWords to NumberInWords
    when 0 also > 0 also 0
        move concatenate(trim(ThousandsInWords), " thousand") to NumberInWords
    when 0 also > 0 also < 100
        move concatenate(trim(ThousandsInWords), " thousand and ",
        UnitsInWords) to NumberInWords
    when 0 also > 0 also >= 100
        move concatenate(trim(ThousandsInWords), " thousand, ",
        UnitsInWords) to NumberInWords
    when > 0 also 0 also 0
        move concatenate(trim(MillionsInWords), " million") to NumberInWords
    when > 0 also 0 also < 100
        move concatenate(trim(MillionsInWords), " million and ",
        UnitsInWords) to NumberInWords
    when > 0 also 0 also >= 100
        move concatenate(trim(MillionsInWords), " million, ",
        UnitsInWords) to NumberInWords
    when > 0 also > 0 also 0
        move concatenate(trim(MillionsInWords), " million, ",
        trim(ThousandsInWords), " thousand") to NumberInWords
    when > 0 also > 0 also < 100
        move concatenate(trim(MillionsInWords), " million, ",
        trim(ThousandsInWords), " thousand and ",
        trim(UnitsInWords)) to NumberInWords
    when > 0 also > 0 also >= 100
        move concatenate(trim(MillionsInWords), " million, ",
        trim(ThousandsInWords), " thousand, ",
        trim(UnitsInWords)) to NumberInWords
    end-evaluate

    goback
    .

end function IntegerInWords.

identification division.
function-id. ThreeDigitNumberInWords.

environment division.
configuration section.
    repository.
    function all intrinsic.

data division.
local-storage section.
01 WorkingValue pic 999.
01 CurrentDigit pic 9.
01 CurrentDigitInWords pic x(25).
01 NumberHasHundreds pic 9 binary value 0.

linkage section.
01 InputValue pic 999.
01 NumberInWords pic x(255).

procedure division using InputValue returning NumberInWords.
Main section.
    move InputValue to WorkingValue
    initialize NumberHasHundreds

    if InputValue less than 10
        move WorkingValue(3:1) to CurrentDigit
        perform UnitValues
        move trim(CurrentDigitInWords) to NumberInWords
        goback
    end-if

    if WorkingValue(1:1) greater than zero
        move WorkingValue(1:1) to CurrentDigit
        perform UnitValues
        move concatenate(trim(CurrentDigitInWords), " hundred") to NumberInWords
        move 1 to NumberHasHundreds
    end-if

    if WorkingValue(2:1) greater than zero
        if WorkingValue(2:1) greater than 1
            move WorkingValue(2:1) to CurrentDigit
            perform Tens
            if NumberHasHundreds equals 1
                move concatenate(trim(NumberInWords), " and ", trim(CurrentDigitInWords)) to NumberInWords
            else
                move CurrentDigitInWords to NumberInWords
            end-if
            if WorkingValue(3:1) greater than zero
                move WorkingValue(3:1) to CurrentDigit
                perform UnitValues
                move concatenate(trim(NumberInWords), "-", trim(CurrentDigitInWords)) to NumberInWords
            end-if
        else
            move WorkingValue(3:1) to CurrentDigit
            perform Teens
            if NumberHasHundreds equals 1
                move concatenate(trim(NumberInWords), " and ", trim(CurrentDigitInWords)) to NumberInWords
            else
                move CurrentDigitInWords to NumberInWords
            end-if
        end-if
    else
        if WorkingValue(3:1) greater than zero
            move WorkingValue(3:1) to CurrentDigit
            perform UnitValues
            move concatenate(trim(NumberInWords), " and ", trim(CurrentDigitInWords)) to NumberInWords
        end-if
    end-if
    goback
    .
EndMain.

UnitValues section.
    evaluate CurrentDigit
        when 1 move "one" to CurrentDigitInWords
        when 2 move "two" to CurrentDigitInWords
        when 3 move "three" to CurrentDigitInWords
        when 4 move "four" to CurrentDigitInWords
        when 5 move "five" to CurrentDigitInWords
        when 6 move "six" to CurrentDigitInWords
        when 7 move "seven" to CurrentDigitInWords
        when 8 move "eight" to CurrentDigitInWords
        when 9 move "nine" to CurrentDigitInWords
    end-evaluate
    exit section
    .

Teens section.
    evaluate CurrentDigit
        when 0 move "ten" to CurrentDigitInWords
        when 1 move "eleven" to CurrentDigitInWords
        when 2 move "twelve" to CurrentDigitInWords
        when 3 move "thirteen" to CurrentDigitInWords
        when 4 move "fourteen" to CurrentDigitInWords
        when 5 move "fifteen" to CurrentDigitInWords
        when 6 move "sixteen" to CurrentDigitInWords
        when 7 move "seventeen" to CurrentDigitInWords
        when 8 move "eighteen" to CurrentDigitInWords
        when 9 move "nineteen" to CurrentDigitInWords
    end-evaluate
    exit section
    .

Tens section.
    evaluate CurrentDigit
        when 2 move "twenty" to CurrentDigitInWords
        when 3 move "thirty" to CurrentDigitInWords
        when 4 move "forty" to CurrentDigitInWords
        when 5 move "fifty" to CurrentDigitInWords
        when 6 move "sixty" to CurrentDigitInWords
        when 7 move "seventy" to CurrentDigitInWords
        when 8 move "eighty" to CurrentDigitInWords
        when 9 move "ninety" to CurrentDigitInWords
    end-evaluate
    exit section
    .

end function ThreeDigitNumberInWords.
