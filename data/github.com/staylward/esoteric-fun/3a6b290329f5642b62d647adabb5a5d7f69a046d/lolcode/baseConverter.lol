HAI 1.2
  CAN HAS STDIO?

  HOW IZ I convertBaseFunction YR value AN YR base
    I HAS A finishedFunction ITZ FAIL
    I HAS A result ITZ ""
    I HAS A nextNumber

    IM IN YR mathLoop UPPIN YR i TIL BOTH SAEM finishedFunction AN WIN
      BOTH SAEM value AN 1
      O RLY?, YA RLY, finishedFunction R WIN, OIC
      nextNumber R MOD OF value AN base
      result R SMOOSH nextNumber AN result MKAY
      value R QUOSHUNT OF value AN base
      BOTH SAEM value AN 0
      O RLY?, YA RLY, finishedFunction R WIN, OIC
    IM OUTTA YR mathLoop
    FOUND YR result
  IF U SAY SO

  HOW IZ I getInputNumberInBase10 YR inputDigit AN YR base
    I HAS A finishedFunction ITZ FAIL
    I HAS A result ITZ inputDigit
    I HAS A nextNumber

    IM IN YR newDigitLoop UPPIN YR i TIL BOTH SAEM finishedFunction AN WIN
      VISIBLE "GIMMEH YA NXT DIJIT!!!1!"
      VISIBLE "ORE IF YA DONZO TYPO 'DON'"
      GIMMEH nextNumber
      BOTH SAEM nextNumber AN "DON"
      O RLY?
        YA RLY
          finishedFunction R WIN
        NO WAI
          nextNumber IS NOW A NUMBR
          BOTH SAEM nextNumber AN BIGGR OF nextNumber AN base
          O RLY?
            YA RLY
              VISIBLE "YR A NOOB!"
              VISIBLE "THAT NUMBR IS WAY BIG FOR YA BASE"
              VISIBLE "IM OUT!"
              finishedFunction R WIN
            NO WAI
              result R PRODUKT OF result AN base
              result R SUM OF result AN nextNumber
          OIC
      OIC
    IM OUTTA YR newDigitLoop
    FOUND YR result
  IF U SAY SO

  VISIBLE "GIMMEH YA INPOOT BUTTERY BISCUIT BASE!!!1!"
  I HAS A inputBase
  GIMMEH inputBase
  inputBase IS NOW A NUMBR

  I HAS A firstDigit
  VISIBLE "GIMMEH YA FURST DIJIT!!!1!"
  GIMMEH firstDigit
  firstDigit IS NOW A NUMBR

  I HAS A inputNumberInBase10
  
  BOTH SAEM firstDigit AN BIGGR OF firstDigit AN inputBase
  O RLY?
    YA RLY
      VISIBLE "YR A NOOB! THAT NUMBR IS WAY BIG FOR YA BASE"
      inputNumberInBase10 R DIFF OF inputBase AN 1
      VISIBLE "I CHOOZY UR NUMBR ITZ NOW " inputNumberInBase10
    NO WAI
      inputNumberInBase10 R I IZ getInputNumberInBase10 YR firstDigit AN YR inputBase MKAY
  OIC
  
  VISIBLE "GIMMEH YA OUTPOOT BUTTERY BISCUIT BASE!!!1!"
  I HAS A outputBase
  GIMMEH outputBase
  outputBase IS NOW A NUMBR

  I HAS A output
  output R I IZ convertBaseFunction YR inputNumberInBase10 AN YR outputBase MKAY

  VISIBLE "YR OWTPOOT IZ: " output " IN BASE " outputBase
KTHXBYE