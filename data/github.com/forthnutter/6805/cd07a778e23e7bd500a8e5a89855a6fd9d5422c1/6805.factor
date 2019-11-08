! Copyright (C) 2011 Joseph L Moschini.
! See http://factorcode.org/license.txt for BSD license.
!
USING: 6805.emulator tools.time ;
IN: 6805


! INSTRUCTION: BRSET 0,dd,rr        ; opcode 00 cycles 05
! INSTRUCTION: BRCLR 0,dd,rr        ; opcode 01 cycles 05
! INSTRUCTION: BRSET 1,dd,rr        ; opcode 02 cycles 05
! INSTRUCTION: BRCLR 1,dd,rr        ; opcode 03 cycles 05
! INSTRUCTION: BRSET 2,dd,rr        ; opcode 04 cycles 05
! INSTRUCTION: BRCLR 2,dd,rr        ; opcode 05 cycles 05
! INSTRUCTION: BRSET 3,dd,rr        ; opcode 06 cycles 05
! INSTRUCTION: BRCLR 3,dd,rr        ; opcode 07 cycles 05
! INSTRUCTION: BRSET 4,dd,rr        ; opcode 08 cycles 05
! INSTRUCTION: BRCLR 4,dd,rr        ; opcode 09 cycles 05
! INSTRUCTION: BRSET 5,dd,rr        ; opcode 0A cycles 05
! INSTRUCTION: BRCLR 5,dd,rr        ; opcode 0B cycles 05
! INSTRUCTION: BRSET 6,dd,rr        ; opcode 0C cycles 05
! INSTRUCTION: BRCLR 6,dd,rr        ; opcode 0D cycles 05
! INSTRUCTION: BRSET 7,dd,rr        ; opcode 0E cycles 05
! INSTRUCTION: BRCLR 7,dd,rr        ; opcode 0F cycles 05

! INSTRUCTION: BSET 0,dd            ; opcode 10 cycles 05
! INSTRUCTION: BCLR 0,dd            ; opcode 11 cycles 05
! INSTRUCTION: BSET 1,dd            ; opcode 12 cycles 05
! INSTRUCTION: BCLR 1,dd            ; opcode 13 cycles 05
! INSTRUCTION: BSET 2,dd            ; opcode 14 cycles 05
! INSTRUCTION: BCLR 2,dd            ; opcode 15 cycles 05
! INSTRUCTION: BSET 3,dd            ; opcode 16 cycles 05
! INSTRUCTION: BCLR 3,dd            ; opcode 17 cycles 05
! INSTRUCTION: BSET 4,dd            ; opcode 18 cycles 05
! INSTRUCTION: BCLR 4,dd            ; opcode 19 cycles 05
! INSTRUCTION: BSET 5,dd            ; opcode 1A cycles 05
! INSTRUCTION: BCLR 5,dd            ; opcode 1B cycles 05
! INSTRUCTION: BSET 6,dd            ; opcode 1C cycles 05
! INSTRUCTION: BCLR 6,dd            ; opcode 1D cycles 05
! INSTRUCTION: BSET 7,dd            ; opcode 1E cycles 05
! INSTRUCTION: BCLR 7,dd            ; opcode 1F cycles 05

! INSTRUCTION: BRA rr               ; opcode 20 cycles 03
! INSTRUCTION: BRN rr               ; opcode 21 cycles 03
! INSTRUCTION: BHI rr               ; opcode 22 cycles 03
! INSTRUCTION: BLS rr               ; opcode 23 cycles 03
! INSTRUCTION: BCC rr               ; opcode 24 cycles 03
! INSTRUCTION: BCS rr               ; opcode 25 cycles 03
! INSTRUCTION: BNE rr               ; opcode 26 cycles 03
! INSTRUCTION: BEQ rr               ; opcode 27 cycles 03
! INSTRUCTION: BHCC rr              ; opcode 28 cycles 03
! INSTRUCTION: BHCS rr              ; opcode 29 cycles 03
! INSTRUCTION: BPL rr               ; opcode 2A cycles 03
! INSTRUCTION: BMI rr               ; opcode 2B cycles 03
! INSTRUCTION: BMC rr               ; opcode 2C cycles 03
! INSTRUCTION: BMS rr               ; opcode 2D cycles 03
! INSTRUCTION: BIL rr               ; opcode 2E cycles 03
! INSTRUCTION: BIH rr               ; opcode 2F cycles 03

! INSTRUCTION: NEG dd               ; opcode 30 cycles 05
! INSTRUCTION: COM dd               ; opcode 33 cycles 05
! INSTRUCTION: LSR dd               ; opcode 34 cycles 05
! INSTRUCTION: ROR dd               ; opcode 36 cycles 05
! INSTRUCTION: ASR dd               ; opcode 37 cycles 05
! INSTRUCTION: LSL dd               ; opcode 38 cycles 05
! INSTRUCTION: ROL dd               ; opcode 39 cycles 05
! INSTRUCTION: DEC dd               ; opcode 3A cycles 05
! INSTRUCTION: INC dd               ; opcode 3C cycles 05
! INSTRUCTION: TST dd               ; opcode 3D cycles 04
! INSTRUCTION: CLR dd               ; opcode 3F cycles 05

! INSTRUCTION: NEGA                 ; opcode 40 cycles 03
! INSTRUCTION: COMA                 ; opcode 43 cycles 03
! INSTRUCTION: LSRA                 ; opcode 44 cycles 03
! INSTRUCTION: RORA                 ; opcode 46 cycles 03
! INSTRUCTION: ASRA                 ; opcode 47 cycles 03
! INSTRUCTION: LSLA                 ; opcode 48 cycles 03
! INSTRUCTION: ROLA                 ; opcode 49 cycles 03
! INSTRUCTION: DECA                 ; opcode 4A cycles 03
! INSTRUCTION: INCA                 ; opcode 4C cycles 03
! INSTRUCTION: TSTA                 ; opcode 4D cycles 03
! INSTRUCTION: CLRA                 ; opcode 4F cycles 03

! INSTRUCTION: NEGX                 ; opcode 50 cycles 03
! INSTRUCTION: COMX                 ; opcode 53 cycles 03
! INSTRUCTION: LSRX                 ; opcode 54 cycles 03
! INSTRUCTION: RORX                 ; opcode 56 cycles 03
! INSTRUCTION: ASRX                 ; opcode 57 cycles 03
! INSTRUCTION: LSLX                 ; opcode 58 cycles 03
! INSTRUCTION: ROLX                 ; opcode 59 cycles 03
! INSTRUCTION: DECX                 ; opcode 5A cycles 03
! INSTRUCTION: INCX                 ; opcode 5C cycles 03
! INSTRUCTION: TSTX                 ; opcode 5D cycles 03
! INSTRUCTION: CLRX                 ; opcode 5F cycles 03

! INSTRUCTION: NEG ff,X             ; opcode 60 cycles 06
! INSTRUCTION: COM ff,X             ; opcode 63 cycles 06
! INSTRUCTION: LSR ff,X             ; opcode 64 cycles 06
! INSTRUCTION: ROR ff,X             ; opcode 66 cycles 06
! INSTRUCTION: ASR ff,X             ; opcode 67 cycles 06
! INSTRUCTION: LSL ff,X             ; opcode 68 cycles 06
! INSTRUCTION: ROL ff,X             ; opcode 69 cycles 06
! INSTRUCTION: DEC ff,X             ; opcode 6A cycles 06
! INSTRUCTION: INC ff,X             ; opcode 6C cycles 06
! INSTRUCTION: TST ff,X             ; opcode 6D cycles 06
! INSTRUCTION: CLR ff,X             ; opcode 6F cycles 06

! INSTRUCTION: NEG ,X               ; opcode 70 cycles 05
! INSTRUCTION: COM ,X               ; opcode 73 cycles 05
! INSTRUCTION: LSR ,X               ; opcode 74 cycles 05
! INSTRUCTION: ROR ,X               ; opcode 76 cycles 05
! INSTRUCTION: ASR ,X               ; opcode 77 cycles 05
! INSTRUCTION: LSL ,X               ; opcode 78 cycles 05
! INSTRUCTION: ROL ,X               ; opcode 79 cycles 05
! INSTRUCTION: DEC ,X               ; opcode 7A cycles 05
! INSTRUCTION: INC ,X               ; opcode 7C cycles 05
! INSTRUCTION: TST ,X               ; opcode 7D cycles 05
! INSTRUCTION: CLR ,X               ; opcode 7F cycles 05

! INSTRUCTION: RTI                  ; opcode 80 cycles 09
! INSTRUCTION: RTS                  ; opcode 81 cycles 06
! INSTRUCTION: SWI                  ; opcode 83 cycles 10
! INSTRUCTION: STOP                 ; opcode 8E cycles 02
! INSTRUCTION: WAIT                 ; opcode 8F cycles 02

! INSTRUCTION: TAX                  ; opcode 97 cycles 02
! INSTRUCTION: CLC                  ; opcode 98 cycles 02
! INSTRUCTION: SEC                  ; opcode 99 cycles 02
! INSTRUCTION: CLI                  ; opcode 9A cycles 02
! INSTRUCTION: SEI                  ; opcode 9B cycles 02
! INSTRUCTION: RSP                  ; opcode 9C cycles 02
! INSTRUCTION: NOP                  ; opcode 9D cycles 02
! INSTRUCTION: TXA                  ; opcode 9F cycles 02

! INSTRUCTION: SUB #nn              ; opcode A0 cycles 02
! INSTRUCTION: CMP #nn              ; opcode A1 cycles 02
! INSTRUCTION: SBC #nn              ; opcode A2 cycles 02
! INSTRUCTION: CPX #nn              ; opcode A3 cycles 02
! INSTRUCTION: AND #nn              ; opcode A4 cycles 02
! INSTRUCTION: BIT #nn              ; opcode A5 cycles 02
! INSTRUCTION: LDA #nn              ; opcode A6 cycles 02
! INSTRUCTION: EOR #nn              ; opcode A8 cycles 02
! INSTRUCTION: ADC #nn              ; opcode A9 cycles 02
! INSTRUCTION: ORA #nn              ; opcode AA cycles 02
! INSTRUCTION: ADD #nn              ; opcode AB cycles 02
! INSTRUCTION: BSR rr               ; opcode AD cycles 06
! INSTRUCTION: LDX #nn              ; opcode AE cycles 02

! INSTRUCTION: SUB dd               ; opcode B0 cycles 03
! INSTRUCTION: CMP dd               ; opcode B1 cycles 03
! INSTRUCTION: SBC dd               ; opcode B2 cycles 03
! INSTRUCTION: CPX dd               ; opcode B3 cycles 03
! INSTRUCTION: AND dd               ; opcode B4 cycles 03
! INSTRUCTION: BIT dd               ; opcode B5 cycles 03
! INSTRUCTION: LDA dd               ; opcode B6 cycles 03
! INSTRUCTION: STA dd               ; opcode B7 cycles 04
! INSTRUCTION: EOR dd               ; opcode B8 cycles 03
! INSTRUCTION: ADC dd               ; opcode B9 cycles 03
! INSTRUCTION: ORA dd               ; opcode BA cycles 03
! INSTRUCTION: ADD dd               ; opcode BB cycles 03
! INSTRUCTION: JMP dd               ; opcode BC cycles 02
! INSTRUCTION: JSR dd               ; opcode BD cycles 05
! INSTRUCTION: LDX dd               ; opcode BE cycles 03
! INSTRUCTION: STX dd               ; opcode BF cycles 04

! INSTRUCTION: SUB dddd             ; opcode C0 cycles 04
! INSTRUCTION: CMP dddd             ; opcode C1 cycles 04
! INSTRUCTION: SBC dddd             ; opcode C2 cycles 04
! INSTRUCTION: CPX dddd             ; opcode C3 cycles 04
! INSTRUCTION: AND dddd             ; opcode C4 cycles 04
! INSTRUCTION: BIT dddd             ; opcode C5 cycles 04
! INSTRUCTION: LDA dddd             ; opcode C6 cycles 04
! INSTRUCTION: STA dddd             ; opcode C7 cycles 05
! INSTRUCTION: EOR dddd             ; opcode C8 cycles 04
! INSTRUCTION: ADC dddd             ; opcode C9 cycles 04
! INSTRUCTION: ORA dddd             ; opcode CA cycles 04
! INSTRUCTION: ADD dddd             ; opcode CB cycles 04
! INSTRUCTION: JMP dddd             ; opcode CC cycles 03
! INSTRUCTION: JSR dddd             ; opcode CD cycles 06
! INSTRUCTION: LDX dddd             ; opcode CE cycles 04
! INSTRUCTION: STX dddd             ; opcode CF cycles 05

INSTRUCTION: SUB dddd,X           ; opcode D0 cycles 05
INSTRUCTION: CMP dddd,X           ; opcode D1 cycles 05
INSTRUCTION: SBC dddd,X           ; opcode D2 cycles 05
INSTRUCTION: CPX dddd,X           ; opcode D3 cycles 05
INSTRUCTION: AND dddd,X           ; opcode D4 cycles 05
INSTRUCTION: BIT dddd,X           ; opcode D5 cycles 05
INSTRUCTION: LDA dddd,X           ; opcode D6 cycles 05
INSTRUCTION: STA dddd,X           ; opcode D7 cycles 06
INSTRUCTION: EOR dddd,X           ; opcode D8 cycles 05
INSTRUCTION: ADC dddd,X           ; opcode D9 cycles 05
INSTRUCTION: ORA dddd,X           ; opcode DA cycles 05
INSTRUCTION: ADD dddd,X           ; opcode DB cycles 05
INSTRUCTION: JMP dddd,X           ; opcode DC cycles 04
INSTRUCTION: JSR dddd,X           ; opcode DD cycles 07
INSTRUCTION: LDX dddd,X           ; opcode DE cycles 05
INSTRUCTION: STX dddd,X           ; opcode DF cycles 06

INSTRUCTION: SUB dd,X             ; opcode E0 cycles 05
INSTRUCTION: CMP dd,X             ; opcode E1 cycles 05
INSTRUCTION: SBC dd,X             ; opcode E2 cycles 05
INSTRUCTION: CPX dd,X             ; opcode E3 cycles 05
INSTRUCTION: AND dd,X             ; opcode E4 cycles 05
INSTRUCTION: BIT dd,X             ; opcode E5 cycles 05
INSTRUCTION: LDA dd,X             ; opcode E6 cycles 05
INSTRUCTION: STA dd,X             ; opcode E7 cycles 05
INSTRUCTION: EOR dd,X             ; opcode E8 cycles 05
INSTRUCTION: ADC dd,X             ; opcode E9 cycles 05
INSTRUCTION: ORA dd,X             ; opcode EA cycles 05
INSTRUCTION: ADD dd,X             ; opcode EB cycles 05
INSTRUCTION: JMP dd,X             ; opcode EC cycles 03
INSTRUCTION: JSR dd,X             ; opcode ED cycles 06
INSTRUCTION: LDX dd,X             ; opcode EE cycles 05
INSTRUCTION: STX dd,X             ; opcode EF cycles 05

INSTRUCTION: SUB ,X               ; opcode F0 cycles 03
INSTRUCTION: CMP ,X               ; opcode F1 cycles 03
INSTRUCTION: SBC ,X               ; opcode F2 cycles 03
INSTRUCTION: CPX ,X               ; opcode F3 cycles 03
INSTRUCTION: AND ,X               ; opcode F4 cycles 03
INSTRUCTION: BIT ,X               ; opcode F5 cycles 03
INSTRUCTION: LDA ,X               ; opcode F6 cycles 03
INSTRUCTION: STA ,X               ; opcode F7 cycles 03
INSTRUCTION: EOR ,X               ; opcode F8 cycles 03
INSTRUCTION: ADC ,X               ; opcode F9 cycles 03
INSTRUCTION: ORA ,X               ; opcode FA cycles 03
INSTRUCTION: ADD ,X               ; opcode FB cycles 03
INSTRUCTION: JMP ,X               ; opcode FC cycles 02
INSTRUCTION: JSR ,X               ; opcode FD cycles 05
INSTRUCTION: LDX ,X               ; opcode FE cycles 03
INSTRUCTION: STX ,X               ; opcode FF cycles 03
