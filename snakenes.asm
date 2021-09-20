.segment "HEADER"
    .byte "NES"
    .byte $1a
    .byte $02
    .byte $01
    .byte %00000001
    .byte $00
    .byte $00
    .byte $00
    .byte $00
    .byte $00, $00, $00, $00, $00


.segment "ZEROPAGE"
;; Variables

pointer:         .res 2   ; Address pointer
gamestate:       .res 1   ; .res 1 means reserve one byte of space
buttons1:        .res 1   ; player 1 gamepad buttons, one bit per button
buttons2:        .res 1   ; player 2 gamepad buttons, one bit per button
score:           .res 1   ; Current score


;; Declare some constants here

PPU_CTRL        = $2000 ; NMI enable (V), PPU master/slave (P), sprite height (H), background tile select (B), sprite tile select (S), increment mode (I), nametable select (NN)
PPU_MASK        = $2001 ; color emphasis (BGR), sprite enable (s), background enable (b), sprite left column enable (M), background left column enable (m), greyscale (G)
PPU_STATUS      = $2002 ; vblank (V), sprite 0 hit (S), sprite overflow (O); read resets write pair for $2005/$2006 
OAM_ADDR        = $2003 ; OAM read/write address 
OAM_DATA        = $2004 ; OAM data read/write 
PPU_SCROLL      = $2005 ; fine scroll position (two writes: X scroll, Y scroll) 
PPU_ADDR        = $2006 ; PPU read/write address (two writes: most significant byte, least significant byte) 
PPU_DATA        = $2007 ; PPU data read/write 
OAM_DMA         = $4014 ; OAM DMA high address

PATTERN_TABLE_0     = $0000
PATTERN_TABLE_1     = $1000
NAMETABLE_0         = $2000
NAMETABLE_1         = $2400
NAMETABLE_2         = $2800
NAMETABLE_3         = $2C00
PALETTE_RAM         = $3F00

STATETITLE      = $00   ; displaying title screen
STATEPLAYING    = $01   ; move paddles/ball, check for collisions
STATEGAMEOVER   = $02   ; displaying game over screen

RIGHTWALL       = $F4   ; when ball reaches one of these, do something
TOPWALL         = $20
BOTTOMWALL      = $E0
LEFTWALL        = $04

OAM_COPY        = $0200

;;;;;;;;;;;;;;;;;;;;

.segment "STARTUP"
.segment "CODE"



RESET:
    SEI             ; disable IRQs
    CLD             ; disable decimal mode
    LDX #$40
    STX $4017       ; disable APU frame IRQ
    LDX #$FF
    TXS             ; Set up stack
    INX             ; now X = 0
    STX PPU_CTRL    ; disable NMI
    STX PPU_MASK    ; disable rendering
    STX $4010       ; disable DMC IRQs

    JSR vblankwait
    TXA
clearmem:
    LDA #$00
    STA $0000, x
    STA $0100, x
    STA $0300, x
    STA $0400, x
    STA $0500, x
    STA $0600, x
    STA $0700, x
    LDA #$FE
    STA $0200, x
    LDA #$00
    INX
    BNE clearmem

    JSR vblankwait


loadpalette:
    LDA PPU_STATUS          ; Read PPU status to reset the high/low latch
    LDA #>PALETTE_RAM
    STA PPU_ADDR            ; Write the high byte of $3F00 address
    LDA #<PALETTE_RAM
    STA PPU_ADDR            ; Write the low byte of $3F00 address
    
    LDX #$00
loadpaletteloop:
    LDA palette, x          ; Load data from address (PaletteData + the value in x)
    STA PPU_DATA            ; Write palette data to PPU
    INX                     ; Increment x
    CPX #32                ; Compare X to decimal 32
    BNE loadpaletteloop    ; Branch to loadpaletteloop if compare was not equal

load_sprites:
    LDA PPU_STATUS
    LDX #00
load_sprite_loop:
    LDA sprites, x
    sta OAM_COPY, x
    inx
    cpx #64
    bne load_sprite_loop
    
loadnametable_0:
    LDA PPU_STATUS
    LDA #>NAMETABLE_0
    STA PPU_ADDR
    LDA #<NAMETABLE_0
    STA PPU_ADDR
    LDA #<title_nametable
    STA pointer + 0
    LDA #>title_nametable
    STA pointer + 1
    jsr load_nametable

loadnametable_1:
    LDA PPU_STATUS
    LDA #>NAMETABLE_1
    STA PPU_ADDR
    LDA #<NAMETABLE_1
    STA PPU_ADDR
    LDA #<playfield_nametable
    STA pointer + 0
    LDA #>playfield_nametable
    STA pointer + 1
    jsr load_nametable
    

LoadAttribute:
  LDA PPU_STATUS             ; read PPU status to reset the high/low latch
  LDA #$23
  STA PPU_ADDR            ; write the high byte of $23C0 address
  LDA #$C0
  STA PPU_ADDR             ; write the low byte of $23C0 address
  LDX #$00              ; start out at 0
LoadAttributeLoop:
  LDA playfield_nametable_attribute, x      ; load data from address (attribute + the value in x)
  STA PPU_DATA             ; write to PPU
  INX                   ; X = X + 1
  CPX #$08              ; Compare X to hex $08, decimal 8 - copying 8 bytes
  BNE LoadAttributeLoop







;;; Set starting game state
    LDA #STATEPLAYING
    STA gamestate

    CLI

    LDA #%10000000      ; enable NMI, sprites from pattern table 0, background from pattern table 0
    STA PPU_CTRL

    LDA #%00011110      ; enable sprites, enable background, noclipping on left side
    STA PPU_MASK
    LDA #$00            ; tell the PPU there is no background scrolling
    STA PPU_SCROLL
    STA PPU_SCROLL

forever:
    JMP forever ; jump back to forever, infinint loop


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VBLANK loop - called every frame ;;;

VBLANK:
    LDA #<OAM_COPY
    STA OAM_ADDR           ; Set the low byte (00) of the RAM address
    LDA #>OAM_COPY
    STA OAM_DMA           ; Set the high byte (02) of the RAM address, start the transfer

    ;; This is the PPU clean up section, so rendering the next frame starts properly
    LDA #%10000000      ; enable NMI, sprites from pattern table 0, background from pattern table 0
    STA PPU_CTRL
    LDA #%00011110      ; enable sprites, enable background, no clipping on left side
    STA PPU_MASK
    LDA #$00            ; tell the PPU there is no background scrolling
    STA PPU_SCROLL
    STA PPU_SCROLL

    ;; all graphics updates done by here, run game engine

    
    JSR ReadController1     ;; get the current button data for player 1
    JSR ReadController2     ;; get the current button data for player 2

    RTI

vblankwait:
    BIT PPU_STATUS
    BPL vblankwait
    RTS


ReadController1:
    LDA #$01
    STA $4016
    LDA #$00
    STA $4016
    LDX #$08
ReadController1Loop:
    LDA $4016
    LSR A           ; bit 0 -> Carry
    ROL buttons1    ; bit 0 <- Carry
    DEX
    BNE ReadController1Loop
    RTS

ReadController2:
    LDA #$01
    STA $4016
    LDA #$00
    STA $4016
    LDX #$08
ReadController2Loop:
    LDA $4017
    LSR A           ; bit 0 -> Carry
    ROL buttons2    ; bit 0 <- Carry
    DEX
    BNE ReadController2Loop
    RTS

load_nametable:
    LDX #04
    LDY #0
LoadBackgroundLoop:
    LDA (pointer), y     ; load data from address (background + the value in x)
    STA PPU_DATA            ; write to PPU
    INY                   ; X = X + 1
    BNE LoadBackgroundLoop
    DEX
    BEQ EndLoadBackgroundLoop
    INC pointer + 1              ; Compare X to hex $80, decimal 128 - copying 128 bytes
    JMP LoadBackgroundLoop
EndLoadBackgroundLoop:
    rts


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sprite / palette / nametable / attributes ;;;


palette:
    .byte $1a,$01,$21,$31   ; Background Palette 0
    .byte $1a,$27,$20,$0f   ; Background Palette 1
    .byte $1a,$15,$25,$16   ; Background Palette 2
    .byte $1a,$09,$19,$29   ; Background Palette 3
    .byte $1a,$01,$21,$31   ; Sprite  Palette 0
    .byte $1a,$27,$20,$0f   ; Sprite  Palette 1
    .byte $1a,$15,$25,$16   ; Sprite  Palette 2
    .byte $1a,$09,$19,$29   ; Sprite  Palette 3

sprites:
        ;vert tile attr horiz
    .byte $70, $00, $01, $78   ; Head top left
    .byte $70, $01, $01, $80   ; Head top right
    .byte $78, $10, $01, $78   ; Head bottom left
    .byte $78, $11, $01, $80   ; Head bottom right
    .byte $80, $08, $01, $78   ; Body Top Left
    .byte $80, $09, $01, $80   ; Body Top right
    .byte $88, $18, $01, $78   ; Body bottom left
    .byte $88, $19, $01, $80   ; Body bottom right
    .byte $90, $0C, $01, $78   ; tail top left
    .byte $90, $0D, $01, $80   ; tail top right right
    .byte $98, $1C, $01, $78   ; tail bottom left
    .byte $98, $1D, $01, $80   ; tail bottom right
    .byte $40, $24, $02, $38   ; apple top left
    .byte $40, $25, $02, $40   ; apple top right right
    .byte $48, $34, $02, $38   ; apple bottom left
    .byte $48, $35, $02, $40   ; apple bottom right
    

.include "playfield_nametable.asm"
.include "title_nametable.asm"

.segment "VECTORS"
    .word VBLANK
    .word RESET
    .word 0

.segment "CHARS"
    .incbin "snake.chr"     ; includes 8KB graphics file from SMB1