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

pointer:            .res 2  ; Address pointer
gamestate:          .res 1  ; .res 1 means reserve one byte of space
buttons1:           .res 1  ; player 1 gamepad buttons, one bit per button
buttons2:           .res 1  ; player 2 gamepad buttons, one bit per button
score:              .res 1  ; Current score
snake_head_x:       .res 1  ; Snake head X location
snake_head_y:       .res 1  ; Snake head Y location
snake_head_facing:  .res 1  ; Snake head facing 0-4 NSEW
snake_body_x:       .res 1  ; Snake head X location
snake_body_y:       .res 1  ; Snake head Y location
snake_body_facing:  .res 1  ; Snake head facing 0-4 NSEW
snake_tail_x:       .res 1  ; Snake head X location
snake_tail_y:       .res 1  ; Snake head Y location
snake_tail_facing:  .res 1  ; Snake head facing 0-4 NSEW
apple_x:            .res 1  ; Apple X location
apple_y:            .res 1  ; Apple Y location

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

reset:
    sei             ; disable IRQs
    cld             ; disable decimal mode
    ldx #$40
    stx $4017       ; disable APU frame IRQ
    ldx #$FF
    txs             ; Set up stack
    inx             ; now X = 0
    stx PPU_CTRL    ; disable NMI
    stx PPU_MASK    ; disable rendering
    stx $4010       ; disable DMC IRQs

    jsr vblankwait
    txa

clearmem:
    lda #$00
    sta $0000, x
    sta $0100, x
    sta $0300, x
    sta $0400, x
    sta $0500, x
    sta $0600, x
    sta $0700, x
    lda #$FE
    sta $0200, x
    lda #$00
    inx
    bne clearmem

    jsr vblankwait

load_palette:
    lda PPU_STATUS          ; Read PPU status to reset the high/low latch
    lda #>PALETTE_RAM
    sta PPU_ADDR            ; Write the high byte of $3F00 address
    lda #<PALETTE_RAM
    sta PPU_ADDR            ; Write the low byte of $3F00 address
    
    ldx #$00
load_palette_loop:
    lda palette, x          ; Load data from address (PaletteData + the value in x)
    sta PPU_DATA            ; Write palette data to PPU
    inx                     ; Increment x
    cpx #32                ; Compare X to decimal 32
    bne load_palette_loop    ; Branch to load_palette_loop if compare was not equal

load_sprites:
    lda PPU_STATUS
    ldx #00
load_sprite_loop:
    lda sprites, x
    sta OAM_COPY, x
    inx
    cpx #64
    bne load_sprite_loop
    
load_nametable_0:
    lda PPU_STATUS
    lda #>NAMETABLE_0
    sta PPU_ADDR
    lda #<NAMETABLE_0
    sta PPU_ADDR
    lda #<title_nametable
    sta pointer + 0
    lda #>title_nametable
    sta pointer + 1
    jsr load_nametable

load_nametable_1:
    lda PPU_STATUS
    lda #>NAMETABLE_1
    sta PPU_ADDR
    lda #<NAMETABLE_1
    sta PPU_ADDR
    lda #<playfield_nametable
    sta pointer + 0
    lda #>playfield_nametable
    sta pointer + 1
    jsr load_nametable
    

load_nametable_0_attributes:
    lda PPU_STATUS             ; read PPU status to reset the high/low latch
    lda #$23
    sta PPU_ADDR            ; write the high byte of $23C0 address
    lda #$C0
    sta PPU_ADDR             ; write the low byte of $23C0 address
    ldx #$00              ; start out at 0
load_nametable_0_attribute_loop:
    lda playfield_nametable_attribute, x      ; load data from address (attribute + the value in x)
    sta PPU_DATA             ; write to PPU
    inx                   ; X = X + 1
    cpx #$08              ; Compare X to hex $08, decimal 8 - copying 8 bytes
    bne load_nametable_0_attribute_loop

load_nametable_1_attributes:
    lda PPU_STATUS             ; read PPU status to reset the high/low latch
    lda #$23
    sta PPU_ADDR            ; write the high byte of $23C0 address
    lda #$C0
    sta PPU_ADDR             ; write the low byte of $23C0 address
    ldx #$00              ; start out at 0
load_nametable_1_attribute_loop:
    lda playfield_nametable_attribute, x      ; load data from address (attribute + the value in x)
    sta PPU_DATA             ; write to PPU
    inx                   ; X = X + 1
    cpx #$08              ; Compare X to hex $08, decimal 8 - copying 8 bytes
    bne load_nametable_1_attribute_loop

;;; Set starting game state
setup:
    lda #STATETITLE
    sta gamestate

    cli

    lda #%10000000      ; enable NMI, sprites from pattern table 0, background from pattern table 0
    sta PPU_CTRL

    lda #%00011110      ; enable sprites, enable background, noclipping on left side
    sta PPU_MASK
    lda #$00            ; tell the PPU there is no background scrolling
    sta PPU_SCROLL
    sta PPU_SCROLL

    jsr titlescreen_setup

forever:
    jmp forever ; jump back to forever, infinint loop


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nmi loop - called every frame ;;;

nmi:
    lda #<OAM_COPY
    sta OAM_ADDR           ; Set the low byte (00) of the RAM address
    lda #>OAM_COPY
    sta OAM_DMA           ; Set the high byte (02) of the RAM address, start the transfer

    ;; This is the PPU clean up section, so rendering the next frame starts properly
    lda #%10000000      ; enable NMI, sprites from pattern table 0, background from pattern table 0
    sta PPU_CTRL
    lda #%00011110      ; enable sprites, enable background, no clipping on left side
    sta PPU_MASK
    lda #$00            ; tell the PPU there is no background scrolling
    sta PPU_SCROLL
    sta PPU_SCROLL

    ;; all graphics updates done by here, run game engine

    
    jsr read_controller_1     ;; get the current button data for player 1
    jsr read_controller_2     ;; get the current button data for player 2

    lda gamestate
    cmp #STATETITLE
    jsr titlescreen

    rti

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subroutines ;;;

vblankwait:
    bit PPU_STATUS
    bpl vblankwait
    rts


read_controller_1:
    lda #$01
    sta $4016
    lda #$00
    sta $4016
    ldx #$08
read_controller_1_loop:
    lda $4016
    lsr A           ; bit 0 -> Carry
    rol buttons1    ; bit 0 <- Carry
    dex
    bne read_controller_1_loop
    rts

read_controller_2:
    lda #$01
    sta $4016
    lda #$00
    sta $4016
    ldx #$08
read_controller_2_loop:
    lda $4017
    lsr A           ; bit 0 -> Carry
    rol buttons2    ; bit 0 <- Carry
    dex
    bne read_controller_2_loop
    rts

load_nametable:
    ldx #04
    ldy #0
load_nametable_loop:
    lda (pointer), y     ; load data from address (background + the value in x)
    sta PPU_DATA            ; write to PPU
    INY                   ; X = X + 1
    bne load_nametable_loop
    dex
    BEQ end_load_nametable_loop
    INC pointer + 1              ; Compare X to hex $80, decimal 128 - copying 128 bytes
    jmp load_nametable_loop
end_load_nametable_loop:
    rts

titlescreen_setup:
    ; Setup background
    jsr vblankwait
    lda #$00
    sta PPU_CTRL    ; disable NMI
    sta PPU_MASK    ; disable rendering
    sta $4010       ; disable DMC IRQs
    lda PPU_STATUS
    lda #>PALETTE_RAM
    sta PPU_ADDR            ; Write the high byte of $3F00 address
    lda #<PALETTE_RAM
    sta PPU_ADDR
    lda #$0F
    sta PPU_DATA
    lda #%10000000      ; enable NMI, sprites from pattern table 0, background from pattern table 0
    sta PPU_CTRL
    lda #%00011110

    ; Set snake head location
    lda #$01
    sta snake_head_facing
    sta snake_body_facing
    sta snake_tail_facing
    lda #$80
    sta snake_head_y
    sta snake_body_y
    sta snake_tail_y
    lda #$00
    sta snake_head_x
    lda #$40
    sta apple_x
    sta apple_y

    rts

titlescreen:
    jsr update_sprites
    jsr draw_sprites
    rts

update_sprites:
    jsr update_head
    jsr update_body
    jsr update_tail
    jsr update_apple

draw_sprites:
    jsr draw_snake_head
    jsr draw_snake_body
    jsr draw_snake_tail
    jsr draw_apple
    rts

update_head:
    lda snake_head_x
    adc 1
    sta snake_head_x
    rts

update_body:
    lda snake_head_x
    clc
    sbc #$9
    sta snake_body_x
    rts

update_tail:
    lda snake_head_x
    clc
    sbc #$19
    sta snake_tail_x
    rts

update_apple:
    rts



draw_snake_head:
    lda #<snake_head_sprite
    sta pointer
    lda #>snake_head_sprite
    sta pointer + 1
    ldx snake_head_facing
    jsr get_sprite_facing_offset
    jsr set_4_tile_sprite_tiles
    lda snake_head_y
    ldx snake_head_x
    jsr set_sprite_x_y
    rts

draw_snake_body:
    lda #<snake_body_sprite
    sta pointer
    lda #>snake_body_sprite
    sta pointer + 1
    ldx snake_body_facing
    jsr get_sprite_facing_offset
    adc #$10
    jsr set_4_tile_sprite_tiles
    lda snake_body_y
    ldx snake_body_x
    jsr set_sprite_x_y
    rts

draw_snake_tail:
    lda #<snake_tail_sprite
    sta pointer
    lda #>snake_tail_sprite
    sta pointer + 1
    ldx snake_tail_facing
    jsr get_sprite_facing_offset
    adc #$20
    jsr set_4_tile_sprite_tiles
    lda snake_tail_y
    ldx snake_tail_x
    jsr set_sprite_x_y
    rts

draw_apple:
    ; Set tiles
    lda #<apple_sprite
    sta pointer
    lda #>apple_sprite
    sta pointer + 1
    lda #$30
    jsr set_4_tile_sprite_tiles
    lda apple_y
    ldx apple_x
    jsr set_sprite_x_y
    rts

get_sprite_facing_offset:
    ; Get the sprite tile offset to rotate the sprite
    ; Sprite facing must be in X
    ; Will set A to the offset
    lda #$FC
    clc
    sprite_facing_offset_loop:
    adc #4
    clc
    dex
    cpx #$FF
    bne sprite_facing_offset_loop
    clc
    rts

set_4_tile_sprite_tiles:
    ; Set the tiles for a m4 tile sprite.
    ; Tiles must be contiguous in CHR ROM ordered TL, TR, BL, BR
    ; Set A to the first tile, put target address in pointer
    clc
    ldy #1
    sta (pointer),Y
    adc #1
    ldy #5
    sta (pointer),Y
    adc #1
    ldy #9
    sta (pointer),Y
    adc #1
    ldy #13
    sta (pointer),Y
    adc #1
    rts

set_sprite_x_y:
    ; Sets the X and Y coords for a sprite
    ; Sprite X location in OAM COPY must be in pointer
    ; New Y must be in A register, New X must be in X register
    clc
    ldy #0
    sta (pointer),Y
    ldy #4
    sta (pointer),Y
    adc #8
    ldy #8
    sta (pointer),Y
    ldy #12
    sta (pointer),Y
    txa
    ldy #3
    sta (pointer),Y
    ldy #11
    sta (pointer),Y
    adc #8
    ldy #7
    sta (pointer),Y
    ldy #15
    sta (pointer),Y
    rts


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sprite / palette / nametable / attributes ;;;


palette:
    .byte $0f,$01,$21,$31   ; Background Palette 0
    .byte $0f,$27,$20,$0f   ; Background Palette 1
    .byte $0f,$15,$25,$16   ; Background Palette 2
    .byte $0f,$3d,$29,$29   ; Background Palette 3
    .byte $1a,$01,$21,$31   ; Sprite  Palette 0
    .byte $1a,$27,$20,$0f   ; Sprite  Palette 1
    .byte $1a,$15,$25,$16   ; Sprite  Palette 2
    .byte $1a,$09,$19,$29   ; Sprite  Palette 3

sprites:
            ;vert   tile    attr    horiz
    .byte   $70,    $00,    $01,    $78   ; Head top left
    .byte   $70,    $01,    $01,    $80   ; Head top right
    .byte   $78,    $10,    $01,    $78   ; Head bottom left
    .byte   $78,    $11,    $01,    $80   ; Head bottom right
    .byte   $80,    $08,    $01,    $78   ; Body Top Left
    .byte   $80,    $09,    $01,    $80   ; Body Top right
    .byte   $88,    $18,    $01,    $78   ; Body bottom left
    .byte   $88,    $19,    $01,    $80   ; Body bottom right
    .byte   $90,    $0C,    $01,    $78   ; tail top left
    .byte   $90,    $0D,    $01,    $80   ; tail top right right
    .byte   $98,    $1C,    $01,    $78   ; tail bottom left
    .byte   $98,    $1D,    $01,    $80   ; tail bottom right
    .byte   $40,    $24,    $02,    $38   ; apple top left
    .byte   $40,    $25,    $02,    $40   ; apple top right right
    .byte   $48,    $34,    $02,    $38   ; apple bottom left
    .byte   $48,    $35,    $02,    $40   ; apple bottom right
    

.include "playfield_nametable.asm"
.include "title_nametable.asm"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OAM_COPY setup

.org OAM_COPY
snake_head_sprite:      .res 16
snake_body_sprite:      .res 16
snake_tail_sprite:      .res 16
apple_sprite:           .res 16

.segment "VECTORS"
    .word nmi
    .word reset
    .word 0

.segment "CHARS"
    .incbin "snake.chr"     ; includes 8KB graphics file from SMB1