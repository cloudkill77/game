; Dn-FamiTracker exported music data: melody5.dnm
;

; Module header
	.word ft_song_list
	.word ft_instrument_list
	.word ft_sample_list
	.word ft_samples
	.word ft_groove_list
	.byte 0 ; flags
	.word 3600 ; NTSC speed
	.word 3000 ; PAL speed

; Instrument pointer list
ft_instrument_list:
	.word ft_inst_0
	.word ft_inst_1
	.word ft_inst_2
	.word ft_inst_3

; Instruments
ft_inst_0:
	.byte 0
	.byte $11
	.word ft_seq_2a03_10
	.word ft_seq_2a03_14

ft_inst_1:
	.byte 0
	.byte $11
	.word ft_seq_2a03_0
	.word ft_seq_2a03_4

ft_inst_2:
	.byte 0
	.byte $01
	.word ft_seq_2a03_5

ft_inst_3:
	.byte 0
	.byte $05
	.word ft_seq_2a03_15
	.word ft_seq_2a03_2

; Sequences
ft_seq_2a03_0:
	.byte $40, $FF, $00, $00, $0D, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0B, $0B, $0B, $0B, $0B, $0B, $0B
	.byte $0B, $0B, $0B, $0B, $0B, $0B, $0A, $0A, $0A, $0A, $0A, $0A, $09, $09, $09, $09, $09, $08, $08, $08
	.byte $08, $08, $08, $07, $07, $07, $07, $06, $06, $06, $05, $05, $05, $04, $04, $03, $03, $02, $01, $01
	.byte $01, $00, $00, $00, $00, $00, $00, $00
ft_seq_2a03_2:
	.byte $08, $FF, $00, $00, $03, $01, $0E, $24, $2E, $FF, $F3, $DC
ft_seq_2a03_4:
	.byte $02, $FF, $00, $00, $02, $02
ft_seq_2a03_5:
	.byte $06, $FF, $00, $00, $0C, $0B, $09, $04, $00, $00
ft_seq_2a03_10:
	.byte $20, $FF, $00, $00, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0B, $0B
	.byte $0B, $0B, $0B, $0A, $0A, $0A, $09, $09, $08, $07, $05, $05, $03, $02, $01, $00
ft_seq_2a03_14:
	.byte $02, $FF, $00, $00, $02, $02
ft_seq_2a03_15:
	.byte $08, $FF, $00, $00, $0E, $0E, $0D, $0C, $0A, $07, $01, $00

; DPCM instrument list (pitch, sample index)
ft_sample_list:

; DPCM samples list (location, size, bank)
ft_samples:

; Groove list
ft_groove_list:
	.byte $00
; Grooves (size, terms)

; Song pointer list
ft_song_list:
	.word ft_song_0
	.word ft_song_1
	.word ft_song_2
	.word ft_song_3
	.word ft_song_4
	.word ft_song_5
	.word ft_song_6
	.word ft_song_7

; Song info
ft_song_0:
	.word ft_s0_frames
	.byte 1	; frame count
	.byte 64	; pattern length
	.byte 6	; speed
	.byte 150	; tempo
	.byte 0	; groove position
	.byte 0	; initial bank

ft_song_1:
	.word ft_s1_frames
	.byte 1	; frame count
	.byte 64	; pattern length
	.byte 6	; speed
	.byte 150	; tempo
	.byte 0	; groove position
	.byte 0	; initial bank

ft_song_2:
	.word ft_s2_frames
	.byte 1	; frame count
	.byte 64	; pattern length
	.byte 6	; speed
	.byte 150	; tempo
	.byte 0	; groove position
	.byte 0	; initial bank

ft_song_3:
	.word ft_s3_frames
	.byte 1	; frame count
	.byte 64	; pattern length
	.byte 6	; speed
	.byte 150	; tempo
	.byte 0	; groove position
	.byte 0	; initial bank

ft_song_4:
	.word ft_s4_frames
	.byte 1	; frame count
	.byte 64	; pattern length
	.byte 6	; speed
	.byte 150	; tempo
	.byte 0	; groove position
	.byte 0	; initial bank

ft_song_5:
	.word ft_s5_frames
	.byte 1	; frame count
	.byte 64	; pattern length
	.byte 6	; speed
	.byte 150	; tempo
	.byte 0	; groove position
	.byte 0	; initial bank

ft_song_6:
	.word ft_s6_frames
	.byte 1	; frame count
	.byte 64	; pattern length
	.byte 6	; speed
	.byte 150	; tempo
	.byte 0	; groove position
	.byte 0	; initial bank

ft_song_7:
	.word ft_s7_frames
	.byte 1	; frame count
	.byte 64	; pattern length
	.byte 6	; speed
	.byte 150	; tempo
	.byte 0	; groove position
	.byte 0	; initial bank


;
; Pattern and frame data for all songs below
;

; Bank 0
ft_s0_frames:
	.word ft_s0f0
ft_s0f0:
	.word ft_s0p0c0, ft_s0p0c1, ft_s0p0c1, ft_s0p0c3, ft_s0p0c1
; Bank 0
ft_s0p0c0:
	.byte $82, $01, $E1, $25, $29, $25, $2A, $25, $2C, $25, $2A, $25, $2C, $25, $30, $25, $33, $25, $2C, $25
	.byte $2E, $25, $29, $25, $2A, $25, $2C, $29, $2E, $29, $2C, $25, $2A, $25, $83, $2E, $01

; Bank 0
ft_s0p0c1:
	.byte $00, $3F

; Bank 0
ft_s0p0c3:
	.byte $E2, $18, $03, $18, $03, $18, $03, $82, $01, $18, $1C, $1A, $19, $17, $15, $1C, $18, $18, $18, $18
	.byte $18, $18, $18, $82, $00, $18, $18, $18, $18, $83, $18, $01, $18, $00, $18, $00, $18, $00, $18, $01
	.byte $18, $00, $18, $00, $18, $01, $18, $00, $18, $00, $18, $01, $18, $00, $18, $00, $18, $02

; Bank 0
ft_s1_frames:
	.word ft_s1f0
ft_s1f0:
	.word ft_s0p0c1, ft_s1p0c1, ft_s0p0c1, ft_s0p0c1, ft_s0p0c1
; Bank 0
ft_s1p0c1:
	.byte $82, $00, $E3, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F
	.byte $83, $3F, $08, $82, $00, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F
	.byte $3F, $3F, $83, $3F, $14

; Bank 0
ft_s2_frames:
	.word ft_s2f0
ft_s2f0:
	.word ft_s0p0c1, ft_s0p0c1, ft_s0p0c1, ft_s2p0c3, ft_s0p0c1
; Bank 0
ft_s2p0c3:
	.byte $E1, $15, $00, $18, $00, $18, $3D

; Bank 0
ft_s3_frames:
	.word ft_s3f0
ft_s3f0:
	.word ft_s0p0c1, ft_s0p0c1, ft_s0p0c1, ft_s3p0c3, ft_s0p0c1
; Bank 0
ft_s3p0c3:
	.byte $00, $00, $E1, $17, $00, $18, $00, $18, $3C

; Bank 0
ft_s4_frames:
	.word ft_s4f0
ft_s4f0:
	.word ft_s0p0c1, ft_s0p0c1, ft_s0p0c1, ft_s4p0c3, ft_s0p0c1
; Bank 0
ft_s4p0c3:
	.byte $00, $00, $E1, $15, $3E

; Bank 0
ft_s5_frames:
	.word ft_s5f0
ft_s5f0:
	.word ft_s0p0c1, ft_s0p0c1, ft_s0p0c1, ft_s5p0c3, ft_s0p0c1
; Bank 0
ft_s5p0c3:
	.byte $E1, $19, $3F

; Bank 0
ft_s6_frames:
	.word ft_s6f0
ft_s6f0:
	.word ft_s0p0c1, ft_s0p0c1, ft_s0p0c1, ft_s6p0c3, ft_s0p0c1
; Bank 0
ft_s6p0c3:
	.byte $E0, $14, $3F

; Bank 0
ft_s7_frames:
	.word ft_s7f0
ft_s7f0:
	.word ft_s0p0c1, ft_s0p0c1, ft_s0p0c1, ft_s7p0c3, ft_s0p0c1
; Bank 0
ft_s7p0c3:
	.byte $E0, $16, $3F


; DPCM samples (located at DPCM segment)
