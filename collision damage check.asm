;; works    - tested basic function 27 November 2023
  ; damage calculation

define player_ch $0     ; player current health
define player_oh $1     ; player original health, used to calculate damage against enemy ship
define player_alive $2  ; player alive = 1, player dead = 0
define enemy_h $10      ; enemy health
define enemy_alive $12  ; enemy alive = 1, enemy dead = 0

; variables for gameplay
lda #$8
sta player_ch ; player current health
lda #$8
sta enemy_h  ; enemy health
lda #$1
sta player_alive ; set player alive
sta enemy_alive  ; set enemy alive


brk
; collision occurred
; play sound effect


lda player_ch
cmp enemy_h
beq mutualdestruction  ; branch to mutualdestruction if health of both were the same


damagecheck:
sec             ; set carry before subtraction
lda player_ch   ; load player health into A
sta player_oh   ; store it in original health
sbc enemy_h     ; subtract enemy_h from player_h
sta player_ch   ; store new player health
bmi playerdead  ; if enemy had higher health, negative flag is set, branch to "dead"
jmp end         ; jmp to end if enemy destroyed


playerdead:
sec             ; set carry before subtraction
lda enemy_h     ; load enemy health
sbc player_oh   ; subtract player original health
sta enemy_h	; store new enemy health

lda #$0
sta player_ch     ; set player health to 0   
sta player_alive  ; set player alive to 0

; check lives, jump to restart screen etc
; subtract life when retrying, or do it automatically
; when lifes equal zero, load GameOver gamestate



brk

end:
; unspawn enemy, play sound effect, animate destruction of enemy vessel
; set respawn timer for enemy, if applicable
lda #$0
sta enemy_h     ; set enemy health to 0 (it can no longer damage the player)
sta enemy_alive ; set enemy alive to 0
brk

mutualdestruction:
; unspawn enemy, play sound effect, animate destruction of enemy vessel
; set respawn timer for enemy, if applicable
; check lives, jump to restart screen etc
; subtract life when retrying, or do it automatically
; when lifes equal zero, load GameOver gamestate
lda #$0
sta player_ch     ; set player health to 0 (it can no longer damage other objects)
sta player_alive  ; set player alive to 0
lda #$0
sta enemy_h       ; set enemy health to 0 (it can no longer damage the player)
sta enemy_alive   ; set enemy alive to 0


brk


