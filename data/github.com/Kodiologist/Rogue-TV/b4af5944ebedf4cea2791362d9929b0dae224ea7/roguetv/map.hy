(require [kodhy.macros [afind-or whenn block meth lc]] [roguetv.macros [*]])

(import
  [random [randint uniform]]
  [kodhy.util [T F ret ucfirst]]
  [roguetv.english [NounPhrase NounPhraseNamed]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.input [user-confirms inventory-loop]]
  [roguetv.types [Drawable MapObject Scheduled]])

(defclass Tile [Drawable MapObject NounPhraseNamed Scheduled] [
  escape-xml-in-np-format T
  info-text "[Missing info text]"
  blocks-movement F
  blocks-sight F
  unpleasant F
    ; `unpleasant` is a flag meaning that monsters tend not to
    ; want to be on this kind of tile.
  smooth F
  spooky F
    ; Whether the tile has an aura that slows movement.
  container F
    ; Whether the tile prevents items from being added to or
    ; removed from it.
  opaque-container F
    ; Whether the player can see what item is on the tile.

  __format__ (fn [self formatstr]
    ; ":full" is accepted at the end of a format string to
    ; include the name-suffix.
    (setv full F)
    (when (.endswith formatstr ":full")
      (setv formatstr (cut formatstr 0 (- (len ":full"))))
      (setv full T))
    (.escape self (cat :sep " " (.__format__ self.name formatstr)
      (when full (.name-suffix self)))))

  information (fn [self]
    (.format "\n  {} {:a:full}\n\n{}"
      (.xml-symbol self)
      self
      (.format self.info-text #** (dict
        (+ [(, "G" information-G)] (list (.__dict__.items (type self))))))))

  name-suffix (fn [self]
    ; This method can be overridden to provide extra information
    ; about a tile, like its state. It's only displayed with the
    ; "most" formatting tag.
    None)

  use-tile (fn [self]
    ; The player has used the command :use-tile on this tile.
    ;
    ; The default implementaton does nothing.
    (msg "There's nothing special you can do at this tile."))

  use-item-while-here (fn [self]
    ; The player has tried to use (i.e., apply or drop) an item
    ; while standing on this tile. Return F to halt further
    ; processing of the action, and T to continue.
    T)

  bump-into (fn [self cr]
    ; A creature has tried to step onto this tile. Return
    ; F to halt further processing of the step (in particular,
    ; the creature is not moved), and T to continue.
    T)

  after-entering (fn [self cr p-from]
    ; A creature has just moved to this tile, by any means.
    None)

  after-step-onto (fn [self cr p-from]
    ; A creature has just finished walking onto this tile.
    ; Teleportation, for example, won't trigger this.
    None)

  step-out-of (fn [self cr p-to]
    ; A creature is stepping out of this tile.
    None)])

(defclass LevelBoundary [NounPhraseNamed] [
  ; This object is not a real tile. Rather, it is used as a flag
  ; for the lack of a tile when you use `mget` rather than `Tile.at`.
  name (NounPhrase "level boundary" :mass T :unit "sections")
  blocks-movement T
  blocks-sight T
  bump-into (fn [self cr] T)])
(setv LevelBoundary (LevelBoundary))

(defn mset [pos tile &optional [initializing-map F]]
  (unless initializing-map
    (setv old-tile (Tile.at pos))
    (when (!= tile.blocks-sight old-tile.blocks-sight)
      (soil-fov))
    (.destroy old-tile))
  (.move tile pos :clobber T))

(defn mget [pos]
  (if (on-map pos)
    (Tile.at pos)
    LevelBoundary))

(defn on-map [pos]
  (and (<= 0 pos.x (dec G.map-width)) (<= 0 pos.y (dec G.map-height))))

(defn outer-corner-pos []
  [(Pos -1 -1) (Pos G.map-width -1)
    (Pos -1 G.map-height) (Pos G.map-width G.map-height)])

(defn all-pos []
  (lc [x (range G.map-width) y (range G.map-height)] (Pos x y)))

(defn room-for? [mo-class pos]
  (and
    (on-map pos)
    (not (. (Tile.at pos) blocks-movement))
    (not (.at mo-class pos))))

(defn ray-taxi [
    start     ; Pos
    direction ; Pos
    length    ; int
    &optional
    [include-off-map F]]
  ; Produces a list of Pos. `direction` should be in Pos.DIR8.
  ; If `direction` is orthogonal, you get `length` elements.
  ; Otherwise, you get `length` // 2 elements.
  (setv l [start])
  (for [_ (range (dec (if (in direction Pos.DIAGS) (// length 2) length)))]
    (setv p (+ (get l -1) direction))
    (unless (or include-off-map (on-map p))
      (break))
    (.append l p))
  l)

(defn disc-taxi [
    center  ; Pos
    radius] ; int
  ; Produces a list of Pos. In taxicab geometry, a disc is a square
  ; sitting on one of its vertices.
  (list-comp (+ center (Pos dx dy))
    [
      dx (seq (- radius) radius)
      dy (seq (- (abs dx) radius) (- radius (abs dx)))]
    (on-map (+ center (Pos dx dy)))))

(defn circ-taxi [
    center  ; Pos
    radius] ; int
  ; Like disc-taxi, but with only the boundary points.
  (list-comp (+ center (Pos dx dy))
    [
      dx (seq (- radius) radius)
      dy [(- (abs dx) radius) (- radius (abs dx))]]
    (on-map (+ center (Pos dx dy)))))

(defn in-los? [eye target] (block
  (for [p (cut (line-bresen eye target) 1)]
    (when (. (mget p) blocks-sight)
      (ret F)))
  T))

(defclass Floor [Tile] [
  name (NounPhrase "ordinary floor" :mass T :unit "tiles")
  char "."
  info-text "Just what it says on the tin."

  smooth T])

(defclass Wall [Tile] [
  name (NounPhrase "wall")
  char "#"
  color-bg G.fg-color
  info-text (.join "\n" [
    "A common and persistent obstacle to scooping up all the prizes on the level and hopping into the down elevator."
    ""
    "  This man, with lime and rough-cast, doth present"
    "  Wall, that vile Wall which did these lovers sunder;"
    "  And through Wall's chink, poor souls, they are content"
    "  To whisper, at the which let no man wonder."])
      ; A Midsummer Night's Dream, 5.1.131–134
  blocks-movement T
  blocks-sight T])

(defclass Elevator [Tile] [
  smooth T])

(defclass UpElevator [Elevator] [
  name (NounPhrase "elevator going up")
  char "<"
  color-bg :green
  info-text "Taking the elevator back up will immediately end your game of Rogue TV, but you'll be able to keep whatever winnings you're carrying."

  use-tile (fn [self]
    (msg 'tara "Beware, there will be no return!")
    (msg "Do you really want to take the elevator up?")
    (when (user-confirms)
      (setv G.endgame :used-up-elevator)))])

(defn upelevator-pos [] (block
  (for [l Tile.omap]
    (for [t l]
      (when (instance? UpElevator t)
        (ret t.pos))))))

(defclass DownElevator [Elevator] [
  name (NounPhrase "elevator going down")
  char ">"
  color-bg :yellow
  info-text "This elevator leads to a new, unexplored level. The time limit will be reset, but you won't be able to return to this level, so make sure you're carrying whatever you intend to keep."

  get-depth (fn [self]
    1)

  use-tile (fn [self]
    (if (= G.dungeon-level G.max-dungeon-level)
      (if (afind-or (instance? (get G.itypes "aoy") it) G.inventory)
        (do
          (msg 'aud "goes wild! You won the game!")
          (setv G.endgame :won))
        (msg 'tara "Sorry, {p}, you'll need the Amulet of Yendor to finish the game."))
      (do
        (when G.autosave
          (rtv saves.write-save-file G.save-file-path))
        (when (<= (- G.time-limit G.current-time) G.super-low-time-threshold)
          ; The audience was counting down when the player got
          ; to the down elevator.
          (msg 'aud "cheers. You made it!"))
        (setv G.dungeon-level (min G.max-dungeon-level
          (+ G.dungeon-level (.get-depth self))))
        (rtv mapgen.reset-level)
        (msg 'tara
          (if (= G.dungeon-level G.max-dungeon-level)
            "{p} has reached level {}, the final level. It's {} by {} squares. {p} must now find the mystical Amulet of Yendor and take the final down elevator to win Rogue TV!"
            "And {p:he's} on to level {}. It spans {} by {} squares.")
          (inc G.dungeon-level) G.map-width G.map-height))))])

(defclass ExpressElevator [DownElevator] [
  name (NounPhrase "express elevator")
  color-bg :orange

  info-text "A down elevator that will take you {G.express-elevator-min-depth} to {G.express-elevator-max-depth} floors down, instead of just one. You'd better be prepared."

  __init__ (meth [depth]
    (DownElevator.__init__ @@)
    (setv @depth depth))

  get-depth (meth []
    @depth)])

(defclass Hellevator [DownElevator] [
  name (NounPhrase "hellevator")
  color-bg :red

  info-text "A down elevator for the really brave that will take you all the way to the bottom floor. (No relation to the competing game show.)"

  get-depth (meth []
    G.max-dungeon-level)])

(defclass Door [Tile] [
  color-fg :brown
  info-text "Rogue TV has obtained only the most rotten and ill-fitting of doors to block your progress through the level. They're unlocked, but heaving them open will take some time. And once open, they'll swing shut after a while."])

(defclass ClosedDoor [Door] [
  name (NounPhrase "closed door")
  char "+"

  blocks-movement T
  blocks-sight T

  open-time (fn [self]
    (seconds (+ 2 G.dungeon-level (randint 1 8))))

  bump-into (fn [self cr] (block
    (unless cr.can-open-doors
      (ret T))
    (setv open-time (.open-time self))
    (if (.get-effect cr (rtv-get creature.Strength))
      (msgp cr "You effortlessly kick the door open.")
      (do
        (msgp cr "You open the old door after a struggle.")
        (.take-time cr open-time)))
    (mset self.pos (OpenDoor open-time))
    F))])

(defclass OpenDoor [Door] [
  name (NounPhrase "open door")
  char "|"

  smooth T
  close-time (fn [self]
    (seconds (+ 5 (randexp (* 60 (inc G.dungeon-level))))))
      ; Yes, doors take longer to close at deeper levels, even
      ; though that makes things easier rather than harder. The
      ; point is to keep the question of whether a door will
      ; close within a given period of time practically
      ; uncertain. Later levels involve more travel and bigger
      ; time losses due to obstacles, so we have to compensate.

  __init__ (fn [self open-time]
    (Door.__init__ self)
    (.schedule self)
    (.take-time self (+ open-time (.close-time self)))
    None)

  act (fn [self]
    (if (.at (rtv-get creature.Creature) self.pos)
      (.wait self)
      (do
        (mset self.pos (ClosedDoor))
        (.deschedule self))))])

(defclass Chest [Tile] [
  name (NounPhrase "chest")
  char "■"
  color-fg :brown
  info-text "This is a treasure chest. How exciting! On average, chests contain better items than are found on the floor of the same dungeon level. You aren't one of those goody-two-shoes video-game heroes who only open locks with the proper key, and these locks aren't very good, so given enough time, you can bust a chest open."
  container T
  opaque-container T

  open-time (meth []
    (+ (seconds 3) (round-to-second (randexp-dl-div 20))))

  use-tile (meth []
    (if (.get-effect G.player (rtv-get creature.Strength))
      (msg "You effortlessly tear the lid off the chest.")
      (do
        (msg "With difficulty, you open the chest.")
        (.take-time G.player (@open-time))))
    (@open-chest G.player))

  open-chest (meth [cr]
    (setv p self.pos)
    (mset p (Floor))
    (setv item (.at (rtv-get item.Item) p))
    (if item
      (msgp cr "You found {} {:a:full}." (.xml-symbol item) item)
      (msgp cr "The chest was empty."))
    item)])

(defclass GlassChest [Chest] [
  name (NounPhrase "glass chest")
  char "□"
  info-text "Implemented, but doesn't spawn currently, because it's too annoying. (An item in a glass chest looks like an item just sitting on the floor unless the player uses look mode.)"
  opaque-container F])

(defclass DoublingMachine [Tile] [
  name (NounPhrase "double-or-nothing machine")
  char "⁑"
  color-fg :red
  info-text "This gambling game doubles as an altar to Missingno, the patron deity of item-duplication glitches. Insert an item to receive, with even odds, two of the same item or nothing. Either way, the machine will then rotate into the floor, so you can only use it once."

  usage-time (seconds 1)

  use-tile (meth [] (block
    (unless (room-for? (rtv-get item.Item) @pos)
      (msg "The machine makes a high-pitched beep. A message on a tiny screen reads: CLEAR OUTPUT BAY.")
      (ret))
    (setv i (inventory-loop "What do you want to try doubling?"))
    (when (none? i)
      (ret))
    (setv item (get G.inventory i))
    (.take-time G.player @usage-time)
    (when (or item.indestructible item.unique)
      (msg "The machine makes a high-pitched beep and spits {:your} back out. A message on a tiny screen reads: {}."
        item
        (cond
          [item.indestructible "OBJECT INSOLUABLE"]
          [item.unique "OBJECT PATENT-ENCUMBERED"]))
      (ret))
    (if (1-in 2)
      (do
        (msg "The machine plays a jolly little tune. It spits {:the} back out and leaves a copy on the floor." item)
        (.clone item @pos))
      (do
        (msg "The machine plays a sad little tune. You lost {:the}." item)
        (.delete item)))
    (mset @pos (Floor))))])

(defclass HasTimePenalty [object] [

  min-exit-time (fn [self] (seconds 1))
  max-exit-time (fn [self] (seconds (* 2 (inc G.dungeon-level))))
  
  do-time-penalty (fn [self cr]
    (.take-time cr (round-to-second
      (uniform (.min-exit-time self) (.max-exit-time self)))))])

(defclass Slime [Tile HasTimePenalty] [
  name (NounPhrase "slime" :mass T :unit "puddles")
  char "}"
  info-text "Stepping into this mystery sludge takes time."
    ; The term "mystery sludge" is from MXC.
  color-fg :white
  color-bg :dark-green

  unpleasant T

  after-step-onto (fn [self cr p-from]
    (unless (or cr.flying cr.slime-immune)
      (.do-time-penalty self cr)))])

(defclass Web [Tile HasTimePenalty] [
  name (NounPhrase "spiderweb")
  char "%"
  color-fg :dark-blue
  info-text "This sizable web will make stepping into its tile difficult, just like slime. Furthermore, while you're standing in a web, you can't apply or drop items."

  unpleasant T

  use-item-while-here (fn [self]
    (if G.player.web-immune
      T
      (do
        (msg 'tara "{p:The} is stuck in the web. {p:He} can't use items.")
        F)))

  after-step-onto (fn [self cr p-from]
    (unless cr.web-immune
      (.do-time-penalty self cr)))])

(defclass Ice [Tile] [
  name (NounPhrase "patch of ice"
    :plural "patches of ice")
  char ":"
  color-fg :white
  color-bg :pale-azure
  info-text "A slippery sort of tile. If the next thing you do after stepping on it is move again in the same direction, you'll glide along without an issue. But if you take any other action, you'll need to take a moment to steady yourself first."

  unpleasant T
  smooth T

  min-slip-time (fn [self] (seconds 1))
  max-slip-time (fn [self] (seconds (* 2 (inc G.dungeon-level))))

  after-step-onto (fn [self cr p-from]
    (unless (or cr.flying (.ice-immune cr))
      ; Set up the creature to slip.
      (setv cr.ice-slip-towards (- self.pos p-from))
      (setv cr.ice-slip-time (round-to-second
        (uniform (.min-slip-time self) (.max-slip-time self))))))

  step-out-of (fn [self cr p-to]
    ; If the creature moves in the same direction it moved to
    ; get here, it doesn't slip.
    (when (= (- p-to self.pos) cr.ice-slip-towards)
      (cr.reset-ice-slipping)))])

(defclass StasisTrap [Tile] [
  name (NounPhrase "stasis trap")
  name-suffix (meth [] (if @trap-active "(on)" "(off)"))

  get-char (meth [] (if @trap-active "✦" "✧"))
  get-color-bg (meth [] (if @trap-active :dark-orange :gold))

  info-text "This tile toggles between two states, on and off, in a fixed pattern. Any creature caught on the tile when it's on will be temporarily frozen. The trap will stay off for the duration of this stasis plus a 1-second grace period, then return to its old pattern."

  unpleasant T
  smooth T

  freeze-time (meth [] (+ @on-time (seconds
    (randint (inc G.dungeon-level) (* 3 (inc G.dungeon-level))))))
  grace-time (seconds 1) ; N.B. Hard-coded in the info-text above.

  __init__ (meth [off-time on-time]
    (Tile.__init__ @@)
    (set-self off-time on-time)
    (setv @trap-active F)
    (@schedule)
    (when @pos
      (@act))
    None)

  act (meth []
    (setv cycle-time (+ @off-time @on-time))
    (setv cycle-pos (% G.current-time cycle-time))
    (setv last-cycle-start (* (// G.current-time cycle-time) cycle-time))
    (setv @trap-active (>= cycle-pos @off-time))
    (setv @next-turn (+ last-cycle-start
      (if @trap-active cycle-time @off-time)))
    (@freeze-creature))

  after-entering (meth [cr p-from]
    (@freeze-creature))

  freeze-creature (meth []
    (when @trap-active (whenn (.at (rtv-get creature.Creature) @pos)
      (cond
        [(player? it) (msg "You are frozen by {:the}." @@)]
        [(seen @pos) (msg "{:The} {:v:is} frozen by {:the}." it it @@)])
      (setv freeze-time (@freeze-time))
      (.take-time it freeze-time)
      (setv @trap-active F)
      (setv @next-turn (+ G.current-time freeze-time @grace-time)))
      T))])

(defclass PusherTile [Tile HasTimePenalty] [
  children {}
  name (NounPhrase "pusher tile")
  name-suffix (meth [] (.format "(pointing {})" (get Pos.DIRNAMES @push-dir)))
  color-bg :pale-pink

  info-text  "This tile exerts a mysterious force that pushes creatures in the direction of the arrow printed on it. Exiting it from any direction except the indicated one (even diagonally) will slow you down."

  step-out-of (meth [cr p-to]
    (unless (= p-to (+ @pos @push-dir))
      (@do-time-penalty cr)))])

(for [d Pos.ORTHS]
  (setv cname (str (+ "PusherTile" (ucfirst (get Pos.DIRNAMES d)))))
  (setv (get (globals) cname) (type cname (, PusherTile) {
    "char" (get {Pos.EAST "→" Pos.NORTH "↑" Pos.WEST "←" Pos.SOUTH "↓"} d)
    "push_dir" d}))
  (setv (get PusherTile.children d) (get (globals) cname)))

(defclass SpookyTotem [Tile] [
  name (NounPhrase "spooky totem")
  char "&"
  color-fg :dark-orange
  info-text (.format "This is a tall, unsettling sculpture of fierce, alien-looking monsters twisting about each other and brandishing their teeth at the viewer. Just being within {} squares of it, even if you can't see it, will make you tremble with fear, slowing your movements. Multiple spooky totems have a cumulative effect." G.spook-radius)

  spooky T

  __init__ (meth []
    (Tile.__init__ @@)
    (setv @player-noticed-spook F)
    None)])

; Below, we create a dictionary tile-save-shorthand which
; holds unambiguous single-character abbreviations for tiles.
; This is used in roguetv.saves for more concise serialization.
(setv tile-save-shorthand ((fn []
  (setv d {})
  ; Match each tile subclass to its char.
  (for [cls (.values (globals))]
    (try
      (unless (issubclass cls Tile)
        (continue))
      (except [TypeError]
        (continue)))
    (setv (get d cls) cls.char))
  ; Remove ambiguous entries (for which a single char has more than
  ; one associated tile).
  (import [collections [Counter]])
  (setv counts (Counter (.values d)))
  (for [[cls char] (list (.items d))]
    (unless (in cls d)
      (continue))
    (when (> (get counts char) 1)
      (del (get d cls))))
  ; But use ">" for plain DownElevators, even though it's the char for
  ; subtypes too. It's much more common than its subtypes.
  (setv (get d DownElevator) ">")
  d)))
