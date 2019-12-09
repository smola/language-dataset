Main -> OCCASION POEM

OCCASION -> "One morning, after" MYLOVER " left," NOTE1|"The empress held a poetry contest." NOTE2
NOTE1 	-> " he sent me a letter" LETTER ITREAD
NOTE2	-> " Her poem was a great succes: \n     "|" My entry caused a great stir: \n     "

LETTER -> " wrapped with fresh grasses."| " on Chinese paper"|" bound with rough cord"
ITREAD -> " It read: \n     "|" I replied: \n     "
MYLOVER -> " Yasunari"|" my lover"


POEM -> TYPEONE|TYPETWO


TYPEONE -> "Though" HUMANPHONE "," HUMANPHTWO ", \n     " MYFEELSARELIKE CLOSINGPHRASE
TYPETWO -> THISMORNING INTOVIEW "\n     " ISLETS

# Strcuture ONE
HUMANPHONE -> " our time was so brief"| " the days fly by"| " I lie awake" " thinking of you":?| " I long for you"| " I await your call"| " my" ROBES AREWET INDEW
HUMANPHTWO -> " how sad it is"| " we must part"| " it cannot be"| " you bid me come"| " you do not call for me"| " dawn still will not come"| " who will share my bed"| " my pillow lies cold"| " who will warm my bed"
MYFEELSARELIKE	-> " my heart is" ISLIKE|" my love" " for you":? " is" ISLIKE|" my name lies forgotten,"|" my cares are" ISLIKE|" our love is" ISLIKE|" my empty bed is" ISLIKE|" the lonesome night is" ISLIKE|" my" DEWDRENCHED ROBES " are" ISLIKE

ROBES 		-> " robes"| " sleeves"| " hems"
AREWET 		-> " are wet"| " are damp"| " are drenched"| " are soaked"
INDEW		-> " with dew"| " with tears"
DEWDRENCHED -> " dew drenched"| " tear stained"
ISLIKE 		-> " like "|" as "

CLOSINGPHRASE -> NATUREPHRASE|WEATHERPHRASE|HUMANPHRASE|FAMOUSPHRASE|SOUNDPHRASE

# Structure two

THISMORNING -> "This morning,"|"As I like awake,"|" As the" SKY " rose,"|"When I woke,"
INTOVIEW 	-> PLACE GEOGRAPHY " came into view,"|" I caught" SOUNDPH ","| " I watched " CLOSINGPHRASE ","
ISLETS		-> HUMANPHTWO|HUMANEXTRAS|BOATSPH

BOATSPH		-> BOATSONE BOATSTWO

BOATSONE	-> " boats cast against the sky"|" sails glint in the sun"|" fishermen dotted amongst the islets"| " boats drifting in the current"
BOATSTWO	-> " carrying us across the sorrow filled sea of life"
HUMANEXTRAS	-> " how can you be so cold?"



# Phrases
NATUREPHRASE 	-> BIRDPH|TREEPH|ANIMALPH|FLOWERPH
WEATHERPHRASE	-> WEATHERPH
SOUNDPHRASE		-> SOUNDPH 
FAMOUSPHRASE	-> "the" FAMOUSCORE OFOLD
HUMANPHRASE		-> PEASANTPH

WHATEVER -> INSECT  |LAND|RIVER |BUILDING|PLACE|SEASON


BIRDPH 		-> "a" FLOCK BIRD "s" WING:?|"a" BIRD WING:?|"a" FLOCK BIRD "s" " above" 					PALACEPLACE| "a" BIRD " above" PALACEPLACE
TREEPH 		-> "a" TREE " tree" SEASONTIME|"the" TREE " trees which line the" RIVER
ANIMALPH 	-> "a" ANIMAL PLAYING SEASONTIME|"a" ANIMAL PLAYING LOCATIONAML| "a" ANIMAL 				" in" PALACEPLACE
FLOWERPH	-> "a" BUNCH INAVASE:?
WEATHERPH	-> "the" SKY " above the" LAND SEASONTIMELOCATION| "the" SEAMODPRE:? SEA 					SEAMODPOST:? SEASONTIMELOCATION|"the" WEATHER  SEASONTIMELOCATION
PEASANTPH	-> "a" PEASANTPRE:? PEASANT PEASANTACTION:? SEASONTIMELOCATION:?|"a group of" 				  PEASANT "s" PEASANTACTION:? SEASONTIMELOCATION:?
FAMOUSCORE	-> BIRD "s"|ANIMAL "s"|BUILDING|LAND|RIVER
SOUNDPH		-> "the" BIRDSOUND BIRD SEASONTIMELOCATION:?| "the ring of the temple bell at" 					PLACE|"the noise of the" SEAMODPRE:? SEA	SEAMODPOST:? 								SEASONTIMELOCATION|"the sound of the" RIVER 

#  MODIFIERS

BIRDSOUND	-> " sound of the"|" call of the"|" song of the"
WING 		-> " on the wing"|" in flight"|" at roost"| " wheeling beneath the" SKY
FLOCK 		-> " flock of"|" pair of"|" few"
SEASONTIME 	-> " in" SEASON| TIMEOFDAY
SEASONTIMELOCATION -> " in" SEASON| TIMEOFDAY| " at" PLACE
PLAYING 	-> " playing"|" hunting"
LOCATIONAML	-> " in the" LAND|" in the" RIVER " shallows"
BUNCH 		-> " single" FLOWER| " bunch of" FLOWERS| " few" FLOWERS| " fresh" FLOWER| " freshly cut" FLOWER
INAVASE		-> " in a" VMODIFIER " vase"
VMODIFIER	-> " chinese"| " korean"| " small"| " blue"| " patterned"| " cracked"| " old"
SEAMODPRE	-> " stormy"|" angry"|" turbulent"|" raging"
SEAMODPOST	-> " that batters the shore"
RAINMOD		-> " endless"|" teeming"|" wind-blown"|" sudden"
RIVERMOD	-> " slow"| " rain-swollen"
PEASANTPRE	-> " young"| " aging"| " old"| " simple"
PEASANTACTION -> " working"|" at work under the" SKY|" resting"
OFOLD		-> " of old"|" of" PLACE

# Baseline items
BIRD 		-> " nightingale"|" snipe"|" sparrow"|" gull"|" pheasant"|" starling"|						" partridge"|" heron"|" crane"|" crow"|" jackdaw"|" raven"|" duck"|						" plover"|" cuckoo"|" hen"
ANIMAL 		-> " deer"|" fox"|" badger"|" ox"|" horse"|" bear"|" mouse"|" dog"|" cat"|					" goat"|" boar"
WATERANIMAL -> " turtle"|" carp"
INSECT 		-> " cricket"|" butterfly"|" firefly"|" dragonfly"|" cicada"|" grasshopper"|				" moth"
TREE 		-> " plum"|" cherry"|" pear"|" maple"|" cypress"|" oak"|" pine"|" willow"|					" pawlonia"|" hinoki"|" gingko"
FLOWER 		-> " iris"|" lily"|" lotus"|" reed"|" grass"|" rose"
FLOWERS 	-> " irises"|" lillies"|" lotuses"|" reeds"|" grasses"|" roses"
WEATHER 	-> " purple":? " clouds"| " swirling":? " mist"|RAINMOD:? " rain" "fall":?|					" soundless":? " snow" |" fog"|" mist"|" wind"
SKY 		-> " stars"|" moon"|" sun"|" clouds"|" night"|" dawn"|" sky"
LAND 		-> " hills"|" mountains"|" forest"|" woods"|" fields"
SEA 		-> " sea"|" ocean"|" waves"|" swell"
RIVER 		-> " tumbling":? " stream"|" sparkling":? " brook"|RIVERMOD:? " river"|						" water"
BUILDING 	-> PLACE:? " temple"|PLACE:? " shrine"|" room"|" corridor"|"humble":? 						"straw-thatched":? " hut"| " meagre":? " peasant's":? " shack"
PALACEPLACE -> " the palace gardens"| " the Empress's chambers"| " the Western Gate"| 					" the office of the Captain of the guards" | " Sugamo shrine"| 							" Tengmangu shrine"|" my chambers"| " a back corridor"
PLACE 		-> " Izumo"|" Nara"|" Yoshino"|" Owari" " plains":?|" Ise"|" Osaka" 						" barrier":? " gate":?|" Yodo"
SEASON 		-> " summer"|" autumn"|" spring"|" winter"
TIMEOFDAY 	-> " at dusk"|" at dawn"|" in the early evening"|" in the morning"|							" by night"|" in the afternoon"
PEASANT		-> " diver"| " farmer"| " hunter"| " peasant"| " priest"| " monk"
GEOGRAPHY	-> " bay"|" barrier"|" temple"|" shrine"