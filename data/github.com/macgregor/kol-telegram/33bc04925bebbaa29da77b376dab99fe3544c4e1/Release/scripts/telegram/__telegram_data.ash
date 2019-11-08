
static monster[string] EASY_QUESTS = {
  "Missing: Fancy Man": $monster[Jeff the Fancy Skeleton],
  "Help! Desperados!": $monster[Pecos Dave],
  "Missing: Pioneer Daughter": $monster[Daisy the Unclean]
};

static monster[string] MEDIUM_QUESTS = {
  "Big Gambling Tournament Announced": $monster[Snake-Eyes Glenn],
  "Haunted Boneyard": $monster[Pharaoh Amoon-Ra Cowtep],
  "Sheriff Wanted": $monster[Former Sheriff Dan Driscoll],
};

static monster[string] HARD_QUESTS = {
  "Madness at the Mine": $monster[unusual construct],
  "Missing: Many Children": $monster[Clara],
  "Wagon Train Escort Wanted": $monster[Granny Hackleton],
};

static int ACCEPT_EASY_QUEST = 1;
static int ACCEPT_MEDIUM_QUEST = 2;
static int ACCEPT_HARD_QUEST = 3;
static int LEAVE_OFFICE = 8;
static int ACCEPT_OVERTIME = 4; // ?? not sure what choice is
