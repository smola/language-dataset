// Layout by dadcore digital
// Adapted from a a layout by cools / Arcade Otaku
// Uses video
// Fields Used: 
//    Name
//    Title
//    Emulator
//    Manufacturer (Dev shop / company name)
//    Extra (used as description field)
///////////////////////////////////////////////////////// 

// Constants
class UserConfig {
 bg_image = "video";
 enable_list="Yes"; 
}

local my_config = fe.get_config();

// Layout Constants
fe.layout.width=1920;
fe.layout.height=1080;

local lw=fe.layout.width;
local lh=fe.layout.height;
local bgx=(lw/8)*-1;
local bgy=(lh/8)*-1
local bgw=(lw/4)*5;
local bgh=(lh/4)*5;

// Game name text. We do this in the layout as the frontend doesn't chop up titles with a forward slash
function gamename( index_offset ) {
 local s = split( fe.game_info( Info.Title, index_offset ), "(/[" );
 if ( s.len() > 0 ) return s[0];
 return "";
}

// Returns a random number below 255. Set randrange to higher values to hit closer to 255
function highrand( randrange ) {
 return 255-(rand()/randrange);
}

// Random high colour values
local red = highrand( 255 );
local green = highrand( 255 );
local blue = highrand( 255 );
local grey = highrand( 1024 );

/////////////////////////////////////////////////////////
// On Screen Objects

// Background Image
local bg = fe.add_artwork( "snap", 0, 0, 1920, 1080 );
local bgmask = fe.add_image ("bgmask.png", 0, 0, lw, lh);

// Game title background
local titlemask = fe.add_image ("titlemask.png", 0, 756, lw, lh-198);
titlemask.set_rgb (0,0,0);

// Game title text
local gametitleshadow = fe.add_text( gamename ( 0 ), 55, 803, lw - 10, 55 );
gametitleshadow.align = Align.Left;
gametitleshadow.set_rgb (0,0,0);
local gametitle = fe.add_text( gamename ( 0 ), 55, 800, lw - 10, 55 );
gametitle.align = Align.Left;

local company = fe.add_text( fe.game_info( Info.Manufacturer), 70, 870, lw - 30, 35 );
company.align = Align.Left;

// Description
local desc = fe.add_text( "Blah", 80, 900, lw - 200, 100 );
desc.align = Align.Left;
desc.charsize = 22;
desc.word_wrap = true;

// Game Text
local romlist = fe.add_text( "[DisplayName]", 2, 202, 315, 30 );
romlist.align = Align.Right;
local cat = fe.add_text( fe.game_info (Info.Category), 2, 212, 315, 10 );
local entries = fe.add_text( "[ListEntry]/[ListSize]", 2, 840, lw-50, 50 );
entries.align = Align.Right;
local filter = fe.add_text( "[FilterName]", -20, 190, lw+40, lh-191 );
filter.align = Align.Right;
filter.alpha = 0;

// This hides the emulator name or something?
romlist.visible = false;

// Transitions
fe.add_transition_callback( "fade_transitions" );
function fade_transitions( ttype, var, ttime ) {
 switch ( ttype ) {
  case Transition.ToNewList:
   var = 0;
  case Transition.ToNewSelection:
   gametitleshadow.msg = gamename ( var );
   gametitle.msg = gametitleshadow.msg;
   // company.msg = copyright ( var );
   company.msg = fe.game_info( Info.Manufacturer, var )
   desc.msg = fe.game_info (Info.Extra, var);
   red = highrand( 255 );
   green = highrand( 255 );
   blue = highrand( 255 );
   romlist.set_rgb (red,green,blue);
   filter.set_rgb (red,green,blue);
   entries.set_rgb (red,green,blue);
   company.set_rgb (red,green,blue);
   break;
  }
 return false;
}
