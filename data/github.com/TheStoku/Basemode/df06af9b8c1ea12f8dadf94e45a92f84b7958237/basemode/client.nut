/* ############################################################## */
/* #			BaseMode v1.0 final by Stoku						# */
/* #					Have fun!								# */
/* ############################################################## */

local SCRIPT_VERSION			= "1.0 final";
local SCRIPT_AUTHOR				= "Stoku";

local GUI_THEME_MAIN_COLOR 		= Colour( 0, 255, 0 );
local SETTING_MUTE				= false;
local WEP_AFK_KILLER 			= 155;

g_pLocalPlayer <- FindLocalPlayer();
g_pMarker <- null;
g_Timer <- null;
g_iGameState <- 0;		// 0-lobby, 1=base
g_bSpectating <- false;
g_iLastMoveTime <- 0;
g_isAFK	<- false;
g_iRoundStartTime <- 0;

g_HelpWindow <- null;
g_HelpMemo <- null;

g_Logo <- null;
g_StatsPanel <- null;
g_StatsPanelGradient <- null;
g_Team1StatsLabel <- null;
g_Team2StatsLabel <- null;
g_SpawnScreenLabel <- null;
g_SpawnScreenLabel2 <- null;
g_ScoreLabel <- null;
g_TimeLabel <- null;
g_SpeedoLabel <- null;
g_CaptureWindow <- null;
g_CaptureBar <- null;
g_HealthBar <- null;

g_VoteLabel <- null;
g_VoteYes <- null;
g_VoteNo <- null;

// Select weapon window
g_SelectWeaponWindow <- null;
g_SelectWeaponLabel1 <- null;
g_SelectWeaponLabel2 <- null;
g_SelectWeaponLabel3 <- null;
g_SelectPrimaryWeaponButton0 <- null;
g_SelectPrimaryWeaponButton1 <- null;
g_SelectPrimaryWeaponButton2 <- null;
g_SelectPrimaryWeaponButton3 <- null;
g_SelectPrimaryWeaponButton4 <- null;
g_SelectSecondaryWeaponButton0 <- null;
g_SelectSecondaryWeaponButton1 <- null;
g_SelectSecondaryWeaponButton2 <- null;
g_SelectAdditionalWeaponButton0 <- null;
g_SelectAdditionalWeaponButton1 <- null;
g_SelectAdditionalWeaponButton2 <- null;
g_SelectAdditionalWeaponButton3 <- null;
g_SelectOKButton <- null;

g_PrimaryWeapon <- null;
g_SecondaryWeapon <- null;
g_AdditionalWeapon <- null;

g_TeamChatEditbox <- null;

g_OwnRadio <- 255;
g_RandomRadio <- 0;

class CGame
{
	RoundTime = 0;
	CountdownTime = 0;
	IsAttacker = false;
	IsRoundInProgress = false;
	IsVotingInProgress = false;
}

class CSettings
{
	ColtAmmo = 0;
	UZIAmmo = 0;
	ShotgunAmmo = 0;
	AKAmmo = 0;
	M16Ammo = 0;
	RifleAmmo = 0;
	MolotovAmmo = 0;
	GrenadeAmmo = 0;
	BaseballBat = false;
	MaxPlayers = 0;
	AFKSlapTime = 0;
}

function onScriptLoad()
{
	ConsoleMessage( "This server is using BaseMode v" + SCRIPT_VERSION + " by " + SCRIPT_AUTHOR + "." );
	
	//PlayFrontEndTrack( 191 );
	
	//BindKey( '0', BINDTYPE_DOWN, "SpectatePlayer_Next" );
	//BindKey( '9', BINDTYPE_DOWN, "SpectatePlayer_Previous" );

	BindKey( KEY_F1, BINDTYPE_DOWN, "ShowHelpWindow", "1" );
	BindKey( KEY_F1, BINDTYPE_UP, "ShowHelpWindow", "0" );
	BindKey( '1', BINDTYPE_UP, "SendTeamMessage", "[#ffff00]Enemy Spotted!" );
	BindKey( '2', BINDTYPE_UP, "SendTeamMessage", "[#ffff00]Need backup!" );
	BindKey( '3', BINDTYPE_UP, "SendTeamMessage", "[#ffff00]Follow me!" );
	BindKey( '4', BINDTYPE_UP, "SendTeamMessage", "[#ffff00]Incoming!" );
	BindKey( '5', BINDTYPE_UP, "SendTeamMessage", "[#ffff00]Go, go, go!" );
	BindKey( '6', BINDTYPE_UP, "SendTeamMessage", "[#ffff00]Roger that!" );
	//BindKey( '0', BINDTYPE_DOWN, "plus" );
	//BindKey( '9', BINDTYPE_DOWN, "minus" );
	
	BindKey( 'Y', BINDTYPE_UP, "ToggleTeamChat" );
	
	pGame <- CGame();
	pSettings <- CSettings();
	
	SetHUDItemEnabled( HUD_MONEY, false );
	SetHUDItemEnabled( HUD_WANTED, false );
	SetNametagDrawDistance( 30.0 );
	
	g_Timer = NewTimer( "TimeProcess", 1000, 0 );
	g_Timer.Start();
	
	g_Logo = GUISprite( "basemode.png", VectorScreen( 5, ScreenHeight - 60 ));
	AddGUILayer( g_Logo );
	
	g_StatsPanelGradient = GUIWindow( VectorScreen( 0, ScreenHeight - 15 ), ScreenSize( ScreenWidth, 15 ), "Statistics" );
	g_StatsPanelGradient.Colour = Colour( 0, 0, 0 );
	g_StatsPanelGradient.Titlebar = false;
	g_StatsPanelGradient.Alpha = 50;
	g_StatsPanelGradient.Visible = true;
	AddGUILayer( g_StatsPanelGradient );
	
	g_StatsPanel = GUIWindow( VectorScreen( 0, ScreenHeight - 25 ), ScreenSize( ScreenWidth, 25 ), "Statistics" );
	g_StatsPanel.Colour = Colour( 0, 0, 0 );
	g_StatsPanel.Titlebar = false;
	g_StatsPanel.Alpha = 50;
	g_StatsPanel.Visible = true;
	AddGUILayer( g_StatsPanel );
	
	g_ScoreLabel = GUILabel( VectorScreen( ScreenWidth/2, 0 ), ScreenSize( ScreenWidth, 30 ), "0:0" );
	g_ScoreLabel.TextColour = Colour( 255, 255, 255 );
	g_ScoreLabel.Alpha = 150;
	g_ScoreLabel.Flags = FLAG_SHADOW;
	g_ScoreLabel.Visible = true;
	//g_ScoreLabel.FontName = "Veranda";
	g_ScoreLabel.FontSize = 12;
	g_ScoreLabel.FontTags = TAG_BOLD;
	g_ScoreLabel.TextAlignment = ALIGN_TOP_CENTER;
	g_StatsPanel.AddChild( g_ScoreLabel );
	
	g_Team1StatsLabel = GUILabel( VectorScreen( 5, -2 ), ScreenSize( ScreenWidth, 25 ), "Red (Attack)" );
	g_Team1StatsLabel.TextColour = Colour( 255, 0, 0 );
	g_Team1StatsLabel.Alpha = 150;
	g_Team1StatsLabel.Flags = FLAG_SHADOW;
	g_Team1StatsLabel.Visible = true;
	//g_Team1StatsLabel.FontName = "Veranda";
	g_Team1StatsLabel.FontSize = 12;
	g_Team1StatsLabel.FontTags = TAG_BOLD;
	g_Team1StatsLabel.TextAlignment = ALIGN_TOP_LEFT;
	g_StatsPanel.AddChild( g_Team1StatsLabel );
	
	g_Team2StatsLabel = GUILabel( VectorScreen( ScreenWidth - 5, -2 ), ScreenSize( ScreenWidth, 25 ), "Blue (Defence)" );
	g_Team2StatsLabel.TextColour = Colour( 0, 0, 255 );
	g_Team2StatsLabel.Alpha = 150;
	g_Team2StatsLabel.Flags = FLAG_SHADOW;
	g_Team2StatsLabel.Visible = true;
	//g_Team2StatsLabel.FontName = "Veranda";
	g_Team2StatsLabel.FontSize = 12;
	g_Team2StatsLabel.FontTags = TAG_BOLD;
	g_Team2StatsLabel.TextAlignment = ALIGN_TOP_RIGHT;
	g_StatsPanel.AddChild( g_Team2StatsLabel );
	
	g_CaptureWindow = GUIWindow( VectorScreen( ScreenWidth - 400, 300 ), ScreenSize( 300, 6 ), "Capturing" );
	g_CaptureWindow.Colour = Colour( 0, 0, 0 );
	g_CaptureWindow.Titlebar = false;
	//g_CaptureWindow.Alpha = 50;
	g_CaptureWindow.Visible = false;
	AddGUILayer( g_CaptureWindow );
	
	g_CaptureBar = GUIProgressBar( VectorScreen( 3, 3 ), ScreenSize( 293, 20 ));
	g_CaptureBar.Visible = true;
	g_CaptureBar.StartColour = Colour( 0, 150, 0 );
	g_CaptureBar.EndColour = Colour( 0, 255, 0 );
	g_CaptureBar.MaxValue = 15;
	g_CaptureBar.Value = 0;
	g_CaptureWindow.AddChild( g_CaptureBar );
	
	g_HealthBar = GUIProgressBar( VectorScreen( 0, 0 ), ScreenSize( ScreenWidth, 25 ));
	g_HealthBar.Visible = true;
	g_HealthBar.StartColour = Colour( 255, 0, 0 );
	g_HealthBar.EndColour = Colour( 0, 255, 0 );
	g_HealthBar.MaxValue = 1000;
	g_HealthBar.Value = 0;
	g_HealthBar.Alpha = 255;
	g_HealthBar.Thickness = 0;		// this is a border size
	g_StatsPanel.AddChild( g_HealthBar );
	
	g_TimeLabel = GUILabel( VectorScreen( ScreenWidth/2, 0 ), ScreenSize( ScreenWidth, ScreenHeight ), "" );
	g_TimeLabel.TextColour = Colour( 255, 255, 255 );
	g_TimeLabel.Alpha = 200;
	g_TimeLabel.Flags = FLAG_SHADOW;
	g_TimeLabel.FontTags = TAG_BOLD;
	g_TimeLabel.TextAlignment = ALIGN_TOP_CENTER;
	g_TimeLabel.FontSize = 20;
	g_TimeLabel.Visible = true;
	AddGUILayer( g_TimeLabel );
		
	g_SpeedoLabel = GUILabel( VectorScreen( ScreenWidth - 10, ScreenHeight - 60 ), ScreenSize( 0, 0 ), "" );
	//g_SpeedoLabel.FontTags = TAG_BOLD;
	g_SpeedoLabel.TextColour = Colour( 255, 255, 255 );
	g_SpeedoLabel.Alpha = 250;
	g_SpeedoLabel.Flags = FLAG_SHADOW;
	g_SpeedoLabel.TextAlignment = ALIGN_TOP_RIGHT;
	g_SpeedoLabel.Visible = true;
	g_SpeedoLabel.FontName = "Impact";
	g_SpeedoLabel.FontSize = 16;
	AddGUILayer( g_SpeedoLabel );
	
	g_TeamChatEditbox = GUIEditbox( VectorScreen( 5, ( ScreenHeight / 2 )), ScreenSize( 500, 25 ));
	g_TeamChatEditbox.Colour = GUI_THEME_MAIN_COLOR;
	//g_TeamChatEditbox.FontTags = TAG_BOLD;
	g_TeamChatEditbox.TextColour = Colour( 255, 255, 255 );
	g_TeamChatEditbox.Alpha = 50;
	g_TeamChatEditbox.Flags = FLAG_SHADOW;
	g_TeamChatEditbox.Visible = false;
	g_TeamChatEditbox.FontName = "Veranda";
	g_TeamChatEditbox.FontSize = 10;
	AddGUILayer( g_TeamChatEditbox );
	
	UpdateSpawnScreenScene( FindSpawnClass( 0 ));
}

function ToggleTeamChat()
{	
	if ( !g_TeamChatEditbox.Visible )
	{
		// open
		g_TeamChatEditbox.Text = "";
		UnbindKey( 'Y', BINDTYPE_UP, "ToggleTeamChat" );
		UnbindKey( '1', BINDTYPE_UP, "SendTeamMessage" );
		UnbindKey( '2', BINDTYPE_UP, "SendTeamMessage" );
		UnbindKey( '3', BINDTYPE_UP, "SendTeamMessage" );
		UnbindKey( '4', BINDTYPE_UP, "SendTeamMessage" );
		UnbindKey( '5', BINDTYPE_UP, "SendTeamMessage" );
		UnbindKey( '6', BINDTYPE_UP, "SendTeamMessage" );
		BindKey( KEY_RETURN, BINDTYPE_DOWN, "SendTeamMessage", "" );
		g_TeamChatEditbox.Visible = true;
		g_TeamChatEditbox.Active = true;
		g_pLocalPlayer.Frozen = true;
	}
	else
	{
		// close
		g_pLocalPlayer.Frozen = false;
		UnbindKey( KEY_RETURN, BINDTYPE_DOWN, "SendTeamMessage" );
		BindKey( 'Y', BINDTYPE_UP, "ToggleTeamChat" );
		BindKey( '1', BINDTYPE_UP, "SendTeamMessage", "[#ffff00]Enemy Spotted!" );
		BindKey( '2', BINDTYPE_UP, "SendTeamMessage", "[#ffff00]Need backup!" );
		BindKey( '3', BINDTYPE_UP, "SendTeamMessage", "[#ffff00]Follow me!" );
		BindKey( '4', BINDTYPE_UP, "SendTeamMessage", "[#ffff00]Incoming!" );
		BindKey( '5', BINDTYPE_UP, "SendTeamMessage", "[#ffff00]Go, go, go!" );
		BindKey( '6', BINDTYPE_UP, "SendTeamMessage", "[#ffff00]Roger that!" );
		g_TeamChatEditbox.Visible = false;
		g_TeamChatEditbox.Active = false;
	}
}

function SendTeamMessage( szText )
{
	if ( szText )
	{
		//ToggleTeamChat();
		CallServerFunc( "basemode/server.nut", "SendTeamMessage", g_pLocalPlayer, szText );
	}
	else if (( g_TeamChatEditbox.Visible ) && ( g_TeamChatEditbox.Text.len() > 0 ))
	{
		ToggleTeamChat();
		CallServerFunc( "basemode/server.nut", "SendTeamMessage", g_pLocalPlayer, g_TeamChatEditbox.Text );
	}
	else ToggleTeamChat();
}

local i = 0;

function plus()
{
	// 93 mission passssssed
	// 165 see you around
	// lock and load 158
	// 180  this is a battle for respect, you cool
	// 191 how u doin?
	// 192 how u doin kid
	// 149//160 for join

	i++;
	StopFrontEndTrack();
	PlayFrontEndTrack( i );
	//PlayFrontEndSound( i );
	Message( i.tostring());
}
function minus()
{
	i--;
	StopFrontEndTrack();
	PlayFrontEndTrack( i );
	//PlayFrontEndSound( i );
	Message( i.tostring());
}

function StartVote( iBaseID, iVoteTime )
{
	pGame.IsVotingInProgress = true;
	
	BindKey( '1', BINDTYPE_DOWN, "Vote", 1 );
	BindKey( '2', BINDTYPE_DOWN, "Vote", 0 );
	
	if ( !g_VoteLabel )
	{
		g_VoteLabel = GUILabel( VectorScreen( 10, ScreenHeight/2 ), ScreenSize( 0, 0 ), "Do you want to start base " + iBaseID + "? (" + iVoteTime + ")" );
		g_VoteLabel.TextColour = Colour( 0, 255, 200 );
		g_VoteLabel.Alpha = 200;
		g_VoteLabel.Flags = FLAG_SHADOW;
		g_VoteLabel.Visible = true;
		g_VoteLabel.FontName = "Tahoma";
		g_VoteLabel.FontSize = 9;
		AddGUILayer( g_VoteLabel );
		
		g_VoteYes = GUILabel( VectorScreen( 10, ScreenHeight/2 + 15 ), ScreenSize( 0, 0 ), "1. Yes (0)" );
		g_VoteYes.TextColour = Colour( 150, 150, 150 );
		g_VoteYes.Alpha = 200;
		g_VoteYes.Flags = FLAG_SHADOW;
		g_VoteYes.Visible = true;
		g_VoteYes.FontName = "Tahoma";
		g_VoteYes.FontSize = 8;
		AddGUILayer( g_VoteYes );
		
		g_VoteNo = GUILabel( VectorScreen( 10, ScreenHeight/2 + 30 ), ScreenSize( 0, 0 ), "2. No (0)" );
		g_VoteNo.TextColour = Colour( 150, 150, 150 );
		g_VoteNo.Alpha = 200;
		g_VoteNo.Flags = FLAG_SHADOW;
		g_VoteNo.Visible = true;
		g_VoteNo.FontName = "Tahoma";
		g_VoteNo.FontSize = 8;
		AddGUILayer( g_VoteNo );
	}
	else
	{	
		g_VoteLabel.Text = "Do you want to start base " + iBaseID + "? (" + iVoteTime + ")";
		
		g_VoteYes.TextColour = Colour( 150, 150, 150 );
		g_VoteNo.TextColour = Colour( 150, 150, 150 );
		
		g_VoteLabel.Visible = true;
		g_VoteYes.Visible = true;
		g_VoteNo.Visible = true;
	}
}

function EndVote()
{
	UnbindKey( '1', BINDTYPE_DOWN, "Vote" );
	UnbindKey( '2', BINDTYPE_DOWN, "Vote" );
	
	g_VoteLabel.Visible = false;
	g_VoteYes.Visible = false;
	g_VoteNo.Visible = false;	
}

function Vote( bBoolean )
{
	CallServerFunc( "basemode/server.nut", "Vote", g_pLocalPlayer, bBoolean );
	
	if ( bBoolean ) g_VoteYes.TextColour = Colour( 255, 255, 255 );
	else g_VoteNo.TextColour = Colour( 255, 255, 255 );
	
	UnbindKey( '1', BINDTYPE_DOWN, "Vote" );
	UnbindKey( '2', BINDTYPE_DOWN, "Vote" );
}

function UpdateVotes( iBaseID, iVoteTime, iYes, iNo )
{
	g_VoteLabel.Text = "Do you want to start base " + iBaseID + "? (" + iVoteTime + ")";
	g_VoteYes.Text = "1. Yes (" + iYes + ")";
	g_VoteNo.Text = "2. No (" + iNo + ")";
}

function Primary_Button_0()
{
	g_SelectPrimaryWeaponButton0.Colour = GUI_THEME_MAIN_COLOR;
	g_SelectPrimaryWeaponButton1.Colour = Colour( 0, 0, 0 );
	g_SelectPrimaryWeaponButton2.Colour = Colour( 0, 0, 0 );
	g_SelectPrimaryWeaponButton3.Colour = Colour( 0, 0, 0 );
	g_SelectPrimaryWeaponButton4.Colour = Colour( 0, 0, 0 );
	
	g_SelectPrimaryWeaponButton0.Alpha = 100;
	g_SelectPrimaryWeaponButton1.Alpha = 20;
	g_SelectPrimaryWeaponButton2.Alpha = 20;
	g_SelectPrimaryWeaponButton3.Alpha = 20;
	g_SelectPrimaryWeaponButton4.Alpha = 20;
	
	g_PrimaryWeapon = 255;
}

function Primary_Button_1()
{
	g_SelectPrimaryWeaponButton0.Colour = Colour( 0, 0, 0 );
	g_SelectPrimaryWeaponButton1.Colour = GUI_THEME_MAIN_COLOR;
	g_SelectPrimaryWeaponButton2.Colour = Colour( 0, 0, 0 );
	g_SelectPrimaryWeaponButton3.Colour = Colour( 0, 0, 0 );
	g_SelectPrimaryWeaponButton4.Colour = Colour( 0, 0, 0 );
	
	g_SelectPrimaryWeaponButton0.Alpha = 20;
	g_SelectPrimaryWeaponButton1.Alpha = 100;
	g_SelectPrimaryWeaponButton2.Alpha = 20;
	g_SelectPrimaryWeaponButton3.Alpha = 20;
	g_SelectPrimaryWeaponButton4.Alpha = 20;
	
	g_PrimaryWeapon = 5;
}

function Primary_Button_2()
{
	g_SelectPrimaryWeaponButton0.Colour = Colour( 0, 0, 0 );
	g_SelectPrimaryWeaponButton1.Colour = Colour( 0, 0, 0 );
	g_SelectPrimaryWeaponButton2.Colour = GUI_THEME_MAIN_COLOR;
	g_SelectPrimaryWeaponButton3.Colour = Colour( 0, 0, 0 );
	g_SelectPrimaryWeaponButton4.Colour = Colour( 0, 0, 0 );
	
	g_SelectPrimaryWeaponButton0.Alpha = 20;
	g_SelectPrimaryWeaponButton1.Alpha = 20;
	g_SelectPrimaryWeaponButton2.Alpha = 100;
	g_SelectPrimaryWeaponButton3.Alpha = 20;
	g_SelectPrimaryWeaponButton4.Alpha = 20;
	
	g_PrimaryWeapon = 6;
}

function Primary_Button_3()
{
	g_SelectPrimaryWeaponButton0.Colour = Colour( 0, 0, 0 );
	g_SelectPrimaryWeaponButton1.Colour = Colour( 0, 0, 0 );
	g_SelectPrimaryWeaponButton2.Colour = Colour( 0, 0, 0 );
	g_SelectPrimaryWeaponButton3.Colour = GUI_THEME_MAIN_COLOR;
	g_SelectPrimaryWeaponButton4.Colour = Colour( 0, 0, 0 );
	
	g_SelectPrimaryWeaponButton0.Alpha = 20;
	g_SelectPrimaryWeaponButton1.Alpha = 20;
	g_SelectPrimaryWeaponButton2.Alpha = 20;
	g_SelectPrimaryWeaponButton3.Alpha = 100;
	g_SelectPrimaryWeaponButton4.Alpha = 20;
	
	g_PrimaryWeapon = 4;
}

function Primary_Button_4()
{
	g_SelectPrimaryWeaponButton0.Colour = Colour( 0, 0, 0 );
	g_SelectPrimaryWeaponButton1.Colour = Colour( 0, 0, 0 );
	g_SelectPrimaryWeaponButton2.Colour = Colour( 0, 0, 0 );
	g_SelectPrimaryWeaponButton3.Colour = Colour( 0, 0, 0 );
	g_SelectPrimaryWeaponButton4.Colour = GUI_THEME_MAIN_COLOR;
	
	g_SelectPrimaryWeaponButton0.Alpha = 20;
	g_SelectPrimaryWeaponButton1.Alpha = 20;
	g_SelectPrimaryWeaponButton2.Alpha = 20;
	g_SelectPrimaryWeaponButton3.Alpha = 20;
	g_SelectPrimaryWeaponButton4.Alpha = 100;
	
	g_PrimaryWeapon = 7;
}

function Secondary_Button_0()
{
	g_SelectSecondaryWeaponButton0.Colour = GUI_THEME_MAIN_COLOR;
	g_SelectSecondaryWeaponButton1.Colour = Colour( 0, 0, 0 );
	g_SelectSecondaryWeaponButton2.Colour = Colour( 0, 0, 0 );
	
	g_SelectSecondaryWeaponButton0.Alpha = 100;
	g_SelectSecondaryWeaponButton1.Alpha = 20;
	g_SelectSecondaryWeaponButton2.Alpha = 20;
	
	g_SecondaryWeapon = 255;
}

function Secondary_Button_1()
{
	g_SelectSecondaryWeaponButton0.Colour = Colour( 0, 0, 0 );
	g_SelectSecondaryWeaponButton1.Colour = GUI_THEME_MAIN_COLOR;
	g_SelectSecondaryWeaponButton2.Colour = Colour( 0, 0, 0 );
	
	g_SelectSecondaryWeaponButton0.Alpha = 20;
	g_SelectSecondaryWeaponButton1.Alpha = 100;
	g_SelectSecondaryWeaponButton2.Alpha = 20;
	
	g_SecondaryWeapon = 2;
}

function Secondary_Button_2()
{
	g_SelectSecondaryWeaponButton0.Colour = Colour( 0, 0, 0 );
	g_SelectSecondaryWeaponButton1.Colour = Colour( 0, 0, 0 );
	g_SelectSecondaryWeaponButton2.Colour = GUI_THEME_MAIN_COLOR;
	
	g_SelectSecondaryWeaponButton0.Alpha = 20;
	g_SelectSecondaryWeaponButton1.Alpha = 20;
	g_SelectSecondaryWeaponButton2.Alpha = 100;
	
	g_SecondaryWeapon = 3;
}

function Additional_Button_0()
{
	g_SelectAdditionalWeaponButton0.Colour = GUI_THEME_MAIN_COLOR;
	g_SelectAdditionalWeaponButton1.Colour = Colour( 0, 0, 0 );
	g_SelectAdditionalWeaponButton2.Colour = Colour( 0, 0, 0 );
	if ( pSettings.BaseballBat ) g_SelectAdditionalWeaponButton3.Colour = Colour( 0, 0, 0 );
	
	g_SelectAdditionalWeaponButton0.Alpha = 100;
	g_SelectAdditionalWeaponButton1.Alpha = 20;
	g_SelectAdditionalWeaponButton2.Alpha = 20;
	if ( pSettings.BaseballBat ) g_SelectAdditionalWeaponButton3.Alpha = 20;
	
	g_AdditionalWeapon = 255;
}

function Additional_Button_1()
{
	g_SelectAdditionalWeaponButton0.Colour = Colour( 0, 0, 0 );
	g_SelectAdditionalWeaponButton1.Colour = GUI_THEME_MAIN_COLOR;
	g_SelectAdditionalWeaponButton2.Colour = Colour( 0, 0, 0 );
	if ( pSettings.BaseballBat ) g_SelectAdditionalWeaponButton3.Colour = Colour( 0, 0, 0 );
	
	g_SelectAdditionalWeaponButton0.Alpha = 20;
	g_SelectAdditionalWeaponButton1.Alpha = 100;
	g_SelectAdditionalWeaponButton2.Alpha = 20;
	if ( pSettings.BaseballBat ) g_SelectAdditionalWeaponButton3.Alpha = 20;
	
	g_AdditionalWeapon = 11;
}

function Additional_Button_2()
{
	g_SelectAdditionalWeaponButton0.Colour = Colour( 0, 0, 0 );
	g_SelectAdditionalWeaponButton1.Colour = Colour( 0, 0, 0 );
	g_SelectAdditionalWeaponButton2.Colour = GUI_THEME_MAIN_COLOR;
	if ( pSettings.BaseballBat ) g_SelectAdditionalWeaponButton3.Colour = Colour( 0, 0, 0 );
	
	g_SelectAdditionalWeaponButton0.Alpha = 20;
	g_SelectAdditionalWeaponButton1.Alpha = 20;
	g_SelectAdditionalWeaponButton2.Alpha = 100;
	if ( pSettings.BaseballBat ) g_SelectAdditionalWeaponButton3.Alpha = 20;
	
	g_AdditionalWeapon = 10;
}

function Additional_Button_3()
{
	g_SelectAdditionalWeaponButton0.Colour = Colour( 0, 0, 0 );
	g_SelectAdditionalWeaponButton1.Colour = Colour( 0, 0, 0 );
	g_SelectAdditionalWeaponButton2.Colour = Colour( 0, 0, 0 );
	if ( pSettings.BaseballBat ) g_SelectAdditionalWeaponButton3.Colour = GUI_THEME_MAIN_COLOR;
	
	g_SelectAdditionalWeaponButton0.Alpha = 20;
	g_SelectAdditionalWeaponButton1.Alpha = 20;
	g_SelectAdditionalWeaponButton2.Alpha = 20;
	if ( pSettings.BaseballBat ) g_SelectAdditionalWeaponButton3.Alpha = 100;
	
	g_AdditionalWeapon = 1;
}

function OK_Button()
{
	if ( g_PrimaryWeapon && g_SecondaryWeapon && g_AdditionalWeapon )
	{
		ShowMouseCursor( false );
		RestoreCamera();
		ToggleCameraMovement( true );
		g_pLocalPlayer.Frozen = false;
		g_SelectWeaponWindow.Visible = false;
		//UnbindKey( KEY_RETURN, BINDTYPE_DOWN, "OK_Button" );
		
		if ( pGame.IsRoundInProgress ) CallServerFunc( "basemode/server.nut", "GiveWeapons", g_pLocalPlayer, g_PrimaryWeapon, g_SecondaryWeapon, g_AdditionalWeapon );
	}
}

function ShowWeaponsSelectMenu()
{
	ShowMouseCursor( true );
	ToggleCameraMovement( false );
	//BindKey( KEY_RETURN, BINDTYPE_DOWN, "OK_Button" );
	
	if ( !g_SelectWeaponWindow )
	{
		g_SelectWeaponWindow = GUIWindow( VectorScreen( ScreenWidth/2-250, ScreenHeight/2-80 ), ScreenSize( 480, 170 ), "Please select your weapons:" );
		g_SelectWeaponWindow.Colour = Colour( 0, 0, 0 );
		g_SelectWeaponWindow.Titlebar = true;
		g_SelectWeaponWindow.Alpha = 100;
		g_SelectWeaponWindow.Visible = true;
		AddGUILayer( g_SelectWeaponWindow );
		
		g_SelectWeaponLabel1 = GUILabel( VectorScreen( 20, 20 ), ScreenSize( 150, 10 ), "Primary Weapon" );
		g_SelectWeaponLabel1.TextColour = Colour( 255, 255, 255 );
		g_SelectWeaponLabel1.Alpha = 200;
		g_SelectWeaponLabel1.Flags = FLAG_SHADOW;
		g_SelectWeaponLabel1.Visible = true;
		g_SelectWeaponLabel1.FontName = "Tahoma";
		g_SelectWeaponLabel1.FontSize = 8;
		g_SelectWeaponLabel1.TextAlignment = ALIGN_TOP_LEFT;
		g_SelectWeaponWindow.AddChild( g_SelectWeaponLabel1 );
		
		g_SelectWeaponLabel2 = GUILabel( VectorScreen( 175, 20 ), ScreenSize( 150, 10 ), "Secondary Weapon" );
		g_SelectWeaponLabel2.TextColour = Colour( 255, 255, 255 );
		g_SelectWeaponLabel2.Alpha = 200;
		g_SelectWeaponLabel2.Flags = FLAG_SHADOW;
		g_SelectWeaponLabel2.Visible = true;
		g_SelectWeaponLabel2.FontName = "Tahoma";
		g_SelectWeaponLabel2.FontSize = 8;
		//g_SelectWeaponLabel2.TextAlignment = ALIGN_TOP_CENTER;
		g_SelectWeaponWindow.AddChild( g_SelectWeaponLabel2 );
		
		g_SelectWeaponLabel3 = GUILabel( VectorScreen( 330, 20 ), ScreenSize( 150, 10 ), "Additional Weapon" );
		g_SelectWeaponLabel3.TextColour = Colour( 255, 255, 255 );
		g_SelectWeaponLabel3.Alpha = 200;
		g_SelectWeaponLabel3.Flags = FLAG_SHADOW;
		g_SelectWeaponLabel3.Visible = true;
		g_SelectWeaponLabel3.FontName = "Tahoma";
		g_SelectWeaponLabel3.FontSize = 8;
		//g_SelectWeaponLabel3.TextAlignment = ALIGN_MIDDLE_CENTER;
		g_SelectWeaponWindow.AddChild( g_SelectWeaponLabel3 );
		
		g_SelectPrimaryWeaponButton0 = GUIButton( VectorScreen( 10, 40 ), ScreenSize( 150, 10 ), "Empty" );
		g_SelectPrimaryWeaponButton0.TextColour = Colour( 255, 255, 255 );
		g_SelectPrimaryWeaponButton0.Colour = Colour( 0, 0, 0 );
		g_SelectPrimaryWeaponButton0.Alpha = 20;
		//g_SelectPrimaryWeaponButton0.Flags = FLAG_SHADOW;
		g_SelectPrimaryWeaponButton0.Visible = true;
		g_SelectPrimaryWeaponButton0.FontName = "Tahoma";
		g_SelectPrimaryWeaponButton0.FontSize = 8;
		g_SelectPrimaryWeaponButton0.SetCallbackFunc( Primary_Button_0 );
		g_SelectWeaponWindow.AddChild( g_SelectPrimaryWeaponButton0 );
		
		g_SelectPrimaryWeaponButton1 = GUIButton( VectorScreen( 10, 60 ), ScreenSize( 150, 10 ), "AK-47 / " + pSettings.AKAmmo.tostring() + " ammo" );
		g_SelectPrimaryWeaponButton1.TextColour = Colour( 255, 255, 255 );
		g_SelectPrimaryWeaponButton1.Colour = Colour( 0, 0, 0 );
		g_SelectPrimaryWeaponButton1.Alpha = 20;
		//g_SelectPrimaryWeaponButton1.Flags = FLAG_SHADOW;
		g_SelectPrimaryWeaponButton1.Visible = true;
		g_SelectPrimaryWeaponButton1.FontName = "Tahoma";
		g_SelectPrimaryWeaponButton1.FontSize = 8;
		g_SelectPrimaryWeaponButton1.SetCallbackFunc( Primary_Button_1 );
		g_SelectWeaponWindow.AddChild( g_SelectPrimaryWeaponButton1 );
		
		g_SelectPrimaryWeaponButton2 = GUIButton( VectorScreen( 10, 80 ), ScreenSize( 150, 10 ), "M16 / " + pSettings.M16Ammo.tostring() + " ammo" );
		g_SelectPrimaryWeaponButton2.TextColour = Colour( 255, 255, 255 );
		g_SelectPrimaryWeaponButton2.Colour = Colour( 0, 0, 0 );
		g_SelectPrimaryWeaponButton2.Alpha = 20;
		//g_SelectPrimaryWeaponButton2.Flags = FLAG_SHADOW;
		g_SelectPrimaryWeaponButton2.Visible = true;
		g_SelectPrimaryWeaponButton2.FontName = "Tahoma";
		g_SelectPrimaryWeaponButton2.FontSize = 8;
		g_SelectPrimaryWeaponButton2.SetCallbackFunc( Primary_Button_2 );
		g_SelectWeaponWindow.AddChild( g_SelectPrimaryWeaponButton2 );
		
		g_SelectPrimaryWeaponButton3 = GUIButton( VectorScreen( 10, 100 ), ScreenSize( 150, 10 ), "Shotgun / " + pSettings.ShotgunAmmo.tostring() + " ammo" );
		g_SelectPrimaryWeaponButton3.TextColour = Colour( 255, 255, 255 );
		g_SelectPrimaryWeaponButton3.Colour = Colour( 0, 0, 0 );
		g_SelectPrimaryWeaponButton3.Alpha = 20;
		//g_SelectPrimaryWeaponButton3.Flags = FLAG_SHADOW;
		g_SelectPrimaryWeaponButton3.Visible = true;
		g_SelectPrimaryWeaponButton3.FontName = "Tahoma";
		g_SelectPrimaryWeaponButton3.FontSize = 8;
		g_SelectPrimaryWeaponButton3.SetCallbackFunc( Primary_Button_3 );
		g_SelectWeaponWindow.AddChild( g_SelectPrimaryWeaponButton3 );
		
		g_SelectPrimaryWeaponButton4 = GUIButton( VectorScreen( 10, 120 ), ScreenSize( 150, 10 ), "Rifle / " + pSettings.RifleAmmo.tostring() + " ammo" );
		g_SelectPrimaryWeaponButton4.TextColour = Colour( 255, 255, 255 );
		g_SelectPrimaryWeaponButton4.Colour = Colour( 0, 0, 0 );
		g_SelectPrimaryWeaponButton4.Alpha = 20;
		//g_SelectPrimaryWeaponButton4.Flags = FLAG_SHADOW;
		g_SelectPrimaryWeaponButton4.Visible = true;
		g_SelectPrimaryWeaponButton4.FontName = "Tahoma";
		g_SelectPrimaryWeaponButton4.FontSize = 8;
		g_SelectPrimaryWeaponButton4.SetCallbackFunc( Primary_Button_4 );
		g_SelectWeaponWindow.AddChild( g_SelectPrimaryWeaponButton4 );
		
		g_SelectSecondaryWeaponButton0 = GUIButton( VectorScreen( 165, 40 ), ScreenSize( 150, 10 ), "Empty" );
		g_SelectSecondaryWeaponButton0.TextColour = Colour( 255, 255, 255 );
		g_SelectSecondaryWeaponButton0.Colour = Colour( 0, 0, 0 );
		g_SelectSecondaryWeaponButton0.Alpha = 20;
		//g_SelectSecondaryWeaponButton0.Flags = FLAG_SHADOW;
		g_SelectSecondaryWeaponButton0.Visible = true;
		g_SelectSecondaryWeaponButton0.FontName = "Tahoma";
		g_SelectSecondaryWeaponButton0.FontSize = 8;
		g_SelectSecondaryWeaponButton0.SetCallbackFunc( Secondary_Button_0 );
		g_SelectWeaponWindow.AddChild( g_SelectSecondaryWeaponButton0 );
		
		g_SelectSecondaryWeaponButton1 = GUIButton( VectorScreen( 165, 60 ), ScreenSize( 150, 10 ), "Colt 45 / " + pSettings.ColtAmmo.tostring() + " ammo" );
		g_SelectSecondaryWeaponButton1.TextColour = Colour( 255, 255, 255 );
		g_SelectSecondaryWeaponButton1.Colour = Colour( 0, 0, 0 );
		g_SelectSecondaryWeaponButton1.Alpha = 20;
		//g_SelectSecondaryWeaponButton1.Flags = FLAG_SHADOW;
		g_SelectSecondaryWeaponButton1.Visible = true;
		g_SelectSecondaryWeaponButton1.FontName = "Tahoma";
		g_SelectSecondaryWeaponButton1.FontSize = 8;
		g_SelectSecondaryWeaponButton1.SetCallbackFunc( Secondary_Button_1 );
		g_SelectWeaponWindow.AddChild( g_SelectSecondaryWeaponButton1 );	
		
		g_SelectSecondaryWeaponButton2 = GUIButton( VectorScreen( 165, 80 ), ScreenSize( 150, 10 ), "UZI / " + pSettings.UZIAmmo.tostring() + " ammo" );
		g_SelectSecondaryWeaponButton2.TextColour = Colour( 255, 255, 255 );
		g_SelectSecondaryWeaponButton2.Colour = Colour( 0, 0, 0 );
		g_SelectSecondaryWeaponButton2.Alpha = 20;
		//g_SelectSecondaryWeaponButton2.Flags = FLAG_SHADOW;
		g_SelectSecondaryWeaponButton2.Visible = true;
		g_SelectSecondaryWeaponButton2.FontName = "Tahoma";
		g_SelectSecondaryWeaponButton2.FontSize = 8;
		g_SelectSecondaryWeaponButton2.SetCallbackFunc( Secondary_Button_2 );
		g_SelectWeaponWindow.AddChild( g_SelectSecondaryWeaponButton2 );
		
		g_SelectAdditionalWeaponButton0 = GUIButton( VectorScreen( 320, 40 ), ScreenSize( 150, 10 ), "Empty" );
		g_SelectAdditionalWeaponButton0.TextColour = Colour( 255, 255, 255 );
		g_SelectAdditionalWeaponButton0.Colour = Colour( 0, 0, 0 );
		g_SelectAdditionalWeaponButton0.Alpha = 20;
		//g_SelectAdditionalWeaponButton0.Flags = FLAG_SHADOW;
		g_SelectAdditionalWeaponButton0.Visible = true;
		g_SelectAdditionalWeaponButton0.FontName = "Tahoma";
		g_SelectAdditionalWeaponButton0.FontSize = 8;
		g_SelectAdditionalWeaponButton0.SetCallbackFunc( Additional_Button_0 );
		g_SelectWeaponWindow.AddChild( g_SelectAdditionalWeaponButton0 );
		
		g_SelectAdditionalWeaponButton1 = GUIButton( VectorScreen( 320, 60 ), ScreenSize( 150, 10 ), "Grenade / " + pSettings.GrenadeAmmo.tostring() + " pcs." );
		g_SelectAdditionalWeaponButton1.TextColour = Colour( 255, 255, 255 );
		g_SelectAdditionalWeaponButton1.Colour = Colour( 0, 0, 0 );
		g_SelectAdditionalWeaponButton1.Alpha = 20;
		//g_SelectAdditionalWeaponButton1.Flags = FLAG_SHADOW;
		g_SelectAdditionalWeaponButton1.Visible = true;
		g_SelectAdditionalWeaponButton1.FontName = "Tahoma";
		g_SelectAdditionalWeaponButton1.FontSize = 8;
		g_SelectAdditionalWeaponButton1.SetCallbackFunc( Additional_Button_1 );
		g_SelectWeaponWindow.AddChild( g_SelectAdditionalWeaponButton1 );
		
		g_SelectAdditionalWeaponButton2 = GUIButton( VectorScreen( 320, 80 ), ScreenSize( 150, 10 ), "Molotov / " + pSettings.MolotovAmmo.tostring() + " pcs." );
		g_SelectAdditionalWeaponButton2.TextColour = Colour( 255, 255, 255 );
		g_SelectAdditionalWeaponButton2.Colour = Colour( 0, 0, 0 );
		g_SelectAdditionalWeaponButton2.Alpha = 20;
		//g_SelectAdditionalWeaponButton2.Flags = FLAG_SHADOW;
		g_SelectAdditionalWeaponButton2.Visible = true;
		g_SelectAdditionalWeaponButton2.FontName = "Tahoma";
		g_SelectAdditionalWeaponButton2.FontSize = 8;
		g_SelectAdditionalWeaponButton2.SetCallbackFunc( Additional_Button_2 );
		g_SelectWeaponWindow.AddChild( g_SelectAdditionalWeaponButton2 );
		
		if ( pSettings.BaseballBat )
		{
			g_SelectAdditionalWeaponButton3 = GUIButton( VectorScreen( 320, 100 ), ScreenSize( 150, 10 ), "Baseball Bat" );
			g_SelectAdditionalWeaponButton3.TextColour = Colour( 255, 255, 255 );
			g_SelectAdditionalWeaponButton3.Colour = Colour( 0, 0, 0 );
			g_SelectAdditionalWeaponButton3.Alpha = 20;
			//g_SelectAdditionalWeaponButton3.Flags = FLAG_SHADOW;
			g_SelectAdditionalWeaponButton3.Visible = true;
			g_SelectAdditionalWeaponButton3.FontName = "Tahoma";
			g_SelectAdditionalWeaponButton3.FontSize = 8;
			g_SelectAdditionalWeaponButton3.SetCallbackFunc( Additional_Button_3 );
			g_SelectWeaponWindow.AddChild( g_SelectAdditionalWeaponButton3 );
		}
		
		g_SelectOKButton = GUIButton( VectorScreen( 10, 145 ), ScreenSize( 460, 10 ), "OK" );
		g_SelectOKButton.TextColour = Colour( 255, 255, 255 );
		g_SelectOKButton.Colour = Colour( 0, 0, 0 );
		g_SelectOKButton.Alpha = 20;
		//g_SelectOKButton.Flags = FLAG_SHADOW;
		g_SelectOKButton.Visible = true;
		g_SelectOKButton.FontName = "Tahoma";
		g_SelectOKButton.FontSize = 8;
		g_SelectOKButton.SetCallbackFunc( OK_Button );
		g_SelectWeaponWindow.AddChild( g_SelectOKButton );
	}
	else g_SelectWeaponWindow.Visible = true;
}

function ShowHelpWindow( bShow )
{
	if ( bShow.tointeger() )
	{
		if ( !g_HelpWindow && bShow.tointeger())
		{
			g_HelpWindow = GUIWindow( VectorScreen( ScreenWidth/2-300, ScreenHeight/2-150 ), ScreenSize( 600, 280 ), "Basemode help" );
			g_HelpWindow.Colour = Colour( 0, 0, 0 );
			g_HelpWindow.Titlebar = true;
			g_HelpWindow.Alpha = 200;
			g_HelpWindow.Visible = true;
			AddGUILayer( g_HelpWindow );
			
			g_HelpMemo = GUIMemobox( VectorScreen( 5, 0 ), ScreenSize( 590, 280 ));
			g_HelpMemo.TextColour = Colour( 255, 255, 255 );
			g_HelpMemo.FontSize = 9;
			g_HelpMemo.Lines = 19;
			g_HelpMemo.Flags = FLAG_COLOURING;
			
			g_HelpMemo.AddLine("[#ffff00]Goals:");
			g_HelpMemo.AddLine("Defending team have to defend the marker for 8 minutes or kill attackers.");
			g_HelpMemo.AddLine("Attacking team have 8 mins to capture the base or kill defenders.");
			g_HelpMemo.AddLine("");
			g_HelpMemo.AddLine("[#ffff00]Client commands:");
			g_HelpMemo.AddLine("[#00ff00]/fix[#ffffff] - hide mouse cursor and restore camera");
			g_HelpMemo.AddLine("[#00ff00]/fix2[#ffffff] - show mouse cursor and restore camera");
			g_HelpMemo.AddLine("[#00ff00]/radio <id 0-8/off>[#ffffff] - turn on/off the radio");
			g_HelpMemo.AddLine("[#00ff00]/sfx <on/off>[#ffffff] - turn on/off sound effects");
			g_HelpMemo.AddLine("");
			g_HelpMemo.AddLine("[#ffff00]Keybinds:");
			g_HelpMemo.AddLine("[#00ff00]Y[#ffffff] - Open teamchat.");
			g_HelpMemo.AddLine("[#00ff00]Enter[#ffffff] - Send team message/close teamchat.");
			g_HelpMemo.AddLine("[#00ff00]1[#ffffff] - [Team] Enemy spotted!");
			g_HelpMemo.AddLine("[#00ff00]2[#ffffff] - [Team] Need backup!");
			g_HelpMemo.AddLine("[#00ff00]3[#ffffff] - [Team] Follow me!");
			g_HelpMemo.AddLine("[#00ff00]4[#ffffff] - [Team] Incoming!");
			g_HelpMemo.AddLine("[#00ff00]5[#ffffff] - [Team] Go, go, go!");
			g_HelpMemo.AddLine("[#00ff00]6[#ffffff] - [Team] Roger that!");

			g_HelpWindow.AddChild( g_HelpMemo );
		}
		else g_HelpWindow.Visible = true;
	}
	else if ( g_HelpWindow ) g_HelpWindow.Visible = false;
}

function UpdateTeamNames( szTeam1Name, szTeam2Name )
{
	g_Team1StatsLabel.Text = szTeam1Name;
	g_Team2StatsLabel.Text = szTeam2Name;
}

function UpdateSettings( ColtAmmo, UZIAmmo, ShotgunAmmo, AKAmmo, M16Ammo, RifleAmmo, MolotovAmmo, GrenadeAmmo, BaseballBat, MaxPlayers, AFKSlapTime )
{
	pSettings.ColtAmmo = ColtAmmo;
	pSettings.UZIAmmo = UZIAmmo;
	pSettings.ShotgunAmmo = ShotgunAmmo;
	pSettings.AKAmmo = AKAmmo;
	pSettings.M16Ammo = M16Ammo;
	pSettings.RifleAmmo = RifleAmmo;
	pSettings.MolotovAmmo = MolotovAmmo;
	pSettings.GrenadeAmmo = GrenadeAmmo;
	pSettings.BaseballBat = BaseballBat;
	pSettings.MaxPlayers = MaxPlayers;
	pSettings.AFKSlapTime = AFKSlapTime;
}

function UpdateScores( iTeam1Score, iTeam2Score )
{
	if ( g_ScoreLabel ) g_ScoreLabel.Text = iTeam1Score.tostring() + ":" + iTeam2Score.tostring();
}

function FormatTime( iSecconds )
{
	local iMins = 0;
	local iSecs = 0;
	
	iMins = iSecconds / 60;
	iSecs = iSecconds - ( iMins * 60 );
	
	if ( iSecs < 10 ) return iMins + ":0" + iSecs;
	return iMins + ":" + iSecs;
}

function UpdateClock()
{
	local iHours, iMins;
	
	if ( date().hour < 10 ) iHours = "0" + date().hour;
	else iHours = date().hour;
	
	if ( date().min < 10 ) iMins = "0" + date().min;
	else iMins = date().min;
	
	g_TimeLabel.Text = iHours + ":" + iMins;
}

function SetSpawnClass( szName )
{
	if ( !g_SpawnScreenLabel )
	{
		g_SpawnScreenLabel = GUILabel( VectorScreen( ScreenWidth/2, ScreenHeight-50 ), ScreenSize( 0, 0 ), " " );
		//g_SpawnScreenLabel.TextColour = Colour( 255, 255, 255 );
		g_SpawnScreenLabel.Flags = FLAG_COLOURING;
		g_SpawnScreenLabel.Visible = true;
		g_SpawnScreenLabel.FontName = "Veranda";
		g_SpawnScreenLabel.FontSize = 16;
		g_SpawnScreenLabel.FontTags = TAG_BOLD;
		g_SpawnScreenLabel.TextAlignment = ALIGN_MIDDLE_CENTER;
		AddGUILayer( g_SpawnScreenLabel );
	}

	if ( !szName ) g_SpawnScreenLabel.Visible = false;
	else
	{
		g_SpawnScreenLabel.Text = szName;
		g_SpawnScreenLabel.Visible = true;
	}
}

function TimeProcess()
{
	if ( pGame.IsRoundInProgress )
	{
		if ( pGame.RoundTime > 0 ) pGame.RoundTime--;
		g_TimeLabel.Text = FormatTime( pGame.RoundTime );
		
		if ( pGame.CountdownTime > 1 )
		{
			PlayFrontEndSound( 147 );
			pGame.CountdownTime--;
			
			BigMessage( "~y~" + pGame.CountdownTime.tostring(), 1000, 3 )
		}
		else if ( pGame.CountdownTime == 1 )
		{
			PlayFrontEndSound( 148 );
			pGame.CountdownTime = 0;
			ShowWeaponsSelectMenu();
			SetHUDEnabled( true );
			RestoreCamera();
			StopFrontEndTrack();
			
			BigMessage( "Fight!", 1500, 3 )
		}
		
		if (( pSettings.AFKSlapTime ) && ( g_iLastMoveTime - pGame.RoundTime  > pSettings.AFKSlapTime ) && ( pGame.IsAttacker ) && ( g_isAFK )) Slap();
	}
	else UpdateClock();
}

function Slap()
{	
	if ( !g_pLocalPlayer.Vehicle )
	{
		local vPosition = g_pLocalPlayer.Pos;
		if ( g_pLocalPlayer.Health > 5 )
		{
			g_pLocalPlayer.Health -= 5;
			g_pLocalPlayer.Pos = Vector( vPosition.x, vPosition.y, vPosition.z + 2.0 );
		}
		else CallServerFunc( "basemode/server.nut", "onM16PlayerKill", g_pLocalPlayer, g_pLocalPlayer, WEP_AFK_KILLER );
	}
	else if ( g_pLocalPlayer.Name == g_pLocalPlayer.Vehicle.Driver.Name )
	{
		if ( g_pLocalPlayer.Health > 5 ) g_pLocalPlayer.Health -= 5;
		else CallServerFunc( "basemode/server.nut", "onM16PlayerKill", g_pLocalPlayer, g_pLocalPlayer, WEP_AFK_KILLER );
	}
	PlayFrontEndSound( 53 );
	Message( "[#ff0000][Slap!] [#00ff00]Go the base showed on radar!", Colour( 255, 255, 255 ));
}

function onBaseStart( iRoundTime, iMarkerID, isAttacker )
{
	ClearMessages();
	if ( !SETTING_MUTE ) PlayFrontEndTrack( 16 );
	pGame.IsAttacker = isAttacker;
	g_iGameState = 1;
	g_pLocalPlayer.Frozen = true;
	g_pMarker = FindSphere( iMarkerID );
	
	if ( GetHUDEnabled() ) SetHUDEnabled( false );
	FadeCamera( 2, true );
	pGame.CountdownTime = 6;
	
	if ( pGame.IsVotingInProgress ) EndVote();
	
	g_RandomRadio = rand() % ( 0 - 8 );
	pGame.RoundTime = iRoundTime;
	g_iRoundStartTime = pGame.RoundTime;
	g_iLastMoveTime = pGame.RoundTime - 10;
	pGame.IsRoundInProgress = true;
}

function onBaseEnd( iTeam1Score, iTeam2Score, isSpawned, isWinner )
{
	UpdateScores( iTeam1Score, iTeam2Score );
	StopFrontEndTrack();
	g_iGameState = 0;
	pGame.RoundTime = 0;
	pGame.CountdownTime = 0;
	pGame.IsAttacker = true;
	pGame.IsRoundInProgress = false;
	
	if ( isWinner && !SETTING_MUTE ) PlayFrontEndTrack( 93 );

	if ( isSpawned )
	{
		FadeCamera( 2, true );
		RestoreCamera();
	}
	
	if ( g_SelectWeaponWindow.Visible )
	{
		ShowMouseCursor( false );
		ToggleCameraMovement( true );
		g_SelectWeaponWindow.Visible = false;
		//UnbindKey( KEY_RETURN, BINDTYPE_DOWN, "OK_Button" );
	}
	
	g_pLocalPlayer.Frozen = false;
	if ( IsMouseCursorShowing() ) ShowMouseCursor( false );
	g_SelectWeaponWindow.Visible = false;
}

function UpdateCaptureTime( iTime )
{
	if ( iTime == 0 ) g_CaptureWindow.Visible = false;
	else
	{
		g_CaptureWindow.Visible = true;
		g_CaptureBar.Value = iTime;
	}
}

local pi = 3.141;
//local iPlayerID = g_pLocalPlayer.ID;
local pCurrentPlayer = g_pLocalPlayer;
local vPlayerPos;

function SpectatePlayer_Process()
{
	if (( !pCurrentPlayer ) || ( pCurrentPlayer == g_pLocalPlayer ))
	{
		
	}
	else if (( pCurrentPlayer.Team == g_pLocalPlayer.Team ) && ( pCurrentPlayer != g_pLocalPlayer ))
	{
		local vCameraPointAt = pCurrentPlayer.Pos;
		local vCameraPosition = pCurrentPlayer.Pos;
		
		local rad = ( pCurrentPlayer.Angle - 90.0 ) * ( pi / 180.0 );
		
		vCameraPosition.x = vCameraPosition.x + 2.5 * cos( rad );
		vCameraPosition.y = vCameraPosition.y + 2.5 * sin( rad );
		vCameraPosition.z += 1.5;
		
		vCameraPointAt.z += 1.0;

		SetCameraMatrix( vCameraPosition, vCameraPointAt );
		
		vCameraPointAt.z -= 25.0;
		g_pLocalPlayer.Pos = vCameraPointAt;
	}
}

function SpectatePlayer_Start()
{
	if ( pCurrentPlayer == g_pLocalPlayer )
	{
		g_pLocalPlayer.Pos = vPlayerPos;
		g_iGameState = 2;
	}
	else
	{
		vPlayerPos = g_pLocalPlayer.Pos;
		g_iGameState = 3;
	}
}

function SpectatePlayer_Stop( )
{
	
	//RestoreCamera();
}

local iCurrentPlayerID = 0;

function SpectatePlayer_Next()
{
	Message("Next");
	
	
	/*do
	{
		iCurrentPlayerID++;
		Message( "Found wrong player: " + iCurrentPlayerID.tostring());
	}
	while (( iCurrentPlayerID < pSettings.MaxPlayers ) && ( FindPlayer( iCurrentPlayerID ).Team != g_pLocalPlayer.Team ) && ( !FindPlayer( iCurrentPlayerID ).Immune ))
	*/

	Message( "Found player: " + iCurrentPlayerID.tostring());
	
	Message("Next_End");
	/*if ( iCurrentPlayerID < pSettings.MaxPlayers )
	{		
		iCurrentPlayerID++;
		
		local pPlayer = FindPlayer( iCurrentPlayerID );
		
		if ( pPlayer == g_pLocalPlayer )
		{
			pCurrentPlayer = pPlayer;
			vPlayerPos.z += 0.2;
			g_pLocalPlayer.Pos = vPlayerPos;
			RestoreCamera();
		}
		else if ( pPlayer.Team == g_pLocalPlayer.Team )
		{
			Message(iCurrentPlayerID.tostring());
			pCurrentPlayer = pPlayer;
		}
	}*/
}

function SpectatePlayer_Previous()
{
	if ( iCurrentPlayerID > 0 )
	{		
		iCurrentPlayerID--;
		
		local pPlayer = FindPlayer( iCurrentPlayerID );
		
		if ( pPlayer == g_pLocalPlayer )
		{
			pCurrentPlayer = pPlayer;
			vPlayerPos.z += 0.2;
			g_pLocalPlayer.Pos = vPlayerPos;
			RestoreCamera();
		}
		else if ( pPlayer.Team == g_pLocalPlayer.Team )
		{
			Message(iCurrentPlayerID.tostring());
			pCurrentPlayer = pPlayer;
		}
	}
}

function GetPartReasonFromID( iReasonID )
{
	switch ( iReasonID )
	{
		case PARTREASON_DISCONNECTED:
			return "Disconnected";
		case PARTREASON_CRASHED:
			return "Crashed";
		case PARTREASON_TIMEOUT:
			return "Connection Timeout";
		case PARTREASON_KICKED:
			return "Kicked";
		case PARTREASON_BANNED:
			return "Banned";
	}
}

function onPlayerJoin( pPlayer )
{
	Message( "* " + pPlayer.Name + " has joined the game! (ID: " + pPlayer.ID + ")", Colour( 255, 255, 0 ));
	if ( !SETTING_MUTE ) PlayFrontEndTrack( 191 );
	
	return 0;
}

function onPlayerPart( pPlayer, iReasonID )
{
	if ( !SETTING_MUTE ) PlayFrontEndSound( 156 );
	if ( !pPlayer.Immune ) Message( "* " + pPlayer.Name + " has left the game. (ID: " + pPlayer.ID + " | Reason: " + GetPartReasonFromID( iReasonID ) + " | Health: " + pPlayer.Health + ")", Colour( 255, 255, 0 ));
	else Message( "* " + pPlayer.Name + " has left the game. (ID: " + pPlayer.ID + " | Reason: " + GetPartReasonFromID( iReasonID ) + ")", Colour( 255, 255, 0 ));
	
	return 0;
}

function onClientEnteredVehicle ( pVehicle, iSeat )
{
	if ( g_OwnRadio != 255 ) PlayFrontEndTrack( g_OwnRadio );
	else PlayFrontEndTrack( g_RandomRadio );
}

function onClientExitingVehicle( pVehicle )
{
	StopFrontEndTrack();
}

function onClientVehicleShot( pVehicle, pPlayer, iWeaponID )
{	
	if ( iWeaponID == 6 )
	{
		if ( pVehicle.Health > 20 )
		{
			CallServerFunc( "basemode/server.nut", "onM16VehicleShot", pPlayer, pVehicle, iWeaponID );
			return 0;
		}
		else return 1;
	}
	else return 1;
}

function onClientShot( pPlayer, iWeaponID, iBodypartID )
{
	if (( !g_pLocalPlayer.Immune ) && ( pPlayer.Team != g_pLocalPlayer.Team ) && ( g_pLocalPlayer.Health >= 1 ))
	{
		if ( iWeaponID == 2 ) 
		{
			if ( g_pLocalPlayer.Health > 20 ) g_pLocalPlayer.Health -= 20;
			else CallServerFunc( "basemode/server.nut", "onM16PlayerKill", pPlayer, g_pLocalPlayer, 2 );
			
			return 0;
		}
		else if ( iWeaponID == 6 )
		{
			if ( g_pLocalPlayer.Health > 7 ) g_pLocalPlayer.Health -= 7;
			else CallServerFunc( "basemode/server.nut", "onM16PlayerKill", pPlayer, g_pLocalPlayer, 6 );
			
			return 0;
		}
		else if ( iWeaponID == 7 )
		{
			if ( g_pLocalPlayer.Health > 60 ) g_pLocalPlayer.Health -= 60;
			else CallServerFunc( "basemode/server.nut", "onM16PlayerKill", pPlayer, g_pLocalPlayer, 7 );
			
			return 0;
		}
	}
}

function onClientDeath( pKiller, iWeapon, iBodypart )
{
	if ( g_SelectWeaponWindow ) g_SelectWeaponWindow.Visible = false;
}

function onGlassSmash( fDamage, fX, fY, fZ, fHitX, fHitY, fHitZ, bExplosion )
{
	return 0;
}

g_fZangle <- 0.0;
g_fRadius <- 50.0;

function onClientRender()
{
	if (( pGame.CountdownTime > 0 ) && ( g_pMarker ))
	{
		local vCameraPosition = g_pMarker.Pos;
		local vCameraPointAt = g_pMarker.Pos;
		
		vCameraPosition.x = vCameraPosition.x + g_fRadius * cos(g_fZangle);
		vCameraPosition.y = vCameraPosition.y + g_fRadius * sin(g_fZangle);
		vCameraPosition.z += 30.0;
		
		
		g_fZangle+= 0.03;

		SetCameraMatrix( vCameraPosition, vCameraPointAt );
	}
	
	// Speedo
	if ( g_pLocalPlayer.Vehicle )
	{
		local pVehicle = g_pLocalPlayer.Vehicle;
		local vVelocity = pVehicle.Velocity;
		local speed = (sqrt((vVelocity.x*vVelocity.x)+(vVelocity.y*vVelocity.y)+(vVelocity.z*vVelocity.z)) * 180 * 1.60 ).tointeger();
		//Speed = sqrt(vehicle.Velocity.x*vehicle.Velocity.x + vehicle.Velocity.y * vehicle.Velocity.y + vehicle.Velocity.z * vehicle.Velocity.z) * 50 * 3.6,
		
		g_HealthBar.Value = pVehicle.Health.tointeger();
		g_SpeedoLabel.Text = speed.tostring() + " KM/h";
	}
	else
	{
		g_HealthBar.Value = g_pLocalPlayer.Health.tointeger() * 10;
		g_SpeedoLabel.Text = "";		
	}

	// Mouse fursor fix
	/*if (( g_SelectWeaponWindow ) && ( g_SelectWeaponWindow.Visible ) && ( !IsMouseCursorShowing() )) ShowMouseCursor( true );
	else if (( !g_SelectWeaponWindow.Visible ) && ( !IsMouseCursorShowing() )) ShowMouseCursor( false );*/
	
	//if (( g_pLocalPlayer.Immune ) && ( g_iGameState == 1 )) SpectatePlayer_Start();
	//if (( g_pLocalPlayer.Immune ) && ( g_iGameState == 3 )) SpectatePlayer_Process();
	//if (( !g_pLocalPlayer.Immune ) && ( g_iGameState == 3 )) SpectatePlayer_End();
	
	return 1;
}

function onClientKeyStateChange( iOldKeys, iNewKeys )
{
	if ( iNewKeys > 0 ) g_isAFK = false;
	else g_isAFK = true;
	g_iLastMoveTime = pGame.RoundTime;
}

function onClientCommand( szCommand, szText )
{
	//if ( szCommand == "track" ) PlayFrontEndTrack( szText.tointeger() );
	//else if ( szCommand == "shake" ) ShakeCamera( szText.tointeger() );
	//else if ( szCommand == "snd" ) i = szText.tointeger();
	
	if ( szCommand == "fix" )
	{
		ShowMouseCursor( false );
		RestoreCamera();
		ToggleCameraMovement( true );
	}
	
	else if ( szCommand == "fix2" )
	{
		ShowMouseCursor( true );
		RestoreCamera();
		ToggleCameraMovement( true );
	}
	else if (( szCommand == "wep" ) || ( szCommand == "weps" ) || ( szCommand == "weapons" ))
	{
		if ( !pGame.IsRoundInProgress ) return 0;
		if ( g_iRoundStartTime - pGame.RoundTime <= 30 ) ShowWeaponsSelectMenu();
		else Message( "This command is avaliable only for 30secs after start.", Colour( 255, 255, 0 ));
	}
	else if ( szCommand == "radio" )
	{
		if ( !szText ) Message( "Use [#00ff00]/radio <1-8>[#ffff00] or [#00ff00]/radio off", Colour( 255, 255, 0 ));
		else if ( szText == "off" ) { StopFrontEndTrack(); g_OwnRadio = 255; }
		else
		{
			local iFM = szText.tointeger();
			if (( iFM <= 8 ) && ( iFM >= 0 ))
			{
				PlayFrontEndTrack( iFM );
				g_OwnRadio = iFM;
			}
		}
	}
	else if ( szCommand == "sfx" )
	{
		if ( !szText ) Message( "Use [#00ff00]/sfx on [#ffff00]or [#00ff00]/sfx off", Colour( 255, 255, 0 ));
		else if ( szText == "on" ) SETTING_MUTE = false;
		else if ( szText == "off" ) SETTING_MUTE = true;
	}
	return 1;
}

function onClientRequestClass( pSpawn )
{
	UpdateSpawnScreenScene( pSpawn );
	return 1;
}

function onClientSpawn( pSpawn )
{
	EndSpawnScreenScene( pSpawn );
	return 1;
}

// #################################################################
// 						Spawn Screen
// #################################################################

function StartSpawnScreenScene()
{
}

function EndSpawnScreenScene( pSpawn )
{
	vPlayerPos = g_pLocalPlayer.Pos;
	SetHUDEnabled( true );
	ClearMessages();
	//StopFrontEndTrack();
}

function UpdateSpawnScreenScene( pSpawn )
{
	SetHUDEnabled( false );
	FadeCamera( 1, true );
	ShakeCamera( 200 );
	PlayFrontEndSound( 94 );
	
	CallServerFunc( "basemode/server.nut", "onPlayerRequestClass", g_pLocalPlayer, pSpawn.Team );
}

// #################################################################