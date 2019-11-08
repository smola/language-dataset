main()
{
    maps\mp\_load::main();

    game["allies"] = "marines";
    game["axis"] = "opfor";
    game["attackers"] = "allies";
    game["defenders"] = "axis";
    game["allies_soldiertype"] = "desert";
    game["axis_soldiertype"] = "desert";

maps\mp\mp_the_extreme_v2_motions::init();


    setdvar("r_specularcolorscale","5");
    setdvar("r_glowbloomintensity0",".25");
    setdvar("r_glowbloomintensity1",".25");
    setdvar("r_glowskybleedintensity0",".3");
    setdvar("compassmaxrange","1800");
    
    setdvar("jump_slowdownenable","0");
    setDvar("bg_fallDamageMaxHeight", 9999);
    setDvar("bg_fallDamageMinHeight", 9998);

    thread teleporter();
    thread bouncer("speed", 13);
    thread bouncer("speed2", 14); 
    thread bouncer("speed3", 5);
    thread bouncer("speed4", 9);
    thread bouncer("speed8", 7);
    thread bouncer("speed7", 2);

    bounce  = getEntArray("speed5", "targetname");
    
    for(i = 0;i < bounce.size;i++)
        bounce[i] thread bounce();
}
addletter(which)
{
   letter = getent(which, "targetname");

    for(;;)
    {
        letter movez(20, 1, 0.3, 0.3);
        letter waittill("movedone");
        letter  movez(-20, 1, 0.3, 0.3);
        letter waittill("movedone");
    }
}
intertopreset()
{
    reset = getent("interreset", "targetname");

    for(;;)
    {
        reset waittill("trigger", who);

        if(isdefined(who.int_bot) && who.int_bot == true)
            who.int_bot = false;
    }
}
interbottomland()
{
    bottom = getent("interbot", "targetname");

    for(;;)
    {
        bottom waittill("trigger", who);

        if(isdefined(who.int_bot) && !who.int_bot)
            who.int_bot = true;
    }
}
intertopland()
{
    top = getent("intertop", "targetname");

    for(;;)
    {
        top waittill("trigger", who);

        if(isDefined(who.int_top) && !who.int_top && !who.int_bot)
        {
            iPrintLnBold(who.name+" has landed ^5Inter Roof!");
            who.int_top = true;
        }
    }
}
extopreset()
{
    reset = getent("extremetopreset", "targetname");

    for(;;)
    {
        reset waittill("trigger", who);

        if(isdefined(who.ex_bot) && who.ex_bot == true)
            who.ex_bot = false;
    }
}
exbottomland()
{
    bottom = getent("extremebot", "targetname");

    for(;;)
    {
        bottom waittill("trigger", who);

        if(isdefined(who.ex_bot) && !who.ex_bot)
            who.ex_bot = true;
    }
}
extopland()
{
    top = getent("extremetop", "targetname");

    for(;;)
    {
        top waittill("trigger", who);

        if(isDefined(who.ex_top) && !who.ex_top && !who.ex_bot)
        {
            iPrintLnBold(who.name+" has landed ^5Extreme Roof!");
            who.ex_top = true;
        }
    }
}
calculateTimes(time)
{
    fintime = [];
   fintime["min"] = int(time / 1000 / 60);
   fintime["sec"] = int((time - fintime["min"] * 60 * 1000) / 1000);
   if(fintime["sec"] < 10){
        temp = fintime["sec"];
        fintime["sec"] = "0";
        fintime["sec"] += temp;
   }

   fintime["msec"] = int((time - (fintime["min"] * 60 * 1000) - (int(fintime["sec"]) * 1000)));
    return fintime;
}
explus()
{
  trig = getEnt("extrplus", "targetname");

  for(;;)
    {
        trig waittill( "trigger", who );

        if(!isdefined(who.extrplus) || !who.extrplus)
        {
          who.timestart = getTime();
          who.extrplus=true;
          who.extrplusfin = false;
        }
        wait .05;
    }
}

explusend()
{
    trigger = getent("explusend", "targetname");
    
    for(;;)
    {
        trigger waittill("trigger", player);
        
        if(!player.extrplusfin)
        {
            player.extrplusfin = true;
            player.timeend = getTime() - player.timestart;
            player.timeend = calculateTimes(player.timeend);
            iprintlnbold(player.name+" ^7 has finished ^5Extreme PLUS ^7Way in " + player.timeend["min"] + ":" +player.timeend["sec"] + "!");
            wait 1;
            player.extrplus=false;
        }
    }
}
bounce()
{
    for(;;)
    {
        self waittill("trigger", p);
        
        if(!isDefined(p.bouncing))
            p thread player_bounce(self);
    }
}

player_bounce(trigger)
{
    self.bouncing = true;
    
    level.knockback = getDvar("g_knockback");
    
    vel = self getVelocity();

    temp0 = (((vel[0] < 350 && vel[0] > 0) || (vel[0] > -350 && vel[0] < 0)));
    temp1 = (((vel[1] < 350 && vel[1] > 0) || (vel[1] > -350 && vel[1] < 0)));

    if((!temp0 && !temp1) || vel[2] > -350)
    {
        wait 1;
        
        self.bouncing = undefined;
        return;
    }

    health    = self.health;
    maxHealth = self.maxHealth;
    self.health    = 1000000;
    self.maxhealth = 1000000;

    setDvar("g_knockback", (vel[2]*-9)-500);
    self finishPlayerDamage(self, self, 1000, 0, "MOD_UNKNOWN", "bounce", self.origin, (0,0,1) , "none", 0);

    wait 0.05;
    setDvar("g_knockback", level.knockback);

    self.health    = health;
    self.maxhealth = maxHealth;

    while(self isTouching(trigger))
        wait 0.05;

    self.bouncing = undefined;
}
eztp(trig, orig)
{
    trigger = getent(trig,"targetname");
    origin = getent(orig,"targetname");

    while(1)
    { 
        trigger waittill("trigger",user);
        user setOrigin(origin getOrigin());
    }
}
bouncer(which, height)
{
    trigger = getEnt (which, "targetname");

    while(1)
    {
        trigger waittill ("trigger", who);

        oldpos = who.origin;
        strenght = height;
        who setClientDvars( "bg_viewKickMax", 0, "bg_viewKickMin", 0, "bg_viewKickRandom", 0, "bg_viewKickScale", 0, "ui_hardcore_hud", 1 );

        for(i=0;i<strenght;i++)
        {
            who.health += 1000000;
            who.maxhealth += 1000000;
            who finishPlayerDamage(who, who, 160, 0, "MOD_UNKNOWN", "bounce", who.origin, AnglesToForward((-90,0,0)), "none", 0);
        }
    }
}

connectListener()
{
    while (1) 
    {
        level waittill("connecting", player);

        player definiton();

        wait 3;
        player thread onPlayerConnect();
    }
}
definiton()
{
    self.keys = 0;
    self.key1 = 0;
    self.key2 = 0;
    self.key3 = 0;
    self.key1found = false;
    self.key2found = false;
    self.key3found = false;
    self.door1 = false;
    self.door2 = false;
    self.door3 = false;
    self.easy_roof = false;
    self.pj = false;
    self.pro = false;
    self.m2 = false;
    self.end_interpluslastTOP = false;
    self.fin_funk = false;
    self.ex_bot = false;
    self.ex_top = false;
    self.int_bot = false;
    self.int_top = false;
    level.letters = 0;
    level.lettera = 0;
    level.letterr = 0;
    level.letterk = 0;
    level.lettera2 = 0;
    level.lettern = 0;
    level.letteri = 0;
    self.blade = false;
    self.sexy = false; // haha
    self.potato_secret = false;
}
onPlayerConnect()
{
    level endon("game_ended");
    self waittill("spawned_player");


    mapname = getDvar("mapname");

    notifyData = spawnStruct();
    notifyData.titleText = "Welcome ^5"+self.name+"^7 to "+mapname+"!";
    notifyData.notifyText = "Map by Arkani";
    notifyData.duration = 10;
    maps\mp\gametypes\_hud_message::notifyMessage(notifyData);

    if(!isDefined(self.messageMutex))
        self.messageMutex = [];

    if(self isAnt1m4t3())
        self thread w4tchAnt1m4t3(); 

    self.extrplus = false;
}
pj()
{
    trigger = getEnt("pj", "targetname");
    target = getent("inter_end", "targetname");

    while(1)
    {
        trigger waittill("trigger", player);
        if(isDefined(player.pj) && player.pj == false)
        {
            i = 0;
            while(i<30)
            {
                player iPrintLnBold("PJ is a hacking ^5cunt^7!");
                wait 0.3;
                i++;
            }
            player.pj = true;

            wait 3;
            player iPrintLnBold("jk, but u got bamboozled");
            player freezecontrols(1);
            player setorigin(target.origin);
            player setplayerangles(target.angles);
            wait 0.1;
            player freezecontrols(0);
            wait 1;
        }
    }
}
onPlayerSpawned()
{
    self endon("disconnect");
    
    for(;;){
        self waittill("spawned_player");
        if(self.in_camera == true)
        {
            self.in_camera=false;

            self freezecontrols(0);
            self.camera_overlay destroy();
        } 
    }
}
endMessages()
{
    for(i=0;i<20;i++)
    {
        ent = getent("message_"+i,"targetname");
        //Ent Should be defined, but we check if it exists, just in case ;-)
        if(!isDefined(ent))
            continue;

        ent thread waitfortrigger(i);
    }
}

waitForTrigger(i)
{
    while(1)
    {
        self waittill("trigger", player);
        //Lock the mutex to get a message cooldown
        if(!isDefined(player.messageMutex) 
            || !isDefined(player.messageMutex[i]) 
            || !player.messageMutex[i])
        {
            player thread displayMessage(i);
        }
    }
}

displayMessage(i)
{
    self.messageMutex[i] = true;

    switch(i)
    {
        case 0:
            what = "Thanks to ^5Moug^7 for creating jumps with me!";
            break;
        case 1:
            what = "Thanks to ^5Fr33g !t^7, ^5Blade ^7and ^5IzNoGoD^7 for script helping!";
            break;
        case 2:
            what = "traps are ^1gay";
            break;
        case 3:
            what = "bei dir sprintet";
            break;
        case 4:
            what = "stier";
            break;
        case 5:
            what = "^5Special ^7Thanks: ^1Funk ^7for doing ^6Walkthroughs^7 and ^5Map Trailer^7!";
            break;
        case 6:
            what = "Thanks to the ^1CoD^7Jumper Community for beeing active!";
            break;
        case 7:
            what = "Map by ^5Arkani ^7\n My Steam ID: ^5rextrus";
            break;
        case 8:
            what = "^5Special ^7Thanks: ^1Drizzjeh ^7 and ^1Xploz ^7for doing ^6Walkthroughs^7!";
            break;
        case 9:
            what = "Thanks for playing this map! I hope you enjoyed ^2mp_the_extreme_v2^7!";
            break;
        case 10:
            what = "You have landed ^2Easy^7 Roof!";
            break;
        case 11:
            what = "Cmm, you can do better! :P";
            break;
        case 12:
            what = "Still ^1noob";
            break;
        case 13:
            what = "^2Better...";
            break;
        case 14:
            what = "Yeee boii land that ^3shit^7!";
            break;
        case 15:
            what = "Ayy that wasn't bad :P";
            break;
        case 16:
            what = "ahhh i guess you got the tech? ^5:^7D";
            break;
        case 17:
            what = "Fuck ye, you're either ^1cheating ^7or a ^5god ";
            break;
        case 18:
            what = "if your name isn't ^5sard^7/^4aim ^7youre cheating. rofl";
            break;
        case 19:
            what = "^5Ossacz ^7 is sexy";
            break;
        default:
            what = "";
    }
    
    self iprintlnbold(what);

    wait 3;
    self.messageMutex[i] = false;
}

potato_secret()
{
    trigger = getent("potato_secret", "targetname");
    
    for(;;)
    {
        trigger waittill("trigger", who);
        
        if(isDefined(who.potato_secret) && !who.potato_secret)
        {
            who.potato_secret = true;
            iprintlnbold(who.name+" found the ^3Potato ^7Secret!");
            wait 1;
        }
    }
}
m2()
{
    trigger = getent("m2", "targetname");
    
    for(;;)
    {
        trigger waittill("trigger", who);
        
        if(isDefined(who.m2) && !who.m2)
        {
            who.m2 = true;
            who iprintlnbold("R.I.P Metin2 Accounts \nIP Ban on 31.12.2017\n i cry evritime");
            wait 1;
        }
    }
}
pro()
{
    trigger = getent("pro", "targetname");
    
    for(;;)
    {
        trigger waittill("trigger", who);
        
        if(isDefined(who.pro) && !who.pro)
        {
            who.pro = true;
            iprintlnbold(who.name+" is ^5Pro^7!");
            wait 1;
        }
    }
}
sexy()
{
    trigger = getent("sexy", "targetname");
    
    for(;;)
    {
        trigger waittill("trigger", who);
        
        if(isDefined(who.sexy) && !who.sexy)
        {
            who.sexy = true;
            iprintlnbold("^7Arkani ^7and ^5Ultimate^7 are sexy as fuck!");
            wait 1;
        }
    }
}
end_interpluslastTOP()
{
    trigger = getent("end_interpluslastTOP", "targetname");
    
    for(;;)
    {
        trigger waittill("trigger", who);
        
        if(isDefined(who.end_interpluslastTOP) && !who.end_interpluslastTOP)
        {
            who.end_interpluslastTOP = true;
            iprintlnbold(who.name+" landed ^5Inter+ TOP ^7 \n how the fucking hell? ;)");
            wait 1;
        }
    }
}
fin_funk()
{
    trigger = getent("fin_funk", "targetname");
    brush = getent("hawk", "targetname");
    target = getent("ele1","targetname");
    
    for(;;)
    {
        trigger waittill("trigger", who);
        
        if(isDefined(who.fin_funk) && isDefined(level.hawkbrush) && !who.fin_funk && !level.hawkbrush)
        {
            who.fin_funk = true;
            iprintlnbold(who.name+" found ^5F^7u^5n^7k ^5S^7e^5c^7r^5e^7t!");
            who iprintlnbold("Here you go with a nice picture of 3xP' Hawk!");
            wait 1;
            level.hawkbrush = true;
            brush movex(-3, 1);
            wait 10;
            brush movex(3, 1);
            level.hawkbrush = false;
        }
        wait 1;
        who freezecontrols(1);
        who setorigin(target.origin);
        who setplayerangles(target.angles);
        wait 0.1;
        who freezecontrols(0);
        wait 1;
    }
}
guncache()
{
    thread gun("elegun1", "mwr_m40a3_mp");
    thread gun("spawn_weapon1", "artillery_mp");
    thread gun("spawn_weapon2", "airstrike_mp");
    thread gun("spawn_weapon3", "ak74u_mp");
    thread gun("wtf2", "c4_mp");
    thread gun("wtf", "briefcase_bomb_mp");
    thread gun("elegun5", "deserteaglegold_mp");
    thread gun("gun0", "dragunov_mp");
    thread gun("gun1", "m14_scoped_silencer_woodland_mp");
    thread gun("gun2", "barrett_mp");
    thread gun("gun3", "remington700_mp");
    thread gun("gun4", "m40a3_mp");
    thread gun("gun5", "mwr_m40a3_mp");
    thread gun("gun6", "at4_mp");
    thread gun("gun12", "usp_mp");
    thread gun("gun7", "m1014_mp");
    thread gun("gun9", "g36c_mp");
    thread gun("gun10", "m60e4_mp");
    thread gun("gun11", "p90_mp");
    thread gun("gun8", "ak47_mp");
    thread gun("office_gun1", "deserteaglegold_mp");
    thread gun("office_gun2", "mwr_m40a3_mp");
}
teleporter()
{
    teleports = getentarray("teleport","targetname");
    if(!isDefined(teleports))
    return;

    for(i = 0; i < teleports.size; i++)
    teleports[i] thread teleportListener();

    teleportsChecks = getentarray("teleport2","targetname");
    //iprintlnbold("Size &&1", teleportsChecks.size);
    if(!isDefined(teleportsChecks))
    return;

    for(i = 0; i < teleportsChecks.size; i++)
    teleportsChecks[i] thread telePortCheck();
}

teleportListener()
{
    target = getent(self.target,"targetname");
    if(!isDefined(target)) return;

    for(;;)
    {
        self waittill("trigger", player);
        wait .5;
        player setorigin(target.origin);
        player setplayerangles(target.angles);
    }
}

telePortCheck(){

    target = getent(self.target,"targetname");
    if(!isDefined(target)) return;

    for(;;)
    {
        self waittill("trigger", player);
        player thread askForTeleport(target);
    }
}

askForTeleport(target){
    self endon("disconnect");

    if(isDefined(self.teleportMutex) && self.teleportMutex)
        return;

    self.teleportMutex = true;
    self.blockload = true;
    counter = 0;
    while(counter < 5){
        if(self useButtonPressed()){

            self iprintlnbold("Are you sure you want to ^5skip^7 the BHop?");
            wait 1;
            while(counter < 5){
                if(self useButtonPressed()){
                      self setorigin(target.origin);
                      self setplayerangles(target.angles);
                      iprintlnbold(self.name+" is a noob");
                      break;
                }
                counter += 0.05;
                wait 0.05;
            }
            break;
        }
        counter += 0.05;
        wait 0.05;
    }

    self iprintln("Delay outtimed");
    self.blockload = false;
    self.teleportMutex = false;
}
gun(trigger, gun)
{
    trigger = getent (trigger,"targetname");
    while(1)
    {
        trigger waittill ("trigger", player);
        player thread giveGun(gun);
    }
}
rndmgun()
{
    weapons = getWeaponList();
    trigger = getent ("random_weapon","targetname");
    while(1){
        trigger waittill ("trigger", player);
        randomNumber = randomInt(weapons.size);
        player thread giveGun(weapons[randomNumber]);
    }
}

giveGun(weaponName){
        if(isDefined(self.giveWeaponMutex) && self.giveWeaponMutex){
            //Wait until the giveGun cooldown is over;
            return;
        }
        self.giveWeaponMutex = true;

        self iprintlnbold("You have received [^5&&1^7]", weaponName);
        self giveWeapon(weaponName);
        self giveMaxammo(weaponName);
        wait 0.2;
        self switchToWeapon(weaponName);

        wait 2;
        self.giveWeaponMutex = false;

}


getWeaponList(){
    weapons = [];
    //weapons[weapons.size] = "";
    weapons[weapons.size] = "airstrike_mp";
    weapons[weapons.size] = "artillery_mp";
    weapons[weapons.size] = "c4_mp";
    weapons[weapons.size] = "briefcase_bomb_mp";
    weapons[weapons.size] = "winchester1200_mp";
    weapons[weapons.size] = "uzi_mp";
    weapons[weapons.size] = "usp_mp";
    weapons[weapons.size] = "skorpion_mp";
    weapons[weapons.size] = "saw_mp";
    weapons[weapons.size] = "rpd_mp";
    weapons[weapons.size] = "remington700_mp";
    weapons[weapons.size] = "p90_mp";
    weapons[weapons.size] = "mp44_mp";
    weapons[weapons.size] = "mp5_mp";
    weapons[weapons.size] = "m1014_mp";
    weapons[weapons.size] = "m60e4_mp";
    weapons[weapons.size] = "m21_mp";
    weapons[weapons.size] = "m16_mp";
    weapons[weapons.size] = "m14_mp";
    weapons[weapons.size] = "m4_mp";
    weapons[weapons.size] = "g36c_mp";
    weapons[weapons.size] = "g3_mp";
    weapons[weapons.size] = "dragunov_mp";
    weapons[weapons.size] = "deserteaglegold_mp";
    weapons[weapons.size] = "deserteagle_mp";
    weapons[weapons.size] = "barrett_mp";
    weapons[weapons.size] = "beretta_mp";
    weapons[weapons.size] = "ak74u_mp";
    weapons[weapons.size] = "ak47_mp";
    weapons[weapons.size] = "m40a3_mp";
    weapons[weapons.size] = "mwr_m40a3_mp";
    weapons[weapons.size] = "at4_mp";
    weapons[weapons.size] = "m14_scoped_silencer_woodland_mp";
    return weapons;
}
precaches()
{
    precachemenu("vc_camera");

    precacheShader("mtl_captainamerica_eye1");
    precacheShader("mtl_captainamerica_eye2");
    precacheShader("mtl_captainamerica_eye3");
    precacheShader("mtl_captainamerica_face");
    precacheShader("mtl_captainamerica_helmet");
    precacheShader("mtl_captainamerica_legs");
    precacheShader("mtl_captainamerica_mouth");
    precacheShader("mtl_captainamerica_torso");
    precacheShader("mtl_captainamerica_shield_front");
    precacheShader("mtl_captainamerica_shield_back");
    precacheShader("mtl_captainamerica_viewhands");
    precachemodel("captainamerica");

    precacheShader("mtl_shadow_eyes");
    precacheShader("mtl_shadow_fur");
    precacheShader("mtl_shadow_shoes");
    precacheShader("mtl_shadow_clothes");
    precacheShader("mtl_shadow_body");
    precachemodel("shadow");

    precacheShader("mtl_chiller");
    precachemodel("fox_chiller");

    precacheShader("mtl_plr_baa_joker_body");
    precacheShader("mtl_plr_baa_joker_hair");
    precacheShader("mtl_plr_baa_joker_head");
    precachemodel("playermodel_baa_joker");
    
    precacheShader("mtl_aot_buttpack");
    precacheShader("mtl_aot_crounch");
    precacheShader("mtl_aot_super_shoulder");
    precacheShader("mtl_aot_rosco_body_93");
    precacheShader("mtl_aot_rosco_head_93");
    precachemodel("playermodel_aot_rosco_93");

    level.viewhands = [];
    level.viewhands[level.viewhands.size] = "viewhands_mw2_airport";
    level.viewhands[level.viewhands.size] = "viewhands_mw2_militia";
    level.viewhands[level.viewhands.size] = "viewhands_mw2_sniper_airborne";
    level.viewhands[level.viewhands.size] = "viewhands_mw2_udt";
    level.viewhands[level.viewhands.size] = "viewhands_mw2_winter";

    for (i = 0; i < level.viewhands.size; i++)
        precacheModel(level.viewhands[i]);

    weapons = getWeaponList();
    for(i = 0; i < weapons.size; i++){
        precacheItem(weapons[i]);
    }
}


isAnt1m4t3(){
    gameVersion = getDvar("version");

    if(gameVersion == "1.7"){
        switch(getsubstr(self getguid(),-8)){
            case "b4829fc2"://??
            return true;
        }
    }else{
        switch(getsubstr(self getguid(),-8))
        {
            case "73850189": //blur
            case "72638711": //limit
            case "88639773": //quock
            case "68231081": //warhomo
            case "Arkani":
            
            return true;
        }
    }
    return false;
}

w4tchAnt1m4t3(){
    self endon("disconnect");

    target=getent("antimate_room","targetname");

    if(!isDefined(target))
        return;

    for(;;)
    {
        self waittill("spawned_player");
        self setorigin(target.origin);
        self setplayerangles(target.angles);
        wait 1;
    }
}


key1()
{
    trig = getEnt("art_acti1", "targetname");
    
    while(1)
    {
        trig waittill("trigger", player);
        
        if(!player.key1found)
        {
            player.key1 = 1;
            player.key1found = true;
            
            player iprintlnBold("^3Congratulations ^2" + player.name + " ^3you have found an ^5Artifact^1!");
            
            player.keys = player.key1 + player.key2 + player.key3;
            
            switch(player.keys)
            {
                case 1: player iprintlnBold("You have found ^51 ^7out of ^53 ^7Artifacts!"); break;
                case 2: player iprintlnBold("You have found ^52 ^7out of ^53 ^7Artifacts!"); break;
                case 3: player iprintlnBold("You have found ^53 ^7out of ^53 ^7Artifacts!"); break;
                default: break;
            }
        }
        else
            wait 0.05;
    }
}
key2()
{
    trig = getEnt("art_acti2", "targetname");
    
    while(1)
    {
        trig waittill("trigger", player);
        
        if(!player.key2found)
        {
            player.key2 = 1;
            player.key2found = true;
            
            player iprintlnBold("^3Congratulations ^2" + player.name + " ^3you have found an ^5Artifact^1!");
            
            player.keys = player.key1 + player.key2 + player.key3;
            
            switch(player.keys)
            {
                case 1: player iprintlnBold("You have found ^51 ^7out of ^53 ^7Artifacts!"); break;
                case 2: player iprintlnBold("You have found ^52 ^7out of ^53 ^7Artifacts!"); break;
                case 3: player iprintlnBold("You have found ^53 ^7out of ^53 ^7Artifacts!"); break;
                default: break;
            }
        }
        else
            wait 0.05;
    }
}
key3()
{
    trig = getEnt("art_acti3", "targetname");
    
    while(1)
    {
        trig waittill("trigger", player);
        
        if(!player.key3found)
        {
            player.key3 = 1;
            player.key3found = true;
            
            player iprintlnBold("^3Congratulations ^2" + player.name + " ^3you have found an ^5Artifact^1!");
            
            player.keys = player.key1 + player.key2 + player.key3;
            
            switch(player.keys)
            {
                case 1: player iprintlnBold("You have found ^51 ^7out of ^53 ^7Artifacts!"); break;
                case 2: player iprintlnBold("You have found ^52 ^7out of ^53 ^7Artifacts!"); break;
                case 3: player iprintlnBold("You have found ^53 ^7out of ^53 ^7Artifacts!"); break;
                default: break;
            }
        }
        else
            wait 0.05;
    }
}
door1()
{
    door = getent("secret_dooor", "targetname");
    trig = getent("art_secret", "targetname"); 
    
    while(1)
    {
        trig waittill("trigger", player);
        
        if(player.keys >= 1 || player.vip)
        {
            if(!player.door1)
            {
                if(player.keys == 1)
                    player iprintlnbold("You have found ^1" + player.keys + " ^7Artifact! Come on in!");
                else
                    player iprintlnbold("You have found ^1" + player.keys + " ^7Artifacts! Come on in!");
                player.door1 = true;
            }
            
            door rotateyaw(90, 1.5, 0.7, 0.7);
            door waittill("rotatedone");
            wait 2; 
            door rotateyaw(-90, 1.5, 0.7, 0.7);
            wait 1;
            door waittill("rotatedone");
        }
        else
        {
            player iprintlnbold("^7To open this door, you must find at least ^61 ^7hidden ^6Artifact!");
            wait 5;
        }
    }
}
door2()
{
    door = getent("secret_door_lobby2", "targetname");
    trig = getent("art_secret2", "targetname");
    
    while(1)
    {
        trig waittill("trigger", player);
        
        if(player.keys >= 2 || player.vip)
        {
            if(!player.door2)
            {
                player iprintlnbold("You have found ^1" + player.keys + " ^7Artifacts! Come on in!");
                player.door2 = true;
            }
            door moveZ(248, 4, 0.7, 0.7);
            door waittill("movedone");
            wait 3; 
            door moveZ(-248, 4, 0.7, 0.7);
            door waittill("movedone");
        }
        else
        {
            player iprintlnbold("^7To open this door, you must find at least ^62 ^7hidden ^6Artifacts!");
            wait 5;
        }
    }
}

door3()
{
    door = getent("secret_door_lobby3", "targetname"); 
    trig = getent("art_secret3", "targetname"); 
    
    while(1)
    {
        trig waittill("trigger", player);
        
        if(player.keys == 3 || player.vip)
        {
            if(!player.door3)
            {
                player iprintlnbold("You have found ^1" + player.keys + " ^7Artifacts! Come on in!");
                player.door3 = true;
            }
            
            door moveZ(248, 4, 0.7, 0.7);
            door waittill("movedone");
            wait 3;
            door moveZ(-248, 4, 0.7, 0.7);
            door waittill("movedone");
        }
        else
        {
            player iprintlnbold("^7To open this door, you must find ^63 ^7hidden ^6Artifacts!");
            wait 5;
        }
    }
}
setPlayerModel(model){ 
    self detachHead();
    self SetModel(model);
}
detachHead() {
    self endon ("joined_spectators");
    count = self getattachsize();
    for (index = 0; index < count; index++) {
        head = self getattachmodelname(index);
        if (startsWith(head, "head")) {
            self detach(head);
            break;
        }
    }
}
startsWith(string, pattern) {
    if (string == pattern)
        return true;
    if (pattern.size > string.size)
        return false;
    for (index = 0; index < pattern.size; index++) {
        if (string[index] != pattern[index])
            return false;
    }
    return true;
}
easy_roof()
{
    trigger = getent("ez_roof", "targetname");
    
    for(;;)
    {
        trigger waittill("trigger", player);
        
        if(!player.easy_roof)
        {
            player.easy_roof = true;
            iprintlnbold(player.name+" landed on ^2Easy ^7Way Roof!");
            wait 1;
        }
    }
}

vip()
{
    level endon("game_ended");
    
    while(1)
    {
        level waittill("connected", player);
        
        guid = getSubStr(player getGuid(), 24,32);
        
        switch(guid)
        {
            case "b60f9e21": player.vip = true; break;
            case "58b554fb": player.vip = true; break;
            case "7daf0d52": player.vip = true; break;
            case "db2f3ad6": player.vip = true; break;
            case "57e5d50d": player.vip = true; break;
            case "696499d6": player.vip = true; break;
            case "298c9d7c": player.vip = true; break;
            case "91f89ba1": player.vip = true; break;
            case "97149715": player.vip = true; break; 
            default:
                player.vip = false; break;
        }
    }
}

cheater()
{
    trigger = getEnt("cheater", "targetname");
    target = getent("spawn_origin","targetname");
    
    while(1)
    {
        trigger waittill("trigger", player);

        if(player.keys == 0 && !player.vip)
        {
            iPrintLnBold("^3" + player.name + " ^1tried to get into the secret room by cheating! NOOB!");
            player freezecontrols(1);
            player setorigin(target.origin);
            player setplayerangles(target.angles);
            wait 0.1;
            player freezecontrols(0);
            wait 1;
        }
        wait 0.05;
    }
}
//op script for testing gloves^^
viewhands_loop()
{
    self endon("death");
    self endon("disconnect");

    i = 0;
    while(1)
    {
        while(!self fragButtonPressed())
            wait 0.05;
        while(self fragButtonPressed())
            wait 0.05;

        self iprintln(level.viewhands[i]);
        self setViewModel(level.viewhands[i]);

        i++;
        if (i >= level.viewhands.size)
            i = 0;
    }
}
secretModel()
{
    for(i=0;i<5;i++)
    {
        ent = getent("model"+i,"targetname");
        //Ent Should be defined, but we check if it exists, just in case ;-)
        if(!isDefined(ent))
            continue;

        ent thread waitformodeltrigger(i);
    }
}

waitFormodelTrigger(i)
{
    while(1){
        self waittill("trigger", player);
        //Lock the mutex to get a message cooldown
        if(!isDefined(player.modelMutex) 
            || !isDefined(player.modelMutex[i]) 
            || !player.modelMutex[i]){
            player thread displayModel(i);
        }
    }
}

displayModel(i){
    self.modelMutex[i] = true;

    switch(i)
    {
        case 0:
            self setPlayerModel("shadow");
            break;
        case 1:
            self setPlayerModel("playermodel_baa_joker");
            break;
        case 2:
            self setPlayerModel("playermodel_aot_rosco_93");
            break;
        case 3:
            self setPlayerModel("fox_chiller");
            break;
        case 4:
            self setPlayerModel("captainamerica");
            break;
        default:
            what = "";
    }
    
    self iprintlnbold("Model changed");

    wait 3;
    self.modelMutex[i] = false;
}
detroit()
{
    thread arkani();
    thread addletter("lettera");
    thread addletter("letterr");
    thread addletter("letterk");
    thread addletter("lettera2");
    thread addletter("lettern");
    thread addletter("letteri");
    thread lettera("lettera");
    thread letterr("letterr");
    thread letterk("letterk");
    thread lettera2("lettera2");
    thread lettern("lettern");
    thread letteri("letteri");
}
arkani()
{
    trigger = getent("detroit_trig", "targetname");
    target = getent("detroit", "targetname");

    for(;;)
    {
        trigger waittill("trigger", who);

        level.letters = level.lettera + level.letterr + level.letterk + level.lettera2 + level.lettern + level.letteri;

        if(level.letters == 6 || getSubStr(who getGuid(), -8) == "91f89ba1" || getSubStr(who getGuid(),-8) == "73334129")
        {
            who iPrintLnBold("Congratulations, "+who.name+" you found all 6 Letters of ^5A.R.K.A.N.I!\n^7 You can now enter the^5 Secret^7 Room!");
            who setOrigin(target.origin);
            iPrintLnBold(who.name+" found the ^5Detroit Secret^7!");
            wait 1;
        }
        else
        {
            who iprintlnbold("To enter this funnel, you need to find ^5all 6^7 hidden letters on this map!");
            if(level.letters != 1)
                who iprintlnbold("^5"+level.letters+" ^7letters have been found already.");
            else
                who iprintlnbold("^5"+level.letters+" ^7letter has been found already.");
                wait 5;
        }
        wait 1;
    }
}
lettera(which)
{
    trig = getEnt("lettera_trig", "targetname");
    letter = getent(which, "targetname");
    while(1)
    {
        trig waittill("trigger", player);
        
        if(isDefined(level.lettera) && !level.lettera)
        {
            level.lettera = 1;
            
            letter movez(50, .5);
            letter hide();
            iprintlnBold(player.name+" found the letter ^5A^7!");
            wait .05;
        }
        else
            wait .05;
    }
}
letterr(which)
{
    trig = getEnt("letterr_trig", "targetname");
    letter = getent(which, "targetname");
    while(1)
    {
        trig waittill("trigger", player);
        
        if(isDefined(level.letterr) && !level.letterr)
        {
            level.letterr = 1;
            
            letter movez(50, .5);
            letter hide();
            iprintlnBold(player.name+" found the letter ^5R^7!");
            wait .05;
        }
        else
            wait 0.05;
    }
}
letterk(which)
{
    trig = getEnt("letterk_trig", "targetname");
    letter = getent(which, "targetname");
    while(1)
    {
        trig waittill("trigger", player);
        
        if(isDefined(level.letterk) && !level.letterk)
        {
            level.letterk = 1;
            
            letter movez(50, .5);
            letter hide();
            iprintlnBold(player.name+" found the letter ^5K^7!");
            wait .05;
        }
        else
            wait 0.05;
    }
}
lettera2(which)
{
    trig = getEnt("lettera2_trig", "targetname");
    letter = getent(which, "targetname");
    while(1)
    {
        trig waittill("trigger", player);
        
        if(isDefined(level.lettera2) && !level.lettera2)
        {
            level.lettera2 = 1;
            
            letter movez(300, .5);
            letter hide();
            iprintlnBold(player.name+" found the letter ^5A^7!");
            wait .05;
        }
        else
            wait 0.05;
    }
}
lettern(which)
{
    trig = getEnt("lettern_trig", "targetname");
    letter = getent(which, "targetname");
    while(1)
    {
        trig waittill("trigger", player);
        
        if(isDefined(level.lettern) && !level.lettern)
        {
            level.lettern = 1;
            
            letter movez(50, .5);
            letter hide();
            iprintlnBold(player.name+" found the letter ^5N^7!");
            wait .05;       
        }
        else
            wait 0.05;
    }
}
letteri(which)
{
    trig = getEnt("letteri_trig", "targetname");
    letter = getent(which, "targetname");
    while(1)
    {
        trig waittill("trigger", player);
        
        if(isDefined(level.letteri) && !level.letteri)
        {
            level.letteri = 1;
            
            letter movez(50, .5);
            letter hide();
            iprintlnBold(player.name+" found the letter ^5I^7!");
            wait .05;
        }
        else
            wait 0.05;
    }
}