{
  "damageLog": {
    "enabled": false,
    "disabledDetailStats": true,
    "disabledSummaryStats": true,
    "log": {
      "moveInBattle": false,
      "x": 240,
      "y": -23,
      "showHitNoDamage": true,
      "groupDamagesFromFire": true,
      "groupDamagesFromRamming_WorldCollision": true,
      "groupDamageFromArtAndAirstrike": true,
      "dmg-kind": {
        "shot": "{{hit-effects}}{{critical-hit}}{{splash-hit}}<tab>{{type-shell}}",
        "fire": "{{hit-effects}}{{critical-hit}}<tab><font face='xvm'>&#x51;</font>",
        "ramming": "{{hit-effects}}{{critical-hit}}<tab><font face='xvm'>&#x52;</font>",
        "world_collision": "{{hit-effects}}{{critical-hit}}<tab><font face='xvm'>&#x53;</font>",
        "drowning": "{{l10n:drowning}}<tab><font face='xvm'>&#x119;</font>",
        "overturn": "{{hit-effects}}<tab><font face='xvm'>&#x112;</font>",
        "death_zone": "DZ",
        "gas_attack": "GA",
        "art_attack": "{{hit-effects}}{{critical-hit}}{{splash-hit}}<tab><font face='xvm'>&#x110;</font>",
        "air_strike": "{{hit-effects}}{{critical-hit}}{{splash-hit}}<tab><font face='xvm'>&#x111;</font>"
      },
      "c:dmg-kind": {
        "shot": "{{c:hit-effects}}",
        "fire": "#FF6655",
        "ramming": "#998855",
        "world_collision": "#228855",
        "drowning": "#CCCCCC",
        "overturn": "#CCCCCC",
        "death_zone": "#CCCCCC",
        "gas_attack": "#CCCCCC",
        "art_attack": "{{c:hit-effects}}",
        "air_strike": "{{c:hit-effects}}"
      },
      "splash-hit": {
        "splash": "<font face='xvm'>&#x2C;</font>",
        "no-splash": ""
      },
      "type-shell": {
        "armor_piercing": "<font color='{{c:costShell}}'>{{l10n:armor_piercing}}</font>",
        "high_explosive": "<font color='{{c:costShell}}'>{{l10n:high_explosive}}</font>",
        "armor_piercing_cr": "<font color='{{c:costShell}}'>{{l10n:armor_piercing_cr}}</font>",
        "armor_piercing_he": "<font color='{{c:costShell}}'>{{l10n:armor_piercing_he}}</font>",
        "hollow_charge": "<font color='{{c:costShell}}'>{{l10n:hollow_charge}}</font>",
        "not_shell": ""
      },
      "c:type-shell": {
        "armor_piercing": "#CCCCCC",
        "high_explosive": "#CCCCCC",
        "armor_piercing_cr": "#CCCCCC",
        "armor_piercing_he": "#CCCCCC",
        "hollow_charge": "#CCCCCC",
        "not_shell": "#CCCCCC"
      },
      "vtype": {
        "HT": "<font face='xvm'>&#x3F;</font>",
        "MT": "<font face='xvm'>&#x3B;</font>",
        "LT": "<font face='xvm'>&#x3A;</font>",
        "TD": "<font face='xvm'>&#x2E;</font>",
        "SPG": "<font face='xvm'>&#x2D;</font>",
        "not_vehicle": ""
      },
      "c:vtype": {
        "HT": "#FFACAC",
        "MT": "#FFF198",
        "LT": "#A2FF9A",
        "TD": "#A0CFFF",
        "SPG": "#EFAEFF",
        "not_vehicle": "#CCCCCC"
      },
      "hit-effects": {
        "armor_pierced": "{{dmg}}",
        "intermediate_ricochet": "{{l10n:intermediate_ricochet}}",
        "final_ricochet": "{{l10n:final_ricochet}}",
        "armor_not_pierced": "{{l10n:armor_not_pierced}}",
        "armor_pierced_no_damage": "{{l10n:armor_pierced_no_damage}}",
        "unknown": "{{l10n:armor_pierced_no_damage}}"
      },
      "c:hit-effects": {
        "armor_pierced": "#FF4D3C",
        "intermediate_ricochet": "#CCCCCC",
        "final_ricochet": "#CCCCCC",
        "armor_not_pierced": "#CCCCCC",
        "armor_pierced_no_damage": "#CCCCCC",
        "unknown": "#CCCCCC"
      },
      "critical-hit": {
        "critical": "*",
        "no-critical": ""
      },
      "crit-device": {
        "engine_crit": "{{l10n:engine}}",
        "ammo_bay_crit": "{{l10n:ammo_bay}}",
        "fuel_tank_crit": "{{l10n:fuel_tank}}",
        "radio_crit": "{{l10n:radio}}",
        "left_track_crit": "{{l10n:left_track}}",
        "right_track_crit": "{{l10n:right_track}}",
        "gun_crit": "{{l10n:gun}}",
        "turret_rotator_crit": "{{l10n:turret_rotator}}",
        "surveying_device_crit": "{{l10n:surveying_device}}",
        "engine_destr": "{{l10n:engine}}",
        "ammo_bay_destr": "{{l10n:ammo_bay}}",
        "fuel_tank_destr": "{{l10n:fuel_tank}}",
        "radio_destr": "{{l10n:radio}}",
        "left_track_destr": "{{l10n:left_track}}",
        "right_track_destr": "{{l10n:right_track}}",
        "gun_destr": "{{l10n:gun}}",
        "turret_rotator_destr": "{{l10n:turret_rotator}}",
        "surveying_device_destr": "{{l10n:surveying_device}}",
        "commander": "{{l10n:commander}}",
        "driver": "{{l10n:driver}}",
        "radioman": "{{l10n:radioman}}",
        "gunner": "{{l10n:gunner}}",
        "loader": "{{l10n:loader}}",
        "no-critical": ""
      },
      "comp-name": {
        "turret": "{{l10n:turret}}",
        "hull": "{{l10n:hull}}",
        "chassis": "{{l10n:chassis}}",
        "wheel": "{{l10n:wheel}}",
        "gun": "{{l10n:gun}}",
        "unknown": ""
      },
      "team-dmg": {
        "ally-dmg": "",
        "enemy-dmg": "",
        "player": "",
        "unknown": ""
      },
      "c:team-dmg": {
        "ally-dmg": "#00EAFF",
        "enemy-dmg": "#CCCCCC",
        "player": "#228855",
        "unknown": "#CCCCCC"
      },
      "costShell": {
        "gold-shell": "",
        "silver-shell": "",
        "unknown": ""
      },
      "c:costShell": {
        "gold-shell": "#FFCC66",
        "silver-shell": "#CCCCCC",
        "unknown": ""
      },
      "shadow": {
        "distance": 1,
        "angle": 90,
        "color": "#000000",
        "alpha": 75,
        "blur": 5,
        "strength": 3,
        "hideObject": false,
        "inner": false,
        "knockout": false,
        "quality": 1
      },
      "formatHistory": "<textformat tabstops='[30,130,165,180]'><font face='mono' size='12'>{{number%3d~.}}</font><tab><font color='{{c:dmg-kind}}'>{{dmg-kind}}</font><tab><font color='{{c:vtype}}'>{{vtype}}</font><tab><font color='{{c:team-dmg}}'>{{vehicle}}</font></textformat>"
    },
    "logBackground": {
      "$ref": {
        "path": "damageLog.log"
      },
      "formatHistory": "<img height='20' width='310' src='xvm://res/icons/damageLog/{{dmg=0?no_dmg|dmg}}.png'>"
    },
    "logAlt": {
      "$ref": {
        "path": "damageLog.log"
      },
      "showHitNoDamage": true,
      "formatHistory": "<textformat tabstops='[30,130,165]'><font face='mono' size='12'>{{number%3d~.}}</font><tab><font color='{{c:dmg-kind}}'>{{dmg-kind}}</font><tab><font color='{{c:team-dmg}}'>{{name}}</font></textformat>"
    },
    "logAltBackground": {
      "$ref": {
        "path": "damageLog.logBackground"
      },
      "formatHistory": "<img height='20' width='310' src='xvm://res/icons/damageLog/{{dmg=0?no_dmg|dmg}}.png'>"
    },
    "lastHit": {
      "$ref": {
        "path": "damageLog.log"
      },
      "moveInBattle": false,
      "x": -120,
      "y": 200,
      "showHitNoDamage": true,
      "timeDisplayLastHit": 7,
      "shadow": {
        "distance": 0,
        "blur": 6,
        "strength": 6,
        "color": "{{dmg=0?#000000|#770000}}"
      },
      "dmg-kind": {
        "shot": "{{hit-effects}}",
        "fire": "{{hit-effects}}",
        "ramming": "{{hit-effects}}",
        "world_collision": "{{hit-effects}}",
        "drowning": "{{l10n:drowning}}",
        "overturn": "{{hit-effects}}",
        "death_zone": "DZ",
        "gas_attack": "GA",
        "art_attack": "{{hit-effects}}",
        "air_strike": "{{hit-effects}}"
      },
      "formatLastHit": "<font size='36' color='{{c:dmg-kind}}'>{{dmg-kind}}</font>"
    }
  }
}
