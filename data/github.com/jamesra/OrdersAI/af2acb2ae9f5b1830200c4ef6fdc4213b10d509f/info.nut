/*
 * This file is part of OrdersAI.
 *
 * OrdersAI is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * OrdersAI is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with OrdersAI.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright 2014 James Anderson
 */

class OrdersAI extends AIInfo {
	function GetAuthor()        { return "James R. Anderson"; }
	function GetName()          { return "Orders Assistant AI"; }
	function GetShortName()     { return "OrdA"; }
	function GetDescription()   { return "An AI assistant which manages vehicle schedules to reduce the micromanagement burden.  Great if you want to focus on network optimization or have small kids who want to play.  This AI is intended to play on the same company as the player.  Thus at the beginning of the game the player should use CTRL+ALT+C to activate cheat mode and switch to the AI's company.  See the readme.rst for more detail."; }
	function GetVersion()       { return 9; }
	function MinVersionToLoad() { return 1; }
	function GetDate()          { return "2014-03-20"; }
	function CreateInstance()   { return "OrdersAI"; }
	function GetAPIVersion()    { return "1.4"; }
	function UseAsRandomAI()    { return false; }
	function GetSettings() {
		//AddSetting({name = "use_busses", description = "Enable busses", easy_value = 1, medium_value = 1, hard_value = 1, custom_value = 1, flags = AICONFIG_BOOLEAN});
		//AddSetting({name = "use_trucks", description = "Enable trucks", easy_value = 1, medium_value = 1, hard_value = 1, custom_value = 1, flags = AICONFIG_BOOLEAN});
		//AddSetting({name = "use_planes", description = "Enable aircraft", easy_value = 1, medium_value = 1, hard_value = 1, custom_value = 1, flags = AICONFIG_BOOLEAN});
		//AddSetting({name = "use_trains", description = "Enable trains", easy_value = 1, medium_value = 1, hard_value = 1, custom_value = 1, flags = AICONFIG_BOOLEAN});
		//AddSetting({name = "build_statues", description = "Try to build statues as soon as the AI has enough money",  easy_value = 0, medium_value = 1, hard_value = 1, custom_value = 1, flags = AICONFIG_BOOLEAN});
		//AddSetting({name = "always_autorenew", description = "Always use autoreplace regardless of the breakdown setting", easy_value = 0, medium_value = 0, hard_value = 0, custom_value = 0, flags = AICONFIG_BOOLEAN});
		//AddSetting({name = "depot_near_station", description = "Build train depots near the loading station instead of near the dropoff station.", easy_value = 1, medium_value = 1, hard_value = 1, custom_value = 1, flags = AICONFIG_BOOLEAN});
		//AddSetting({name = "build_bus_dtrs", description = "Build drive-through stops for busses", easy_value = 1, medium_value = 1, hard_value = 1, custom_value = 1, flags = AICONFIG_BOOLEAN});
		//AddSetting({name = "debug_signs", description = "Enable building debug signs", easy_value = 0, medium_value = 0, hard_value = 0, custom_value = 0, flags = AICONFIG_BOOLEAN});
		AddSetting({name = "min_rating", description = "Minimum service rate in percent to consider available cargo", min_value = 0, max_value = 100, easy_value = 30, medium_value = 30, hard_value = 30, custom_value = 30, step_size = 5, flags = AICONFIG_INGAME});
		AddSetting({name = "load_town_cargo", description = "Town cargo load orders", min_value = 0, max_value = 2, easy_value = 1, medium_value = 1, hard_value = 1, custom_value = 0, flags = AICONFIG_INGAME});
        AddLabels("load_town_cargo", {
            _0 = "Load if available",
            _1 = "Full load any cargo",
            _2 = "Full load all cargo"
        });
        
        AddSetting({name = "load_industry_cargo", description = "Industry cargo load orders", min_value = 0, max_value = 2, easy_value = 1, medium_value = 1, hard_value = 1, custom_value = 0, flags = AICONFIG_INGAME});
        AddLabels("load_industry_cargo", {
            _0 = "Load if available",
            _1 = "Full load any cargo",
            _2 = "Full load all cargo"
        });
	}
};

RegisterAI(OrdersAI());
