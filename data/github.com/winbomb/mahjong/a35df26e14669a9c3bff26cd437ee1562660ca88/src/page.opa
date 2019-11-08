/*************************************************************************
 *	Mahjong: An html5 mahjong game built with opa. 
 *  Copyright (C) 2012
 *  Author: winbomb
 *  Email:  li.wenbo@whu.edu.cn
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 ************************************************************************/
package mahjong

import stdlib.themes.bootstrap.css;
import stdlib.web.template;

type GameInfo.t = {unchanged} or {GameMsg.t msg}
type GameMsg.t = 
	{string id,   //game id
	 int rc,      //ready_count 
	 int tc,      //total_count
	 bool st      //status
	}

module Page {
		
	function page_ready(){
		//刷新游戏列表
		refresh_game_list();	

		//订阅游戏刷新方法
		obs = Network.observe(refresh_game_list_on_msg,hall);
		Dom.bind_unload_confirmation(function(_){
			Network.unobserve(obs);
			{none}
		});
	}

	function refresh_game_list(){
		List.iter(function(msg){
			Dom.set_text(#{msg.id^"_2"},"{msg.rc} / {msg.tc}");
			Dom.set_text(#{msg.id^"_3"},"{status_to_string(msg.st)}");			
			Dom.set_enabled(#{msg.id^"_4"}, not(msg.st || msg.tc == 4));
		},Game.get_game_list());
	}
	
	function status_to_string(status){
		if(status) "in process" else "waiting"
	}

	client function refresh_game_list_on_msg(message){
		List.iter(function(msg_x){
			match(msg_x){
				case {unchanged}: void
				case ~{msg}:{
					Dom.set_text(#{msg.id^"_2"},"{msg.rc} / {msg.tc}");
					Dom.set_text(#{msg.id^"_3"},"{status_to_string(msg.st)}");			
					Dom.set_enabled(#{msg.id^"_4"}, not(msg.st || msg.tc == 4));
				}
			}
		},message)
	}
	
	client function join_game(need_bot){
		match(need_bot){
		case {false}:{
			match(Game.get_free_gameid()){
			case {none}: jlog("unable to join game.");
			case {some:id}: {
				Client.goto("/game/{id}");
			}}
		}
		case {true}:{
			match(Game.get_empty_gameid()){
			case {none}: jlog("unable to join game.");
			case {some:id}: {
				Client.goto("/gamex/{id}");
			}}
		}}
	}

	function leave(){
		logout();
		Client.goto("/login");
	}

	function game_list_view(){
		Resource.full_page("China Mahjong",
			<>
			<div class="dragon_bg"></div>
			<div id="game_list" onready={function(_){ page_ready()}} >
				<div class="title"><h2>{get_username()}</h2></div>
				<div class="quick-start">
					<input type="button" class="btn btn-primary" value="Play With Bots"
						onclick={function(_){join_game({true})}}/>
					<input type="button" class="btn btn-primary" value="Quick Start"
						onclick={function(_){join_game({false})}}/>
					<input type="button" class="btn btn-primary" value="Leave"
						onclick={function(_){leave()}}/>
				</div>
				<div class="game_list_pannel">
					<table class="tb_game_list">
						<tr>
							<th>Game Id</th>
							<th>Player</th>
							<th>Status</th>
							<th>Join</th>
						</tr>
						{List.map(function(msg){
							<tr>
								<td> {msg.id} </td>
								<td id=#{msg.id^"_2"}>{msg.rc} / {msg.tc}</td>
								<td id=#{msg.id^"_3"}>{status_to_string(msg.st)}</td>
								<td width="50px">
									{if(msg.st || msg.tc == 4){
										<input id=#{msg.id^"_4"} class="btn btn-primary" type="button" value="Join"
											disabled="disabled" onclick={function(_){Client.goto("/game/{msg.id}")}}/>
									 }else {
										<input id=#{msg.id^"_4"} class="btn btn-primary" type="button" value="Join"
											onclick={function(_){Client.goto("/game/{msg.id}")}}/>
									 }
									}
								</td>
							</tr>
						},Game.get_game_list())}
					</table>
				</div>
			</div>
			</>,
			<>
			<meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no">
			<link rel="stylesheet" type="text/css" href="/resources/css/page.css" media="only screen and (min-width:800px)">
			<link rel="stylesheet" type="text/css" href="/resources/css/page_small.css"
				  media="only screen and (min-width:240px) and (max-width:800px)">
			</>,
			{success},[]
		);
	}
}
