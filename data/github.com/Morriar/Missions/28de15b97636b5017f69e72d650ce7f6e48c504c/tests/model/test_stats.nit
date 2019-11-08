# Copyright 2017 Alexandre Terrasa <alexandre@moz-code.org>.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

module test_stats is test

import test_achievements

class StatsTest
	super AchievementsTest
	test

	fun test_player_stats is test do
		var t1 = new_track("t1")
		var m1 = new_mission(t1, "m1")
		var m2 = new_mission(t1, "m2")
		m2.parents.add m1.id
		config.missions.save m2
		var m3 = new_mission(t1, "m3")
		m3.parents.add m2.id
		config.missions.save m3

		var t2 = new_track("t2")
		var m4 = new_mission(t2, "m4")
		var m5 = new_mission(t2, "m5")

		var player = new_player("id1")

		var stats = player.stats(config)

		assert stats.score == 0
		assert stats.missions_count == 5
		assert stats.missions_open == 3
		assert stats.missions_locked == 2
		assert stats.missions_success == 0

		var status1 = player.mission_status(config, m1)
		status1.status = "success"
		config.missions_status.save(status1)

		stats = player.stats(config)
		assert stats.score == 10
		assert stats.missions_count == 5
		assert stats.missions_open == 4
		assert stats.missions_locked == 1
		assert stats.missions_success == 1

		var a1 = new_achievement(player, "a1")
		player.add_achievement(config, a1)
		stats = player.stats(config)
		assert stats.score == 20
		assert stats.achievements == 1
	end

	fun test_player_ranking is test do
		var t1 = new_track("t3")
		var m1 = new_mission(t1, "m6")
		m1.solve_reward = 10000
		config.missions.save m1

		var p1 = new_player("id2")
		var p2 = new_player("id3")

		var status1 = p1.mission_status(config, m1)
		status1.status = "success"
		config.missions_status.save(status1)

		var ranking = config.players_ranking
		assert ranking.first.player == p1
		assert ranking.last.player == p2
	end
end

redef fun after_module do super
