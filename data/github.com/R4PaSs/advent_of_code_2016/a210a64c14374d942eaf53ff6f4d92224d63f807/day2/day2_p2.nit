class Cursor
	var pos = 5

	fun forbidden_r: Array[Int] do return once [1, 4, 9, 12, 13]
	fun forbidden_l: Array[Int] do return once [1, 2, 5, 10, 13]
	fun forbidden_u: Array[Int] do return once [5, 2, 1, 4, 9]
	fun forbidden_d: Array[Int] do return once [5, 10, 13, 12, 9]

	fun move(dir: Char) do
		if dir == 'R' then
			if forbidden_r.has(pos) then return
			pos += 1
		else if dir == 'L' then
			if forbidden_l.has(pos) then return
			pos -= 1
		else if dir == 'U' then
			if forbidden_u.has(pos) then return
			if pos == 13 or pos == 3 then
				pos -= 2
			else
				pos -= 4
			end
		else if dir == 'D' then
			if forbidden_d.has(pos) then return
			if pos == 1 or pos == 11 then
				pos += 2
			else
				pos += 4
			end
		else
			print "Unknown command `{dir}`"
		end
	end
end

var input = args[0].to_path.read_all
var il = input.split('\n')
var curse = new Cursor
for i in il do
	if i == "" then continue
	for j in i do curse.move(j)
	printn curse.pos.to_base(16)
end
print ""
