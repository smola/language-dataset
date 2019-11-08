# Model for brewnit
module model

import db_binding

redef class Fermentable
	redef fun to_s do
		return "{class_name}: {name}, potential: {potential}, colour: {colour}"
	end
end

redef class Yeast
	redef fun to_s do
		var ret = new Buffer
		ret.append "Yeast {brand}, {name}"
		if not aliases.is_empty then ret.append "; Aliases: "
		ret.append(aliases.join(", "))
		return ret.to_s
	end
end

redef class Equipment
	redef fun to_s do
		return "Equipment: {name}, Mean efficiency: {efficiency}%, Total volume: {volume}, trub losses: {trub_losses}, loss/hour: {boil_loss * 100.0}%"
	end
end

redef class FermentableProfile
	redef fun to_s do return "Fermentable: {fermentable}, quantity: {quantity}"
end

redef class Hop
	redef fun to_s do return "Hop: {name}"
end

redef class HopProfile
	redef fun to_s do return "{hop}, AA: {alpha_acid}, quantity: {quantity}, use: {if use == boil then "Boil" else "Dry Hop"}, time: {time}"
end

redef class Recipe
	redef fun to_s do
		var res = new Buffer
		res.append "Recipe {name}, volume: {target_volume}, mash_temperature: {target_mash_temp}, mash_time: {mash_time}\n{equipment}\n{yeast}\nMalts:\n"
		for i in malts do
			res.append i.to_s
			res.append "\n"
		end
		res.append "Hops:\n"
		for i in hops do
			res.append i.to_s
			res.append "\n"
		end
		return res.to_s
	end
end
