/* SmartFridge.  Much todo
*/
/obj/machinery/smart_fridge
	name = "SmartFridge"
	icon = 'icons/obj/vending.dmi'
	icon_state = "smartfridge"
	layer = 2.9
	density = 1
	anchored = 1
	use_power = 1
	idle_power_usage = 5
	active_power_usage = 100
	flags = NOREACT
	var/global/max_n_of_items = 200
	var/item_quants = list()
	var/ispowered = 1 //starts powered
	var/isbroken = 0


/obj/machinery/smart_fridge/power_change()
	if( powered() )
		src.ispowered = 1
		stat &= ~NOPOWER
		if(!isbroken)
			icon_state = "smartfridge"
	else
		spawn(rand(0, 15))
		src.ispowered = 0
		stat |= NOPOWER
		if(!isbroken)
			icon_state = "smartfridge-off"


/*******************
*   Item Adding
********************/

/obj/machinery/smart_fridge/attackby(var/obj/item/O as obj, var/mob/user as mob)
	if(!src.ispowered)
		user << "\red The [src] is unpowered and useless."
		return
	if(istype(O,/obj/item/weapon/reagent_containers/food/snacks/grown/))
		if (contents.len>=max_n_of_items)
			user << "\red This [src] is full of ingredients, you cannot put more."
			return 1
		/*todo: plantbag*/
		else
			user.before_take_item(O)
			O.loc = src
			if(item_quants[O.name])
				item_quants[O.name]++
			else
				item_quants[O.name]=1
			user.visible_message( \
				"\blue [user] has added \the [O] to \the [src].", \
				"\blue You add \the [O] to \the [src].")
	else if (istype(O, /obj/item/weapon/plant_bag))
		for (var/obj/item/weapon/reagent_containers/food/snacks/grown/G in O.contents)
			O.contents -= G
			G.loc = src
			if(item_quants[G.name])
				item_quants[G.name]++
			else
				item_quants[G.name]=1
		user.visible_message( \
			"\blue [user] loads \the [src] with \the [O].", \
			"\blue You load \the [src] with \the [O].")

	else
		user << "\red The [src] smartly refuses [O]."
		return 1
	src.updateUsrDialog()

/obj/machinery/smart_fridge/attack_paw(mob/user as mob)
	return src.attack_hand(user)

/obj/machinery/smart_fridge/attack_ai(mob/user as mob)
	return 0

/obj/machinery/smart_fridge/attack_hand(mob/user as mob)
	user.machine = src
	interact(user)

/*******************
*   SmartFridge Menu
********************/

/obj/machinery/smart_fridge/proc/interact(mob/user as mob)
	if(!src.ispowered)
		return

	var/dat = "<TT><b>Select an item:</b><br>"

	if (contents.len == 0)
		dat += "<font color = 'red'>No product loaded!</font>"
	else
		for (var/O in item_quants)
			if(item_quants[O] > 0)
				var/N = item_quants[O]
				dat += "<FONT color = 'blue'><B>[capitalize(O)]</B>:"
				dat += " [N] </font>"
				dat += "<a href='byond://?src=\ref[src];vend=[O]'>Vend</A>"
				dat += "<br>"

		dat += "</TT>"
	user << browse("<HEAD><TITLE>SmartFridge Supplies</TITLE></HEAD><TT>[dat]</TT>", "window=smartfridge")
	onclose(user, "smartfridge")
	return

/obj/machinery/smart_fridge/Topic(href, href_list)
	if(..())
		return

	usr.machine = src

	var/N = href_list["vend"]

	if(item_quants[N] <= 0) // Sanity check, there are probably ways to press the button when it shouldn't be possible.
		return

	item_quants[N] -= 1
	for(var/obj/O in contents)
		if(O.name == N)
			O.loc = src.loc
			break
	src.updateUsrDialog()
	return
