module engine


import gamnit::flat
import gamnit::display_linux
import physic_engine



redef class App	

	# Instancie la classe contenant les lois physiques
	var physics = new Physics

	# Instancie les entités présents dans notre référentiel
	var entity1 = new Carre1
	var entity2 = new Carre2

	#  Instancie un tableau contenant les entités présentent dans le référentiel
	var tableau_entity = new Array[Entity]

	# Instancie les textures d'images
	var carre1 = new Texture ("images/carre.jpg")
	var carre2 = new Texture ("images/carre.jpg")

	# Instancie les sprites, prend en paramètre une texture et des coordonnées
	var sprite1 = new Sprite (carre1, entity1.coord) 
	var sprite2 = new Sprite (carre2, entity2.coord)



	redef fun on_create
	do		
		super

		# Charge les textures d'image
		carre1.load	
		carre2.load

		# Charge les sprites
		sprites.add sprite1 
		sprites.add sprite2

		# Ajoute les entités au tableau des entités
		tableau_entity.add(entity1)	
		tableau_entity.add(entity2)

		# Ajoute les entités du tableau d'entités à la liste des entités de chaque entités
		for e in tableau_entity do
			e.list_entity = tableau_entity
		end
	end


	redef fun update(dt)
	do	
		# Calcule le mouvement à l'instant t des entités
		entity2.calc_direction(dt)
		entity1.calc_direction(dt)

		# Actualise le mouvement des entités
		entity1.update(dt)	
		entity2.update(dt)	


	end


	redef fun accept_event(event)
	do
		if event isa QuitEvent then
			exit 0
		else if event isa KeyEvent then
			var rot = event.rotation
			if rot == 1.0 then	
			else if rot == -1.0 then
			else if rot == 0.0 then
			end
		
		else if event isa PointerEvent then
			var tableau = event.test
			print tableau
		end
		return false
	end

end


redef class KeyEvent

	# How does this event affect the ship thrust?
	fun thrust: Float
	do
		if is_arrow_up or name == "w" then return 1.0
		return 0.0
	end

	# How does this event affect the ship thrust?
	fun rotation: Float
	do
		if is_arrow_right or name == "d" then return 1.0
		if is_arrow_left or name == "a" then return -1.0
		return 0.0
	end
end

redef class PointerEvent 

	fun test : Array[Float]
	do
		var retour = new Array[Float]
		if pressed == true then 
			retour.add(x)
			retour.add(y)
		
		else if depressed == true then
			retour.add(x)
			retour.add(y)
		end		

		return retour
	end


end

class Carre1
 

	super Entity

	redef var v_x : Float = 0.0 is writable
	redef var v_y : Float = 0.0 is writable
	redef var width : Float = 45.0 is writable
	redef var height : Float = 45.0 is writable
	redef var masse : Float = 1.0 is writable
	redef var coord = new Point3d[Float](0.0,500.0,0.0) is writable
	redef var result : Array[Float] = [0.0,0.0] is writable

end


class Carre2 


	super Entity

	redef var coord = new Point3d[Float](-250.0,-250.0,0.0) is writable
	redef var v_x : Float = 1.0 is writable
	redef var v_y : Float = 1.0 is writable
	redef var width : Float = 45.0 is writable
	redef var height : Float = 45.0 is writable
	redef var masse : Float = 1.0 is writable
	redef var result : Array[Float] = [1.0,1.0] is writable

end