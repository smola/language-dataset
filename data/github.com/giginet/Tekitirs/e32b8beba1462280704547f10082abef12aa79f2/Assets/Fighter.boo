import UnityEngine

class Fighter(MonoBehaviour): 
	public bulletPrefab as GameObject
	public bulletSpeed as single
	
	def Start ():
		pass
	
	def Update ():
		# Shoot a bullet
		if Input.GetButtonDown("Fire1"):
			bullet as GameObject = Instantiate(bulletPrefab, transform.position, Quaternion.identity)
			velocity as Vector3 =   transform.forward  * bulletSpeed
			bullet.rigidbody.velocity = velocity
		# Bank the body
		maxSpeed as single = GetComponent[of CharacterMotor]().movement.maxSidewaysSpeed
		currentSpeed as single = GetComponent[of CharacterController]().velocity.x
		transform.localRotation.eulerAngles.z = -45  * currentSpeed / maxSpeed