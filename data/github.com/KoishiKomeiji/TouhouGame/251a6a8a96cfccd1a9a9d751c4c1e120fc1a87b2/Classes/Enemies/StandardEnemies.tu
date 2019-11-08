unit
class StandardEnemyUnit
    export setMaxEnemies, maxEnemies, initializeEnemy, sameAngleEnemy, drawEnemies, enemyErase, stopAllEnemies, setWalls,
	getArrayPosition, getClosestEnemy, setAngleX, setAngleY, getAngleX, getAngleY, angleNormalize, setCharX, setCharY, setCharacterParam,
	setIsCharDead, getIsCharDead, charCenterX, charCenterY, getEnemyHP, setEnemyHP, enemyCounter, getNumAttacks, getAttackingEnemies,
	getAttackPattern, getEnemyX, getEnemyY, getEnemiesAlive, getEnemiesAlivePosition, EnemyTypeKillRadius, getEnemies, setAttackPattern,
	setFPSMultiplier, getAttackPattern2, setAttackPattern2, setAttackDelay, setVelocity, initializeBoss, setEnemyPosX, setEnemyPosY
    %%%%%%%%% EnemyType Variables %%%%%%%%%
    % 3 different types of enemies %
    % 1 Boss %
    % 1st variable is the picture %
    % 2nd variable used store its enemy number %
    % Use Syntax(1,Enemy(i)) for making your life easier %
    var EnemyTypes : array 1 .. 4 of int
    var EnemyTypeKillRadius : array 1 .. 4 of real
    var EnemyTypeCenter : array 1 .. 2, 1 .. 4 of real
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%%%%%%%%%%%%%%% VARIABLES %%%%%%%%%%%%%%%%%%%%%%
    %%%%%%%%% Enemies %%%%%    %%%%
    % Blue Enemy %
    EnemyTypes (1) := Pic.FileNew ("Images/e sprites/BlueEnemy.bmp")
    EnemyTypeKillRadius (1) := 11
    EnemyTypeCenter (1, 1) := 12
    EnemyTypeCenter (2, 1) := 10
    Pic.SetTransparentColour (EnemyTypes (1), brightgreen)
    % Red Enemy %
    EnemyTypes (2) := Pic.FileNew ("Images/e sprites/PurpleEnemy.bmp")
    EnemyTypeKillRadius (2) := 11
    EnemyTypeCenter (1, 2) := 12
    EnemyTypeCenter (2, 2) := 10
    Pic.SetTransparentColour (EnemyTypes (2), brightgreen)
    % Red Enemy %
    EnemyTypes (3) := Pic.FileNew ("Images/e sprites/RedEnemy.bmp")
    EnemyTypeKillRadius (3) := 11
    EnemyTypeCenter (1, 3) := 12
    EnemyTypeCenter (2, 3) := 10
    Pic.SetTransparentColour (EnemyTypes (3), brightgreen)
    % Yuka %
    EnemyTypes (4) := Pic.FileNew ("Images/e sprites/Yuka.bmp")
    EnemyTypeKillRadius (4) := 28
    EnemyTypeCenter (1, 4) := 19
    EnemyTypeCenter (2, 4) := 28
    Pic.SetTransparentColour (EnemyTypes (4), brightgreen)
    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % WALLS!!! %
    % 70 is the largest enemy size so far %
    var leftWall : int := 0 - 70
    var rightWall : int := 800 + 70
    var topWall : int := 600 + 70
    var bottomWall : int := 0 - 70

    %%%%%% Parameters used when a enemy is fired %%%%%%
    % Have a maximum of 1000 Enemies...not that I will use it all up %
    % Max Enemies %
    var maxEnemies : int := 1000
    % Enemy information %
    var Enemies : array 1 .. maxEnemies of int
    % Speed of the enemy and a counter to make it happen %
    % Was velocity a scalar or magnitude? I forget %
    % *Looks up 2 lines* Oh yeah, it was a scalar, it's speed with a DIRECTION %
    % Increases the x and y of a enemy in such a way it creates a direction/angle %
    var velocity : array 1 .. 2, 1 .. maxEnemies of real
    var vCounter : array 1 .. 2, 1 .. maxEnemies of real
    % Where the enemy is %
    var enemyPositions : array 1 .. 2, 1 .. maxEnemies of real
    % Which enemies are drawn %
    var enemyBoolean : array 1 .. maxEnemies of boolean
    for i : 1 .. maxEnemies
	enemyBoolean (i) := false
    end for
    % How many enemies there are %
    var enemyCounter : int := 0
    % Enemy HP %
    var enemyHP : array 1 .. maxEnemies of int
    % Enemy attack pattern code %
    var attackPattern : array 1 .. maxEnemies of int
    var attackPattern2 : array 1 .. maxEnemies of int
    % Interval in which enemy must wait until it may initiate attack, expressed in frames %
    var attackDelay : array 1 .. maxEnemies of int
    % To allow manager to see how many attacks there are %
    var numAttacks : int := 0
    % To allow manager to see which enemies are attacking %
    var attackingEnemies : array 1 .. maxEnemies of int
    % To make a timer to control when the enemy fires bullet(s), expressed in frames %
    var enemyTimer : array 1 .. maxEnemies of int
    for i : 1 .. maxEnemies
	enemyTimer (i) := 0
    end for
    % To figure out how many enemies are alive %
    var enemiesAlive : int
    % To figure out which enemies are alive %
    var enemiesAlivePosition : array 1 .. maxEnemies of int

    var FPSMultiplier : real := 1
    proc setFPSMultiplier (value : real)
	FPSMultiplier := value
    end setFPSMultiplier

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    %%%%%%%%%%%%%%%%%%%%%%
    % Character Funtions %
    %%%%%%%%%%%%%%%%%%%%%%
    % You only need to know where the character is, and if the enemy killed it %
    % Must be updated every frame %
    var charPosX, charPosY : real
    var charCenterX, charCenterY, charRad : real
    var isCharDead : boolean

    proc setCharX (posX : real)
	charPosX := posX
    end setCharX

    proc setCharY (posY : real)
	charPosY := posY
    end setCharY

    proc setCharacterParam (chCenterX, chCenterY, chRad : real)
	charCenterX := chCenterX
	charCenterY := chCenterY
	charRad := chRad
    end setCharacterParam

    proc setIsCharDead (isDead : boolean)
	isCharDead := isDead
    end setIsCharDead

    fcn getIsCharDead : boolean
	result isCharDead
    end getIsCharDead


    %%%%%%%%%%%%%%%%%%%%%%%
    % Get angle functions %
    %%%%%%%%%%%%%%%%%%%%%%%
    % Use pythagorean relationship to determine the angle from one co-ordinate to another %
    var angleX, angleY, angleMag : real := 0

    proc setAngleX (X : real)
	angleX := X
    end setAngleX

    fcn getAngleX : real
	result angleX
    end getAngleX

    proc setAngleY (Y : real)
	angleY := Y
    end setAngleY

    fcn getAngleY : real
	result angleY
    end getAngleY

    fcn angleMagnitude () : real
	result sqrt (angleX ** 2 + angleY ** 2)
    end angleMagnitude

    proc angleNormalize ()
	angleMag := angleMagnitude ()
	if getAngleX () not= angleMag and getAngleY () not= angleMag then
	    angleX := angleX / angleMag
	    angleY := angleY / angleMag
	end if
    end angleNormalize
    %%%%%%%%%%%%%%%%%%%%
    % Enemy functions %
    %%%%%%%%%%%%%%%%%%%%
    proc setMaxEnemies (number : int)
	maxEnemies := number
    end setMaxEnemies

    proc setWalls (left, right, up, down : int)
	leftWall := left - 30
	rightWall := right + 30
	topWall := up + 30
	bottomWall := down - 30
    end setWalls

    fcn getEnemies (arrayPos : int) : int
	result Enemies (arrayPos)
    end getEnemies

    fcn getNumAttacks : int
	result numAttacks
    end getNumAttacks

    fcn getAttackingEnemies (arrayPos : int) : int
	result attackingEnemies (arrayPos)
    end getAttackingEnemies

    fcn getAttackPattern (arrayPos : int) : int
	result attackPattern (arrayPos)
    end getAttackPattern

    proc setAttackPattern (arrayPos, attackPatternValue : int)
	attackPattern (arrayPos) := attackPatternValue
    end setAttackPattern

    fcn getAttackPattern2 (arrayPos : int) : int
	result attackPattern2 (arrayPos)
    end getAttackPattern2

    proc setAttackPattern2 (arrayPos, attackPatternValue : int)
	attackPattern2 (arrayPos) := attackPatternValue
    end setAttackPattern2

    proc setAttackDelay (arrayPos, value : int)
	attackDelay (arrayPos) := value div FPSMultiplier
    end setAttackDelay

    % Gets nearest false enemyBoolean location %
    fcn getArrayPosition : int
	for i : 1 .. maxEnemies
	    if enemyBoolean (i) = false then
		result i
	    end if
	end for
    end getArrayPosition

    % Gets the nearest true enemyBoolean location %
    fcn getClosestEnemy : int
	for i : 1 .. maxEnemies
	    if enemyBoolean (i) = true then
		result i
	    end if
	    % If none are present, result a number not within the max number of enemies %
	    result 100001
	end for
    end getClosestEnemy

    fcn getEnemiesAlive : int
	result enemiesAlive
    end getEnemiesAlive

    fcn getEnemiesAlivePosition (arrayPos : int) : int
	result enemiesAlivePosition (arrayPos)
    end getEnemiesAlivePosition

    fcn getEnemyX (arrayPos : int) : int
	result round (enemyPositions (1, arrayPos) + EnemyTypeCenter (1, Enemies (arrayPos)) + vCounter (1, arrayPos))
    end getEnemyX

    fcn getEnemyY (arrayPos : int) : int
	result round (enemyPositions (2, arrayPos) + EnemyTypeCenter (2, Enemies (arrayPos)) + vCounter (2, arrayPos))
    end getEnemyY

    proc setEnemyPosX (arrayPos : int)
	enemyPositions (1, arrayPos) := getEnemyX (arrayPos)
    end setEnemyPosX

    proc setEnemyPosY (arrayPos : int)
	enemyPositions (2, arrayPos) := getEnemyY (arrayPos)
    end setEnemyPosY

    % Deletes all enemies %
    proc stopAllEnemies
	for i : 1 .. maxEnemies
	    if enemyBoolean (i) = true then
		enemyBoolean (i) := false
	    end if
	end for
    end stopAllEnemies

    % To set the velocity, the vCounter will be reset for the velocity to work %
    proc setVelocity (speedX, speedY : real, arrayPos : int)
	velocity (1, arrayPos) := speedX * FPSMultiplier
	velocity (2, arrayPos) := speedY * FPSMultiplier
	vCounter (1, arrayPos) := 0
	vCounter (2, arrayPos) := 0
    end setVelocity

    % Stop the enemy from drawing %
    proc enemyErase (arrayPos : int)
	enemyBoolean (arrayPos) := false
	enemyCounter -= 1
	% This was not used with stop all enemies because we don't know whether the enemy killed the player yet %
	% You can leave the work to the procedure below %
    end enemyErase

    % Did the enemy hit the character %
    proc didEnemyHitChar (centerEnemyPositionX, centerEnemyPositionY : real, arrayPos : int)
	% If the enemy hits the character, kill it and delete the enemies %
	if sqrt (((charPosX + charCenterX - centerEnemyPositionX) ** 2) + ((charPosY + charCenterY - centerEnemyPositionY) ** 2)) <= EnemyTypeKillRadius (Enemies (arrayPos)) + charRad then
	    setIsCharDead (true)
	end if
    end didEnemyHitChar

    % To initialize a enemy to be ready for drawing %
    proc initializeEnemy (speedX, speedY : real, setPositionX, setPositionY, enemyType, HpValue, attack, delayTime : int)
	if enemyCounter < maxEnemies then
	    var arrayPos : int := getArrayPosition
	    setVelocity (speedX, speedY, arrayPos)
	    enemyPositions (1, arrayPos) := setPositionX - EnemyTypeCenter (1, enemyType)
	    enemyPositions (2, arrayPos) := setPositionY - EnemyTypeCenter (2, enemyType)
	    Enemies (arrayPos) := enemyType
	    enemyCounter += 1
	    enemyHP (arrayPos) := HpValue
	    attackPattern (arrayPos) := attack
	    attackPattern2 (arrayPos) := 0
	    attackDelay (arrayPos) := delayTime div FPSMultiplier
	    enemyBoolean (arrayPos) := true
	end if
    end initializeEnemy

    % To initialize a boss to be ready for drawing %
    proc initializeBoss (speedX, speedY : real, setPositionX, setPositionY, enemyType, HpValue, attack, delayTime, arrayPos : int)
	if enemyCounter < maxEnemies then
	    setVelocity (speedX, speedY, arrayPos)
	    enemyPositions (1, arrayPos) := setPositionX - EnemyTypeCenter (1, enemyType)
	    enemyPositions (2, arrayPos) := setPositionY - EnemyTypeCenter (2, enemyType)
	    Enemies (arrayPos) := enemyType
	    enemyCounter += 1
	    enemyHP (arrayPos) := HpValue
	    attackPattern (arrayPos) := attack
	    attackPattern2 (arrayPos) := 0
	    attackDelay (arrayPos) := delayTime div FPSMultiplier
	    enemyBoolean (arrayPos) := true
	end if
    end initializeBoss

    % To initialize an enemy coming at you %
    proc sameAngleEnemy (endX, endY, speed : real, startX, startY, enemyType, HpValue, attack, delayTime : int)
	setAngleX ((endX + EnemyTypeCenter (1, enemyType) - startX))
	setAngleY ((endY + EnemyTypeCenter (2, enemyType) - startY))
	angleNormalize
	initializeEnemy (getAngleX * speed, getAngleY * speed, startX, startY, enemyType, HpValue, attack, delayTime)
    end sameAngleEnemy

    fcn getEnemyHP (arrayPos : int) : int
	result enemyHP (arrayPos)
    end getEnemyHP

    proc setEnemyHP (arrayPos, HPvalue : int)
	enemyHP (arrayPos) := HPvalue
    end setEnemyHP

    % Draws enemies, updates them, and analyzes them %
    proc drawEnemies

	% Variable used for an attempt to speed up processing %
	var netPosition : array 1 .. 2 of int

	% Precalculations to make the enemy kill %
	var charPositionX : real := charPosX + charCenterX
	var charPositionY : real := charPosY + charCenterY
	var centerEnemyPositionX : real
	var centerEnemyPositionY : real
	% To reset counters %
	numAttacks := 0
	enemiesAlive := 0

	% Check if the enemy is dead %
	for i : 1 .. maxEnemies
	    if enemyBoolean (i) = true then
		if enemyHP (i) <= 0 then
		    enemyErase (i)
		end if
	    end if
	end for

	for i : 1 .. maxEnemies
	    if enemyBoolean (i) = true then
		% To stop when you get to the end of the enemy boolean %

		netPosition (1) := round (enemyPositions (1, i) + vCounter (1, i))
		netPosition (2) := round (enemyPositions (2, i) + vCounter (2, i))

		% To calculate the center of enemies and the character %
		centerEnemyPositionX := netPosition (1) + EnemyTypeCenter (1, Enemies (i))
		centerEnemyPositionY := netPosition (2) + EnemyTypeCenter (2, Enemies (i))

		% EnemyTypes(Enemies(i)) contain which enemy type it is, Enemies(i) show the type of enemy in the array %
		Pic.Draw (EnemyTypes (Enemies (i)), netPosition (1), netPosition (2), picMerge)
		vCounter (1, i) += velocity (1, i)
		vCounter (2, i) += velocity (2, i)

		% If the enemy goes off screen %
		if netPosition (1) < leftWall or netPosition (1) > rightWall then
		    enemyErase (i)
		elsif netPosition (2) < bottomWall or netPosition (2) > topWall then
		    enemyErase (i)
		end if

		% So that the enemy won't kill the character if the character is already dead but it will continue moving %
		if getIsCharDead = false then
		    didEnemyHitChar (centerEnemyPositionX, centerEnemyPositionY, i)
		end if

		% This creates a delay so the enemies will attack only after a set time %
		enemyTimer (i) += 1
		enemyTimer (i) := enemyTimer (i) mod attackDelay (i)
		if enemyTimer (i) = 0 then
		    numAttacks += 1
		    attackingEnemies (numAttacks) := i
		end if

		enemiesAlive += 1
		enemiesAlivePosition (enemiesAlive) := i

	    end if
	end for

    end drawEnemies

end StandardEnemyUnit
