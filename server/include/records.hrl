-record(client, {
	id,
	state,
	socket,
	gameServerPID,
	playerPID,
	inputBuffer
}).

-record(entity, {
	id, 
	controller, 
	representation, 
	driver, 
	positionX, 
	positionY, 
	velocityX, 
	velocityY, 
	angle, 
	width, 
	height,
	pid,
	statePID
}).

-record(driverState, {
	entityID,
    positionX,
    positionY,
    north,
    south,
    west,
    east,
    fire,
    secondary,
    mouseX,
    mouseY
}).

-record(state, {
	worldBoundsX,
   	worldBoundsY,
    worldBoundsWidth,
    worldBoundsHeight,
  	entityCount,
    entities,
    subscribers
}).
