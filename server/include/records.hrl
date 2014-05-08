-record(client, {
	id,
	state,
	socket,
	gameServerPID,
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
	height 
}).

-record(state, {
	worldBoundsX,
   	worldBoundsY,
    worldBoundsWidth,
    worldBoundsHeight,
  	entityCount = 0,
    entities = []
}).
