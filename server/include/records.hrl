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
	height,
	statePID
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
