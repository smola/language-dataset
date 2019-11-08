module audiostreamerscrobbler.factories.PlayerControlThreadFactory

import audiostreamerscrobbler.factories.{Config, GroupFactory, PlayerDetectorThreadFactory, PlayerMonitorThreadFactory, ScrobblerHandlerFactory}
import audiostreamerscrobbler.threads.PlayerControlThread

function createPlayerControlThreadFactory = {
	let factory = DynamicObject("PlayerControlThreadFactory"):
		define("createPlayerControlThread", |this, scrobblerErrorHandler| -> createPlayerControlThreadInstance(scrobblerErrorHandler))

	return factory
}

local function createPlayerControlThreadInstance = |scrobblerErrorHandler| {
	let config = getConfig()

	let groupFactory = createGroupFactory()
	let playerDetectorThreadFactory = createPlayerDetectorThreadFactory()
	let playerMonitorThreadFactory = createPlayerMonitorThreadFactory()

	let scrobblerHandlerFactory = createScrobblerHandlerFactory()
	let scrobblerHandler = scrobblerHandlerFactory: createScrobblerHandler(scrobblerErrorHandler)

	return createPlayerControlThread(groupFactory, playerDetectorThreadFactory, playerMonitorThreadFactory, scrobblerHandler, config)
}
