TRAJECTORIES.CONSTANTS <- new.env(parent = emptyenv())

assign('LOGGER_THRESHOLD', 'INFO', envir=TRAJECTORIES.CONSTANTS)
assign('TRAJECTORIES_PACKAGE_NAME', 'Trajectories', envir=TRAJECTORIES.CONSTANTS)

assign('RARE_TRAJECTORIES', c(), envir=TRAJECTORIES.CONSTANTS) #used temporarily for faster check-up when counting trajectories
