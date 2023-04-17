# Function to calculate time-dependent state sensitivities.
# See the .Rd file for a more detailed description of the input arguments.

state_sens = function(
  model_type,
  dynamic_fn,
  parms,
  reward_fn,
  terminal_fn,
  y_0,
  times,
  interpol = "spline",
  dynamic_fn_arglist = list(),
  reward_fn_arglist = list(),
  terminal_fn_arglist = list(), 
  state_ode_arglist = list(),
  adjoint_ode_arglist = list(),
  numDeriv_arglist = list()
  ){
  
  # Save a copy of dynamic_fn_arglist, since it will be modified later on,
  # but yet we need to return the original in the output.
  dynamic_fn_arglist_original = dynamic_fn_arglist
  
  
  
  
  
  # --------------------------
  # Check the input arguments.
  # --------------------------
  
  cat("Checking input arguments... ")
  
  # Check model_type first because it will influence the subsequent checks.
  if( !is.character(model_type) )
    stop("model_type must be character.")
  if( length(model_type) != 1 )
    stop("model_type must have length = 1.")
  if( !(model_type %in% c("continuous", "discrete")) )
    stop('model_type can only take values "continuous" or "discrete".')

  
  
  # Check that the input arguments (except parms) are of the correct classes.
  if( !is.function(dynamic_fn) )
    stop("dynamic_fn must be a function.")
  if( !is.function(reward_fn) )
    stop("reward_fn must be a function.")
  if( !is.function(terminal_fn) )
    stop("terminal_fn must be a function.")
  if( !is.numeric(y_0) )
    stop("y_0 must be numeric.")
  if( !is.numeric(times) )
    stop("times must be numeric.")
  if( !is.list(dynamic_fn_arglist) )
    stop("dynamic_fn_arglist must be a list.")
  if( !is.list(reward_fn_arglist) )
    stop("reward_fn_arglist must be a list.")
  if( !is.list(terminal_fn_arglist) )
    stop("terminal_fn_arglist must be a list.")
  if( !is.list(numDeriv_arglist) )
    stop("numDeriv_arglist must be a list.")
  
  if( model_type == "continuous" ){
    if( !is.character(interpol) )
      stop("interpol must be character.")
    if( !is.list(state_ode_arglist) )
      stop("state_ode_arglist must be a list.")  
        if( !is.list(adjoint_ode_arglist) )
      stop("adjoint_ode_arglist must be a list.")
  }
  
  
  
  # Check y_0.
  n_y = length(y_0)
  if( n_y < 1 )
    stop("y_0 must have length >= 1.")
  if( any(is.na(y_0)) )
    stop("y_0 cannot contain NA values.")
  
  
  
  # Check times.
  # Since we will use times[1] when checking parms, this check must be performed
  # before we check parms.
  n_t = length(times)
  if( n_t < 2 )
    stop("times must have length >= 2.")
  if( any(is.na(times)) )
    stop("times cannot contain NA values.")
  if( any(duplicated(times)) )
    stop("times cannot have duplicate elements.")
  if( is.unsorted(times) )
    stop("times must be sorted in ascending order.")
  
  if( model_type == "discrete" ){
    if( any(round(times) != times) | any(diff(times) != 1) )
      stop("times can only contain consecutive integer values.")
  }
  
  
  
  # Check parms. This is probably the most complicated check.
  if( missing(parms) ){
    # Allow parms to be missing, for consistency with ode.
    # However, we will set it to NULL so that we can return it in the output.
    parms = NULL
    
  } else if( is.null(parms) ){
    # If parms is NULL, do nothing.
    
  } else if( is.numeric(parms) ){
    # If parms is numeric, check whether it contains NA values.
    if( any(is.na(parms)) )
      stop("parms cannot contain NA values.")
    
  } else if( is.function(parms) ){
    # If parms is a function, make further checks.
    
    # Check whether the function only accepts one input argument.
    if( length(formals(parms)) != 1 )  
      stop("If parms is a function, it must only accept one input argument t.")
    # Check the output using the first time step.
    if( !is.numeric(parms(times[1])) )
      stop("If parms is a function, it must return a numeric output.")
    if( any(is.na(parms(times[1]))) )
      stop("If parms is a function, the output cannot contain NA values.")
    
  } else if( is.list(parms) ){
    # If parms is a list, make further checks.
    
    # Check whether every element in the list numeric or a function.
    # We use vapply because sapply doesn't simplify if parms is an empty list.
    num_elements = which( vapply(X=parms, FUN=is.numeric, FUN.VALUE=T) )  # Numeric elements.
    fun_elements = which( vapply(X=parms, FUN=is.function, FUN.VALUE=T) )  # Function elements.
    if( length(parms) != (length(num_elements) + length(fun_elements)) )
      stop("If parms is a list, every element must either be numeric or a function.")
    
    # For each numeric element, check whether it contains NA values.
    for(i in num_elements){
      if( any(is.na(parms[[i]])) )
        stop(paste("parms[[",i,"]] cannot contain NA values.", sep=""))
    }
    
    # For each function element, make further checks.
    for(i in fun_elements){
      # Check whether the function only accepts one input argument.
      if( length(formals(parms[[i]])) != 1 )
        stop(paste("If parms[[",i,"]] is a function, it must only accept one input argument t.", sep=""))
      # Check the output using the first time step.
      if( !is.numeric(parms[[i]](times[1])) )
        stop(paste("If parms[[",i,"]] is a function, it must return a numeric output.", sep=""))
      # Does its output contain NA values? Check using the first time step.
      if( any(is.na(parms[[i]](times[1]))) )
        stop(paste("If parms[[",i,"]] is a function, the output cannot contain NA values.", sep=""))
    }
    
  } else {
    # parms does not have an allowed structure.
    stop("Structure of parms not allowed. See help(state_sens) for allowed structures.")
    
  }
  
  
  
  # Check interpol.
  if( model_type == "continuous" ){
    if( length(interpol) != 1 )
      stop("interpol must have length = 1.")
    if( !(interpol %in% c("linear", "spline")) ) 
      stop('interpol can only take values "linear" or "spline".')
  }
  
  
  
  # Check the optional argument lists.
  if( any(names(dynamic_fn_arglist) == "") | any(duplicated(names(dynamic_fn_arglist))) )
    stop("All elements in dynamic_fn_arglist must be named, and the names cannot contain duplicates.")
  if( any(names(reward_fn_arglist) == "") | any(duplicated(names(reward_fn_arglist))) )
    stop("All elements in reward_fn_arglist must be named, and the names cannot contain duplicates.")
  if( any(names(terminal_fn_arglist) == "") | any(duplicated(names(terminal_fn_arglist))) )
    stop("All elements in terminal_fn_arglist must be named, and the names cannot contain duplicates.")
  if( any(names(numDeriv_arglist) == "") | any(duplicated(names(numDeriv_arglist))) )
    stop("All elements in numDeriv_arglist must be named, and the names cannot contain duplicates.")
  
  if( model_type == "continuous" ){
    if( any(names(state_ode_arglist) == "") | any(duplicated(names(state_ode_arglist))) )
      stop("All elements in state_ode_arglist must be named, and the names cannot contain duplicates.")
    if( any(names(adjoint_ode_arglist) == "") | any(duplicated(names(adjoint_ode_arglist))) )
      stop("All elements in adjoint_ode_arglist must be named, and the names cannot contain duplicates.")
    
    # Check that the user is not using the "iteration" method.
    if( !is.null(state_ode_arglist[["method"]]) ){
      if( state_ode_arglist[["method"]] == "iteration" )
        stop('state_ode_arglist[["method"]] cannot be "iteration" if model_type is "continuous.')
    }
    if( !is.null(adjoint_ode_arglist[["method"]]) ){
      if( adjoint_ode_arglist[["method"]] == "iteration" )
        stop('adjoint_ode_arglist[["method"]] cannot be "iteration" if model_type is "continuous.')
    }
  }

  
  
  # Check the user-defined functions.
  
  # Extract the argument names of the user-defined functions.
  dynamic_fn_argnames = names(formals(dynamic_fn))
  reward_fn_argnames = names(formals(reward_fn))
  terminal_fn_argnames = names(formals(terminal_fn))
  
  # Check that the user-defined functions have the required number of arguments.
  if(length(dynamic_fn_argnames) < 3)
    stop("dynamic_fn must accept at least three input arguments t, y and parms.")
  if(length(reward_fn_argnames) < 2)
    stop("reward_fn must accept at least two input arguments t and y.")
  if(length(terminal_fn_argnames) < 1)
    stop("terminal_fn must accept at least one input argument y.")
  
  # Obtain sample output from the user-defined functions.
  # Evaluate at times[1] and y_0.
  # Note that assigning an element to NULL automatically creates a list.
  dynamic_fn_arglist[[dynamic_fn_argnames[1]]] = times[1]
  dynamic_fn_arglist[[dynamic_fn_argnames[2]]] = y_0
  dynamic_fn_arglist[[dynamic_fn_argnames[3]]] = parms
  dynamic_fn_output = do.call(dynamic_fn, dynamic_fn_arglist)
  reward_fn_arglist[[reward_fn_argnames[1]]] = times[1]
  reward_fn_arglist[[reward_fn_argnames[2]]] = y_0
  reward_fn_output = do.call(reward_fn, reward_fn_arglist)
  terminal_fn_arglist[[terminal_fn_argnames[1]]] = times[1]
  terminal_fn_output = do.call(terminal_fn, terminal_fn_arglist)
  
  # Check the output from dynamic_fn.
  if( !is.list(dynamic_fn_output) )
    stop("dynamic_fn must return a list.")
  if( !is.numeric(dynamic_fn_output[[1]]) | (length(dynamic_fn_output[[1]]) != n_y) )
    stop("The first element of the list returned by dynamic_fn must be numeric, and have the same length as y_0.")
  if( any(is.na(dynamic_fn_output[[1]])) )
    stop("The first element of the list returned by dynamic_fn cannot contain NA values.")
  if( length(dynamic_fn_output) > 1 ){
    if( !is.numeric(unlist(dynamic_fn_output[-1])) )
      stop("Each optional element of the list returned by dynamic_fn must be numeric.")
  }
  
  # Check the output from reward_fn and terminal_fn.
  if( !is.numeric(reward_fn_output) | (length(reward_fn_output) != 1) )
    stop("reward_fn must return a single number.")
  if( !is.numeric(terminal_fn_output) | (length(terminal_fn_output) != 1) )
    stop("terminal_fn must return a single number.")
  
  cat("Done!\n\n")
  
  
  
  
  
  # -----------------------------------------------
  # Solve the dynamic equations for the state ODEs.
  # -----------------------------------------------
  cat("Solving the dynamic equations for the state variables... ")
  
  # The following assignments have been commented out because they were already
  # made during the input checks.
  # 
  # # Length of y_0 and times.
  # n_y = length(y_0)
  # n_t = length(times)
  # 
  # # Extract the argument names of the user-defined functions.
  # dynamic_fn_argnames = names(formals(dynamic_fn))
  # reward_fn_argnames = names(formals(reward_fn))
  # terminal_fn_argnames = names(formals(terminal_fn))
  
  # Add parms to dynamic_fn_arglist once and for all.
  dynamic_fn_arglist[[dynamic_fn_argnames[3]]] = parms
  
  # Duplicate dynamic_fn, except with parms and any additional arguments
  # already pre-assigned. By pre-assigning these arguments rather than relying
  # on ode to do so, we avoid any potential conflicts involving argument names.
  # Note that changing dynamic_fn_arglist within dynamic_fn1 will not affect
  # dynamic_fn_arglist outside the local environment of dynamic_fn1.
  dynamic_fn1 = function(t, y, parms_unused){
    dynamic_fn_arglist[[dynamic_fn_argnames[1]]] = t
    dynamic_fn_arglist[[dynamic_fn_argnames[2]]] = y
    return( do.call(dynamic_fn, dynamic_fn_arglist) )
  }  
  
  # Solve the dynamic equations for the state variables.
  # For discrete-time models, we will simply use ode with method="iteration".
  if( model_type == "discrete" ){
    # Unlike "lsoda", the "iteration" method in ode requires that parms be
    # specified, and that it cannot be NULL. Since this argument is used to
    # specify parms_unused in dynamic_fn1, we set it to NA.
    state_ode_arglist[["parms"]] = NA    
    state_ode_arglist[["method"]] = "iteration"
  }
  state_ode_arglist[["y"]] = y_0
  state_ode_arglist[["times"]] = times
  state_ode_arglist[["func"]] = dynamic_fn1
  state = do.call(what="ode", args=state_ode_arglist)
  
  # Check that the solutions do not contain NA values.
  # We will not check the additional columns corresponding to "global values".
  if( any(is.na(state[, 2:(n_y+1)])) )
    stop("The solutions of the dynamic equations contain NA values. Check dynamic_fn for errors.")
  
  cat("Done!\n\n")
  
  
  
  
  # -----------------------------------------------------------------------------
  # Continous-time models: Define interpolation function for the state variables.
  # -----------------------------------------------------------------------------
  
  if( model_type == "continuous" ){
    
    cat("Defining interpolation function for the state variables... ")
    
    # Neither splinefun() nor approxfun() can interpolate vector-valued functions,
    # so use apply() with splinefun() or approxfun() to create a list of
    # interpolation functions, one for each component of the state vector.
    # Note that drop=F is needed in case there is only one state variable.
    if(interpol == "spline"){
      state_fn_list = apply(X=state[, 2:(n_y+1), drop=F], MARGIN=2,
                            FUN=splinefun, x=times)
      
    } else if(interpol == "linear"){
      state_fn_list = apply(X=state[, 2:(n_y+1), drop=F], MARGIN=2,
                            FUN=approxfun, x=times, rule=2)
    }
    
    # Using the list of functions, create a single function that returns a
    # vector-valued output.
    state_fn = function(t){
      return( sapply(state_fn_list, do.call, list(t)) )
    }
    
    cat("Done!\n\n")
    
  }

  
  
  
  
  # -------------------------------------------------------
  # Defining the adjoint equations and terminal conditions.
  # -------------------------------------------------------
  
  cat("Defining the adjoint equations and terminal conditions... ")
  
  # Since the Hamiltonian is linear in the adjoint variables, we can create a
  # function representing the adjoint equations by numerically evaluating the
  # Jacobian of dynamic_fn, and the gradient of reward_fn.
  
  # Duplicate dynamic_fn, except with parms and any additional arguments
  # already pre-assigned, and returning only the first element of the list.
  # This is because numDeriv requires the output to be a vector.
  dynamic_fn2 = function(t, y){
    dynamic_fn_arglist[[dynamic_fn_argnames[1]]] = t
    dynamic_fn_arglist[[dynamic_fn_argnames[2]]] = y
    return( do.call(dynamic_fn, dynamic_fn_arglist)[[1]] )
  }
  
  # Duplicate reward_fn and terminal_fn, except with any additional arguments
  # already pre-assigned. By pre-assigning these arguments rather than relying
  # on ode to do so, we avoid any potential conflicts involving argument names.
  reward_fn2 = function(t, y){
    reward_fn_arglist[[reward_fn_argnames[1]]] = t
    reward_fn_arglist[[reward_fn_argnames[2]]] = y
    return( do.call(reward_fn, reward_fn_arglist) )
  }
  
  terminal_fn2 = function(y){
    terminal_fn_arglist[[terminal_fn_argnames[1]]] = y
    return( do.call(terminal_fn, terminal_fn_arglist) )
  }
  
  
  
  # Argument lists for jacobian applied to dynamic_fn2, and grad applied to
  # reward_fn2.
  jacobian_arglist = numDeriv_arglist
  jacobian_arglist[["func"]] = dynamic_fn2
  grad_reward_arglist = numDeriv_arglist
  grad_reward_arglist[["func"]] = reward_fn2
  
  # Adjoint equations.
  if( model_type == "continuous" ){
    
    # Ordinary differential equations.
    adjoint_fn = function(t, lambda, parms_unused){
      
      # State variables at time t.
      y = state_fn(t)
      
      # Calculate the Jacobian of dynamic_fn2.
      # Note that dynamic_fn2 has been defined such that the time argument is
      # named "t".
      jacobian_arglist[["x"]] = y
      jacobian_arglist[["t"]] = t
      jacob_state_ode = do.call(what="jacobian", args=jacobian_arglist)
      
      # Calculate the gradient of reward_fn2.
      # Note that reward_fn2 has been defined such that the time argument is
      # named "t".
      grad_reward_arglist[["x"]] = y
      grad_reward_arglist[["t"]] = t
      grad_reward = do.call(what="grad", args=grad_reward_arglist)
      
      # Return the adjoint equations.
      list( - grad_reward - matrix(lambda, nrow=1) %*% jacob_state_ode )
    }
    
  } else if( model_type == "discrete" ){
    
    # Backward recurrence equations.
    # Because of the way the "iteration" method works, it is better to have a
    # function that takes in lambda_{t} and return lambda_{t-1}, rather than a
    # function that takes in lambda_{t+1} and returns lambda_{t}.
    adjoint_fn = function(t, lambda, parms_unused){
      
      # During the backward solution, since lambda_{t=times[1]} is obtained by
      # evaluating adjoint_fn at times[2], there is really no need for the 
      # "iteration" method to evaluate adjoint_fn at times[1].
      # Unfortunately, it will still do so just to obtain "global values".
      # We need to handle this unwanted iteration step as an exception.
      
      if( t == times[1] ){
        
        list( as.numeric( rep(NA, n_y) ) )
        
      } else {
        
        tminus1 = t - 1
        
        # State variables at time t - 1.
        y_tminus1 = state[which(times==tminus1), 2:(n_y+1)]
        
        # Calculate the Jacobian of dynamic_fn2.
        # Note that dynamic_fn2 has been defined such that the time argument is
        # named "t".
        jacobian_arglist[["x"]] = y_tminus1
        jacobian_arglist[["t"]] = tminus1
        jacob_state_ode = do.call(what="jacobian", args=jacobian_arglist)
        
        # Calculate the gradient of reward_fn2.
        # Note that reward_fn2 has been defined such that the time argument is
        # named "t".
        grad_reward_arglist[["x"]] = y_tminus1
        grad_reward_arglist[["t"]] = tminus1
        grad_reward = do.call(what="grad", args=grad_reward_arglist)
        
        # Return the adjoint equations.
        list( grad_reward + matrix(lambda, nrow=1) %*% jacob_state_ode )
        
      }
      
    }
  
  }
  
  
  
  # Argument lists for grad applied to terminal_fn2.
  grad_terminal_arglist = numDeriv_arglist
  grad_terminal_arglist[["func"]] = terminal_fn2
  
  # Calculate the gradient of terminal_fn to obtain the terminal conditions.
  if( model_type == "continuous" ){
    grad_terminal_arglist[["x"]] = state_fn(times[n_t])
    
  } else if( model_type == "discrete" ){
    grad_terminal_arglist[["x"]] = state[n_t, 2:(n_y+1)]
    
  }
  
  lambda_terminal = do.call(what="grad", args=grad_terminal_arglist)
  
  cat("Done!\n\n")
  
  
  
  
  
  # ------------------------------
  # Solving the adjoint equations.
  # ------------------------------
  
  cat("Solving the adjoint equations for the adjoint variables... ")
  
  # For discrete-time models, we will simply use ode with method="iteration".
  if( model_type == "discrete" ){
    # Unlike "lsoda", the "iteration" method in ode requires that parms be
    # specified, and that it cannot be NULL. Since this argument is used to
    # specify parms_unused in adjoint_fn, we set it to NA.
    adjoint_ode_arglist[["parms"]] = NA    
    adjoint_ode_arglist[["method"]] = "iteration"
  }
  adjoint_ode_arglist[["y"]] = lambda_terminal
  adjoint_ode_arglist[["times"]] = rev(times)   # Reversed time steps.
  adjoint_ode_arglist[["func"]] = adjoint_fn
  tdss_rev = do.call(what="ode", args=adjoint_ode_arglist)
  cat("Done!\n\n")
  
  # Reverse the row ordering.
  tdss = tdss_rev[n_t:1,]
  
  # Check that the solutions do not contain NA values.
  if( any(is.na(tdss[, 2:(n_y+1)])) )
    stop("The solutions of the adjoint equations contain NA values. Check dynamic_fn, reward_fn or terminal_fn for errors.")

  cat("Time-dependent state sensitivity calculations complete.\n\n")
  
  return(list(
    model_type=model_type,
    dynamic_fn=dynamic_fn,
    parms=parms,
    dynamic_fn_arglist=dynamic_fn_arglist_original,
    times=times,
    state=state[,-1, drop=F],  # drop=F so it remains a matrix if we only have one state variable.
    tdss=tdss[,-1, drop=F]     # Ditto.
    ))
}