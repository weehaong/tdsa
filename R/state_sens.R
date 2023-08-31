# Function to calculate time-dependent state sensitivities.
#
# Since the input arguments are rather complicated, it is easier to "handcraft"
# the .Rd file than to use roxygen2 to create a documentation with the desired
# look.
# Therefore, to avoid duplicate and possibly conflicting information, the
# function description should be placed directly in the .Rd file and not here.

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
  numDeriv_arglist = list(),
  verbose = TRUE
  ){
  
  # Save a copy of dynamic_fn_arglist, since it will be modified later on,
  # but yet we need to return the original in the output.
  dynamic_fn_arglist_original = dynamic_fn_arglist
  
  
  
  
  
  # --------------------------
  # Check the input arguments.
  # --------------------------
  
  # Check verbose first so we know whether to display the progress indicators
  # for the subsequent checks.
  assert_choice(scalar=verbose, choices=c(TRUE, FALSE))
  
  if(verbose)cat("Checking input arguments... ")
  
  
  
  # Check the other input arguments.
  assert_choice(scalar=model_type, choices=c("continuous", "discrete"))
  assert_y_0(y_0=y_0)
  assert_arglist(arglist=dynamic_fn_arglist)
  assert_arglist(arglist=reward_fn_arglist)
  assert_arglist(arglist=terminal_fn_arglist)
  assert_arglist(arglist=numDeriv_arglist)

  # These checks should only be done after model_type has been checked.
  assert_times(times=times, model_type=model_type)
  
  if( model_type == "continuous" ){
    assert_choice(scalar=interpol, choices=c("spline", "linear"))
    
    assert_arglist(arglist=state_ode_arglist)
    assert_arglist(arglist=adjoint_ode_arglist)
    
    # Check that the user is not using the "iteration" method if the model type
    # is continuous.
    if( !is.null(state_ode_arglist[["method"]]) )
      if( state_ode_arglist[["method"]] == "iteration" )
        stop('state_ode_arglist[["method"]] cannot be "iteration" if model_type is "continuous.')
    if( !is.null(adjoint_ode_arglist[["method"]]) )
      if( adjoint_ode_arglist[["method"]] == "iteration" )
        stop('adjoint_ode_arglist[["method"]] cannot be "iteration" if model_type is "continuous.')
  }
  
  
  
  # Check parms. This should only be done after times has been checked.
  if( missing(parms) ){
    # Allow parms to be missing, for consistency with ode.
    # However, we will set it to NULL so that we can return it in the output.
    parms = NULL
    
  } else if( is.null(parms) ){
    # If parms is NULL, do nothing.
    
  } else {
    # Run the other checks for parms.
    assert_parms(parms=parms, times=times)
  }
  

  
  # Check dynamic_fn, reward_fn and terminal_fn. This should only be done after
  # parms, y_0, times and the optional argument lists have been checked.
  assert_dynamic_fn(dynamic_fn=dynamic_fn, parms=parms, y=y_0, times=times,
                    dynamic_fn_arglist=dynamic_fn_arglist,
                    length_label="length(y_0)")
  assert_reward_fn(reward_fn=reward_fn, y=y_0, times=times,
                   reward_fn_arglist=reward_fn_arglist)
  assert_terminal_fn(terminal_fn=terminal_fn, y=y_0,
                     terminal_fn_arglist=terminal_fn_arglist)
  
  

  if(verbose)cat("Done!\n\n")
  
  
  
  
  
  # -----------------------------------------------
  # Solve the dynamic equations for the state ODEs.
  # -----------------------------------------------
  if(verbose)cat("Solving the dynamic equations for the state variables... ")
  
  # Length of y_0 and times.
  n_y = length(y_0)
  n_t = length(times)

  # Extract the argument names of the user-defined functions.
  dynamic_fn_argnames = names(formals(dynamic_fn))
  reward_fn_argnames = names(formals(reward_fn))
  terminal_fn_argnames = names(formals(terminal_fn))
  
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
  state = do.call(what=deSolve::ode, args=state_ode_arglist)
  
  # Check that the solutions do not contain NA values.
  # We will not check the additional columns corresponding to "global values".
  if( any(is.na(state[, 2:(n_y+1)])) )
    stop("The solutions of the dynamic equations contain NA values. Check dynamic_fn for errors.")
  
  if(verbose)cat("Done!\n\n")
  
  
  
  
  # -----------------------------------------------------------------------------
  # Continous-time models: Define interpolation function for the state variables.
  # -----------------------------------------------------------------------------
  
  if( model_type == "continuous" ){
    
    if(verbose)cat("Defining interpolation function for the state variables... ")
    
    # Neither splinefun() nor approxfun() can interpolate vector-valued functions,
    # so use apply() with splinefun() or approxfun() to create a list of
    # interpolation functions, one for each component of the state vector.
    # Note that drop=FALSE is needed in case there is only one state variable.
    if(interpol == "spline"){
      state_fn_list = apply(X=state[, 2:(n_y+1), drop=FALSE], MARGIN=2,
                            FUN=splinefun, x=times)
      
    } else if(interpol == "linear"){
      state_fn_list = apply(X=state[, 2:(n_y+1), drop=FALSE], MARGIN=2,
                            FUN=approxfun, x=times, rule=2)
    }
    
    # Using the list of functions, create a single function that returns a
    # vector-valued output.
    state_fn = function(t){
      return( sapply(state_fn_list, do.call, list(t)) )
    }
    
    if(verbose)cat("Done!\n\n")
    
  }

  
  
  
  
  # -------------------------------------------------------
  # Defining the adjoint equations and terminal conditions.
  # -------------------------------------------------------
  
  if(verbose)cat("Defining the adjoint equations and terminal conditions... ")
  
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
      jacob_state_ode = do.call(what=numDeriv::jacobian, args=jacobian_arglist)
      
      # Calculate the gradient of reward_fn2.
      # Note that reward_fn2 has been defined such that the time argument is
      # named "t".
      grad_reward_arglist[["x"]] = y
      grad_reward_arglist[["t"]] = t
      grad_reward = do.call(what=numDeriv::grad, args=grad_reward_arglist)
      
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
        jacob_state_ode = do.call(what=numDeriv::jacobian, args=jacobian_arglist)
        
        # Calculate the gradient of reward_fn2.
        # Note that reward_fn2 has been defined such that the time argument is
        # named "t".
        grad_reward_arglist[["x"]] = y_tminus1
        grad_reward_arglist[["t"]] = tminus1
        grad_reward = do.call(what=numDeriv::grad, args=grad_reward_arglist)
        
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
  
  lambda_terminal = do.call(what=numDeriv::grad, args=grad_terminal_arglist)
  
  if(verbose)cat("Done!\n\n")
  
  
  
  
  
  # ------------------------------
  # Solving the adjoint equations.
  # ------------------------------
  
  if(verbose)cat("Solving the adjoint equations for the adjoint variables... ")
  
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
  tdss_rev = do.call(what=deSolve::ode, args=adjoint_ode_arglist)
  if(verbose)cat("Done!\n\n")
  
  # Reverse the row ordering.
  tdss = tdss_rev[n_t:1,]
  
  # Check that the solutions do not contain NA values.
  if( any(is.na(tdss[, 2:(n_y+1)])) )
    stop("The solutions of the adjoint equations contain NA values. Check dynamic_fn, reward_fn or terminal_fn for errors.")

  if(verbose)cat("Time-dependent state sensitivity calculations complete.\n\n")
  
  return(list(
    model_type=model_type,
    dynamic_fn=dynamic_fn,
    parms=parms,
    dynamic_fn_arglist=dynamic_fn_arglist_original,
    times=times,
    state=state[,-1, drop=FALSE],  # drop=FALSE so it remains a matrix if we only have one state variable.
    tdss=tdss[,-1, drop=FALSE]     # Ditto.
    ))
}