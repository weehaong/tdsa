# Helper functions to perform checks on input arguments.
#
# - In each of these functions, the order of the checks is important, because
#   each check often ensures that the conditions used in the later checks work
#   properly, i.e. they do not involve undefined methods, nor return NA or a 
#   logical with length != 1.
#
# - In each of these functions, prefix is a string attached to the front of the 
#   error message, to make the error message more informative. If non-empty, it
#   should end with a space because we are using paste0() and not paste().





# Function to check scalar (length-1) choices. This function was motivated by
# a function of the same name from the checkmate package.
# The argument choices must be logical, character or numeric.
assert_choice = function(scalar, choices, prefix=""){
  
  # Name of scalar variable.
  scalar_name = deparse(substitute(scalar))
  
  # Check if scalar has the correct class.
  if( is.logical(choices) & !is.logical(scalar) ){
    stop(paste0(prefix, scalar_name, " must be logical."))
  } else if( is.character(choices) & !is.character(scalar) ){
    stop(paste0(prefix, scalar_name, " must be character."))
  } else if( is.numeric(choices) & !is.numeric(scalar) ){
    stop(paste0(prefix, scalar_name, " must be numeric."))
  }
  
  # Check if scalar has length = 1.
  if( length(scalar) != 1 )
    stop(paste0(prefix, scalar_name, " must have length = 1."))
  
  # Check if scalar is from one of the allowed choices.
  if( !(scalar %in% choices) )
    if( is.character(choices) ){
      stop(paste0(prefix, scalar_name, ' can only take the following values: "',
                  paste0(choices, collapse='", "'), '".'))
    } else {
      stop(paste0(prefix, scalar_name, ' can only take the following values: ',
                  paste0(choices, collapse=', '), '.'))
    }
}





# Function to check y_0
assert_y_0 = function(y_0, prefix=""){
  
  if( !is.numeric(y_0) )
    stop(paste0(prefix, "y_0 must be numeric."))
  if( length(y_0) < 1 )
    stop(paste0(prefix, "y_0 must have length >= 1."))
  if( any(is.na(y_0)) )
    stop(paste0(prefix, "y_0 cannot contain NA values."))
}





# Function to check times.
# Only run this after model_type has been checked.
assert_times = function(times, model_type, prefix=""){
  
  if( !is.numeric(times) )
    stop(paste0(prefix, "times must be numeric."))
  if( length(times) < 2 )
    stop(paste0(prefix, "times must have length >= 2."))
  if( any(is.na(times)) )
    stop(paste0(prefix, "times cannot contain NA values."))
  if( any(duplicated(times)) )
    stop(paste0(prefix, "times cannot have duplicate elements."))
  if( is.unsorted(times) )
    stop(paste0(prefix, "times must be sorted in ascending order."))
  
  # If model_type is "discrete", perform further checks.
  if( model_type == "discrete" ){
    if( any(round(times) != times) | any(diff(times) != 1) )
      stop(paste0(prefix, "times can only contain consecutive integer values."))
  }
}





# Function to check optional argument lists.
assert_arglist = function(arglist, prefix=""){
  
  # Name of argument list.
  arglist_name = deparse(substitute(arglist))
  
  if( !is.list(arglist) )
    stop(paste0(prefix, arglist_name, " must be a list."))
  # The next line works even if names(arglist) returns NULL, because
  # any(logical(0)) returns FALSE so the final condition is still a length-1
  # logical.
  if( (length(arglist) >= 1) & ( is.null(names(arglist)) |
                                 any(names(arglist) == "") |
                                 any(duplicated(names(arglist))) ) )
    stop(paste0(prefix, "All elements in ", arglist_name,
                " must be named, and the names cannot contain duplicates."))
}





# Function to check parms.
# Only run this after times has been checked.
assert_parms = function(parms, times, prefix=""){
  
  if( is.numeric(parms) ){
    # If parms is numeric, check whether it contains NA values.
    if( any(is.na(parms)) )
      stop(paste0(prefix, "parms cannot contain NA values."))
    
  } else if( is.function(parms) ){
    # If parms is a function, make further checks.
    
    # Check whether the function only accepts one input argument.
    if( length(formals(parms)) != 1 )  
      stop(paste0(prefix, "If parms is a function, it must only accept one input argument t."))
    # Check the output using the first time step.
    if( !is.numeric(parms(times[1])) )
      stop(paste0(prefix, "If parms is a function, it must return a numeric output."))
    if( any(is.na(parms(times[1]))) )
      stop(paste0(prefix, "If parms is a function, the output cannot contain NA values."))
    
  } else if( is.list(parms) ){
    # If parms is a list, make further checks.
    
    # Check whether every element in the list is numeric or a function.
    # We use vapply because sapply doesn't simplify if parms is an empty list.
    num_elements = which( vapply(X=parms, FUN=is.numeric, FUN.VALUE=T) )  # Numeric elements.
    fun_elements = which( vapply(X=parms, FUN=is.function, FUN.VALUE=T) )  # Function elements.
    if( length(parms) != (length(num_elements) + length(fun_elements)) )
      stop(paste0(prefix, "If parms is a list, every element must either be numeric or a function."))
    
    # For each numeric element, check whether it contains NA values.
    for(i in num_elements){
      if( any(is.na(parms[[i]])) )
        stop(paste0(prefix, paste0("parms[[",i,"]] cannot contain NA values.")))
    }
    
    # For each function element, make further checks.
    for(i in fun_elements){
      # Check whether the function only accepts one input argument.
      if( length(formals(parms[[i]])) != 1 )
        stop(paste0(prefix, paste0("If parms[[",i,"]] is a function, it must only accept one input argument t.")))
      # Check the output using the first time step.
      if( !is.numeric(parms[[i]](times[1])) )
        stop(paste0(prefix, paste0("If parms[[",i,"]] is a function, it must return a numeric output.")))
      # Does its output contain NA values? Check using the first time step.
      if( any(is.na(parms[[i]](times[1]))) )
        stop(paste0(prefix, paste0("If parms[[",i,"]] is a function, the output cannot contain NA values.")))
    }
    
  } else {
    # parms does not have an allowed structure.
    stop(paste0(prefix, "Structure of parms not allowed. See help(state_sens) for allowed structures."))
    
  }
}





# Functions to check the user-specified functions.
# Only run these after parms, y_0, times and optional argument lists have been
# checked.

# Function to check dynamic_fn.
# The argument length_label is the label used in the error message for the
# number of state variables.
assert_dynamic_fn = function(dynamic_fn, parms, y, times, dynamic_fn_arglist,
                             prefix="", length_label){
  
  if( !is.function(dynamic_fn) )
    stop(paste0(prefix, "dynamic_fn must be a function."))
  
  # Extract the argument names of dynamic_fn.
  dynamic_fn_argnames = names(formals(dynamic_fn))
  # Check that dynamic_fn has at least 3 arguments.
  if(length(dynamic_fn_argnames) < 3)
    stop(paste0(prefix, "dynamic_fn must accept at least three input arguments t, y and parms."))
  
  # Obtain sample output from dynamic_fn, by evaluating at times[1] and y.
  dynamic_fn_arglist[[dynamic_fn_argnames[1]]] = times[1]
  dynamic_fn_arglist[[dynamic_fn_argnames[2]]] = y
  dynamic_fn_arglist[[dynamic_fn_argnames[3]]] = parms
  dynamic_fn_output = do.call(dynamic_fn, dynamic_fn_arglist)
  
  # Check the output from dynamic_fn.
  if( !is.list(dynamic_fn_output) )
    stop(paste0(prefix, "dynamic_fn must return a list."))
  if( length(dynamic_fn_output) < 1 )
    stop(paste0(prefix, "dynamic_fn must return a list of length >= 1."))
  if( !is.numeric(dynamic_fn_output[[1]]) )
    stop(paste0(prefix, "The first element of the list returned by dynamic_fn must be numeric."))
  if( length(dynamic_fn_output[[1]]) != length(y) )
    stop(paste0(prefix, "The first element of the list returned by dynamic_fn must have length = ", length_label, "."))
  if( any(is.na(dynamic_fn_output[[1]])) )
    stop(paste0(prefix, "The first element of the list returned by dynamic_fn cannot contain NA values."))
  if( length(dynamic_fn_output) > 1 )
    if( !is.numeric(unlist(dynamic_fn_output[-1])) )
      stop(paste0(prefix, "Each optional element of the list returned by dynamic_fn must be numeric."))
}


# Function to check reward_fn.
assert_reward_fn = function(reward_fn, y, times, reward_fn_arglist, prefix=""){
  
  if( !is.function(reward_fn) )
    stop(paste0(prefix, "reward_fn must be a function."))
  
  # Extract the argument names of reward_fn.
  reward_fn_argnames = names(formals(reward_fn))
  # Check that reward_fn has at least 2 arguments.
  if(length(reward_fn_argnames) < 2)
    stop(paste0(prefix, "reward_fn must accept at least two input arguments t and y."))
  
  # Obtain sample output from reward_fn, by evaluating at times[1] and y.
  reward_fn_arglist[[reward_fn_argnames[1]]] = times[1]
  reward_fn_arglist[[reward_fn_argnames[2]]] = y
  reward_fn_output = do.call(reward_fn, reward_fn_arglist)
  
  # Check the output from reward_fn.
  if( !is.numeric(reward_fn_output) )
    stop(paste0(prefix, "reward_fn must return a numeric output."))
  if( length(reward_fn_output) != 1 )
    stop(paste0(prefix, "reward_fn must return a output of length = 1."))
  if( any(is.na(reward_fn_output)) )
    stop(paste0(prefix, "The output returned by reward_fn cannot contain NA values."))
}


# Function to check terminal_fn.
assert_terminal_fn = function(terminal_fn, y, terminal_fn_arglist, prefix=""){
  
  if( !is.function(terminal_fn) )
    stop(paste0(prefix, "terminal_fn must be a function."))
  
  # Extract the argument names of terminal_fn.
  terminal_fn_argnames = names(formals(terminal_fn))
  # Check that terminal_fn has at least 1 argument.
  if(length(terminal_fn_argnames) < 1)
    stop(paste0(prefix, "terminal_fn must accept at least one input argument t."))
  
  # Obtain sample output from terminal_fn, by evaluating at y.
  terminal_fn_arglist[[terminal_fn_argnames[1]]] = y
  terminal_fn_output = do.call(terminal_fn, terminal_fn_arglist)
  
  # Check the output from terminal_fn.
  if( !is.numeric(terminal_fn_output) )
    stop(paste0(prefix, "terminal_fn must return a numeric output."))
  if( length(terminal_fn_output) != 1 )
    stop(paste0(prefix, "terminal_fn must return a output of length = 1."))
  if( any(is.na(terminal_fn_output)) )
    stop(paste0(prefix, "The output returned by terminal_fn cannot contain NA values."))
}





# Function to check the state and tdss matrices.
assert_mat = function(mat, times, prefix=""){
  
  # Name of scalar variable.
  mat_name = deparse(substitute(mat))

  if( !is.numeric(mat) | !is.matrix(mat) )
    stop(paste0(prefix, mat_name, " must be a numeric matrix."))  
  if( nrow(mat) != length(times) )
    stop(paste0(prefix, "nrow(", mat_name, ") must be equal to length(times)."))
  if( ncol(mat) < 1 )
    stop(paste0(prefix, "ncol(", mat_name, ") cannot be less than 1."))
  if( any(is.na(mat)) )
    stop(paste0(prefix, mat_name, " cannot contain NA values."))  
}


