# Function to calculate time-dependent parameter sensitivities.
# See the .Rd file for a more detailed description of the input arguments.
# The argument "verbose" controls whether to display progress messages in the
# console.

parm_sens = function(
  state_sens_out,
  numDeriv_arglist = list(),
  verbose = TRUE
  ){
  
  
  
  # --------------------------
  # Check the input arguments.
  # --------------------------
  
  if(verbose)cat("Checking input arguments... ")
  
  # Check numDeriv_arglist.
  if( !is.list(numDeriv_arglist) )
    stop("numDeriv_arglist must be a list.")
  if( any(names(numDeriv_arglist) == "") | any(duplicated(names(numDeriv_arglist))) )
    stop("All elements in numDeriv_arglist must be named, and the names cannot contain duplicates.")
  
  
  
  # Check state_sens_out. This is a long series of checks.
  # Common error message to display.
  err = "Error with state_sens_out; please use the output of state_sens instead of trying to create it by hand.\nDetails:"
  
  # Check that state_sens_out is a list.
  if( !is.list(state_sens_out) )
    stop(paste(err, "state_sens_out must be a list."))
  
  # Check that state_sens_out contains all required elements.
  element_names = c("model_type", "dynamic_fn", "parms", "dynamic_fn_arglist",
                    "times", "state", "tdss")
  if( !all( element_names %in% names(state_sens_out) ) )
    stop(paste(err, "state_sens_out is missing required elements."))
  
  
  
  # Check elements of state_sens_out.
  
  # Elements of state_sens_out.
  model_type = state_sens_out$model_type
  dynamic_fn = state_sens_out$dynamic_fn
  parms = state_sens_out$parms
  dynamic_fn_arglist = state_sens_out$dynamic_fn_arglist
  times = state_sens_out$times
  state = state_sens_out$state
  tdss = state_sens_out$tdss
  
  
  
  # Check model_type first because it will influence the subsequent checks.
  if( !is.character(model_type) )
    stop(paste(err, "model_type must be character."))
  if( length(model_type) != 1 )
    stop(paste(err, "model_type must have length = 1."))
  if( !(model_type %in% c("continuous", "discrete")) )
    stop(paste(err, 'model_type can only take values "continuous" or "discrete".'))
  
  
  
  # Check that the input arguments (except parms) are of the correct classes.
  if( !is.function(dynamic_fn) )
    stop(paste(err, "dynamic_fn must be a function."))
  if( !is.numeric(times) )
    stop(paste(err, "times must be numeric."))
  if( !is.list(dynamic_fn_arglist) )
    stop(paste(err, "dynamic_fn_arglist must be a list."))
  if( !is.numeric(state) | !is.matrix(state) )
    stop(paste(err, "state must be a numeric matrix."))
  if( !is.numeric(tdss) | !is.matrix(tdss) )
    stop(paste(err, "tdss must be a numeric matrix."))
  
  
  
  # Check times.
  # Since we will use times[1] when checking parms, this check must be performed
  # before we check parms.
  n_t = length(times)
  if( n_t < 2 )
    stop(paste(err, "times must have length >= 2."))
  if( any(is.na(times)) )
    stop(paste(err, "times cannot contain NA values."))
  if( any(duplicated(times)) )
    stop(paste(err, "times cannot have duplicate elements."))
  if( is.unsorted(times) )
    stop(paste(err, "times must be sorted in ascending order."))
  
  if( model_type == "discrete" ){
    if( any(round(times) != times) | any(diff(times) != 1) )
      stop(paste(err, "times can only contain consecutive integer values."))
  }
  
  
  
  # Check parms. This is probably the most complicated check.
  if( is.null(parms) ){
    # If parms is NULL, there are no parameters to analyse.
    stop(paste(err, "parms is NULL, so there are no parameters to analyse."))
    
  } else if( is.numeric(parms) ){
    # If parms is numeric, check whether it contains NA values.
    if( any(is.na(parms)) )
      stop(paste(err, "parms cannot contain NA values."))
    
  } else if( is.function(parms) ){
    # If parms is a function, make further checks.
    
    # Check whether the function only accepts one input argument.
    if( length(formals(parms)) != 1 )  
      stop(paste(err, "If parms is a function, it must only accept one input argument t."))
    # Check the output using the first time step.
    if( !is.numeric(parms(times[1])) )
      stop(paste(err, "If parms is a function, it must return a numeric output."))
    if( any(is.na(parms(times[1]))) )
      stop(paste(err, "If parms is a function, the output cannot contain NA values."))
    
  } else if( is.list(parms) ){
    # If parms is a list, make further checks.
    
    # Check whether every element in the list numeric or a function.
    # We use vapply because sapply doesn't simplify if parms is an empty list.
    num_elements = which( vapply(X=parms, FUN=is.numeric, FUN.VALUE=T) )  # Numeric elements.
    fun_elements = which( vapply(X=parms, FUN=is.function, FUN.VALUE=T) )  # Function elements.
    if( length(parms) != (length(num_elements) + length(fun_elements)) )
      stop(paste(err, "If parms is a list, every element must either be numeric or a function."))
    
    # For each numeric element, check whether it contains NA values.
    for(i in num_elements){
      if( any(is.na(parms[[i]])) )
        stop(paste(err, paste("parms[[",i,"]] cannot contain NA values.", sep="")))
    }
    
    # For each function element, make further checks.
    for(i in fun_elements){
      # Check whether the function only accepts one input argument.
      if( length(formals(parms[[i]])) != 1 )
        stop(paste(err, paste("If parms[[",i,"]] is a function, it must only accept one input argument t.", sep="")))
      # Check the output using the first time step.
      if( !is.numeric(parms[[i]](times[1])) )
        stop(paste(err, paste("If parms[[",i,"]] is a function, it must return a numeric output.", sep="")))
      # Does its output contain NA values? Check using the first time step.
      if( any(is.na(parms[[i]](times[1]))) )
        stop(paste(err, paste("If parms[[",i,"]] is a function, the output cannot contain NA values.", sep="")))
    }
    
  } else {
    # parms does not have an allowed structure.
    stop(paste(err, "Structure of parms not allowed. See help(state_sens) for allowed structures."))
    
  }
  
  
  
  # Check state and tdss.
  
  # Use ncol(tdss) for n_y, since state might contain additional columns
  # corresponding to "global values".
  n_y = ncol(tdss)
  n_t = length(times)
  
  # Check the dimensions of state and tdss.
  if( nrow(state) != n_t )
    stop(paste(err, "nrow(state) must be equal to length(times)."))
  if( nrow(tdss) != n_t )
    stop(paste(err, "nrow(tdss) must be equal to length(times)."))
  if( n_y < 1 )
    stop(paste(err, "ncol(tdss) cannot be less than 1"))
  if( ncol(tdss) < n_y )
    stop(paste(err, "ncol(state) cannot be less than ncol(tdss)."))
  
  # Check whether state and tdss contain NA values.
  if( any(is.na(state)) )
    stop(paste(err, "state cannot contain NA values."))
  if( any(is.na(tdss)) )
    stop(paste(err, "tdss cannot contain NA values."))
  
  # Initial conditions.
  y_0 = state[1, 1:n_y]
  
  
  
  # Check dynamic_fn_arglist.
  if( any(names(dynamic_fn_arglist) == "") | any(duplicated(names(dynamic_fn_arglist))) )
    stop(paste(err, "All elements in dynamic_fn_arglist must be named, and the names cannot contain duplicates."))
  
  
  
  # Check dynamic_fn.
  
  # Extract the argument names of dynamic_fn.
  dynamic_fn_argnames = names(formals(dynamic_fn))
  
  # Check that dynamic_fn has the required number of arguments.
  if(length(dynamic_fn_argnames) < 3)
    stop(paste(err, "dynamic_fn must accept at least three input arguments t, y and parms."))
  
  # Obtain sample output from dynamic_fn.
  # Evaluate at times[1] and y_0.
  # Note that assigning an element to NULL automatically creates a list.
  dynamic_fn_arglist[[dynamic_fn_argnames[1]]] = times[1]
  dynamic_fn_arglist[[dynamic_fn_argnames[2]]] = y_0
  dynamic_fn_arglist[[dynamic_fn_argnames[3]]] = parms
  dynamic_fn_output = do.call(dynamic_fn, dynamic_fn_arglist)

  # Check the output from dynamic_fn.
  if( !is.list(dynamic_fn_output) )
    stop(paste(err, "dynamic_fn must return a list."))
  if( !is.numeric(dynamic_fn_output[[1]]) | (length(dynamic_fn_output[[1]]) != n_y) )
    stop(paste(err, "The first element of the list returned by dynamic_fn must be numeric, and have length = ncol(tdss)."))
  if( any(is.na(dynamic_fn_output[[1]])) )
    stop(paste(err, "The first element of the list returned by dynamic_fn cannot contain NA values."))
  
  if(verbose)cat("Done!\n\n")
  
  
  
  
  # ------------------------------------------------------------------------
  # Clone dynamic_fn in such a way that it becomes compatible with numDeriv.
  # ------------------------------------------------------------------------
  
  if(verbose)cat("Re-defining dynamic_fn to allow numerical derivatives with respect to parms... ")
  
  # The following assignments have been commented out because they were already
  # made during the input checks.
  # 
  # # Elements of state_sens_out.
  # model_type = state_sens_out$model_type
  # dynamic_fn = state_sens_out$dynamic_fn
  # parms = state_sens_out$parms
  # dynamic_fn_arglist = state_sens_out$dynamic_fn_arglist
  # times = state_sens_out$times
  # state = state_sens_out$state
  # tdss = state_sens_out$tdss
  # 
  # # Use ncol(tdss) for n_y, since state might contain additional columns
  # # corresponding to "global values".
  # n_y = ncol(tdss)
  # n_t = length(times)
  # 
  # # Extract the argument names of dynamic_fn.
  # dynamic_fn_argnames = names(formals(dynamic_fn))

  
  
  # First, clone parms but make the clone parms2 a list if not already one.
  # This is so that we can use the same procedure later on, regardless of the
  # structure of parms. We also use a logical variable to keep track of whether
  # parms was originally a list.
  if( is.list(parms) ){
    parms_not_list = F
    parms2 = parms
  } else {
    parms_not_list = T
    parms2 = list(parms)
  }

  # Which elements of parms2 are functions?
  fun_elements = which( sapply(parms2, is.function) )
  
  # Create a sublist of parms2 containing only the functions.
  parms2_fun = parms2[fun_elements]
  
  # Clone parms2, but with the function elements replaced by their evaluations
  # at times[1]. This will be used as the "skeleton" when using relist later on.
  skel = parms2
  skel[fun_elements] = lapply(parms2_fun, do.call, list(times[1]))
  
  # Note that relist doesn't restore arrays with 3 or more indices, so we have
  # to do it by hand.
  # We need to identify which elements of skel are arrays with 3 or more
  # indices, and also "save" the dimensions and dimension names.
  skel_dim = lapply(skel, dim)
  skel_dimnames = lapply(skel, dimnames)
  skel_array_elements = which(sapply(skel_dim, length) >= 3)
  
  
  
  # Clone dynamic_fn, but with argument parms replaced by a numeric vector
  # parms_unlist, so numDeriv can take derivatives of the clone with respect to
  # parms_unlist.
  # Also, pre-assign any additional arguments.
  dynamic_fn2 = function(t, y, parms_unlist){
    
    # Relist parms_unlist using the skeleton skel.
    parms_relist = relist(parms_unlist, skel)
    
    # If any element of skel is an array with 3 or more indices, restore the
    # corresponding element of parms_relist back into an array.
    for(i in skel_array_elements){
      parms_relist[[i]] = array(parms_relist[[i]],
                                dim=skel_dim[[i]],
                                dimnames=skel_dimnames[[i]])
    }
    
    # parms_relist is now a list of numeric objects with the same structure as
    # skel.
    
    # Clone parms_relist, except that if any element of parms2 is a function, we
    # will also convert the corresponding element in the clone parms_relist2
    # into a "trivial" function.
    # Note that the naive method of defining the a "trivial" function using
    # parms_relist2[[i]] = function(t){parms_relist[[i]]}
    # doesn't work because the "i" used in the function will always be its
    # current global value.
    # Hence, we need the method based on body and parse as shown below.
    parms_relist2 = parms_relist
    for(i in fun_elements){
      parms_relist2[[i]] = function(t){}
      body(parms_relist2[[i]]) = parse(text=paste("parms_relist[[",i,"]]",sep=""))
    }

    # Evaluate dynamic_fn.
    dynamic_fn_arglist[[dynamic_fn_argnames[1]]] = t
    dynamic_fn_arglist[[dynamic_fn_argnames[2]]] = y
    # Now, if parms wasn't originally a list, we need to extract the first
    # element from parms_relist2, because dynamic_fn doesn't expect parms to be
    # a list.
    if(parms_not_list){
      dynamic_fn_arglist[[dynamic_fn_argnames[3]]] = parms_relist2[[1]]
    } else {
      dynamic_fn_arglist[[dynamic_fn_argnames[3]]] = parms_relist2
    }
    return( do.call(dynamic_fn, dynamic_fn_arglist)[[1]] )
  }
  
  if(verbose)cat("Done!\n\n")
  
  
  
  
  # --------------------------------------
  # Calculate the parameter sensitivities.
  # --------------------------------------
  
  if(verbose)cat("Calculating parameter sensitivities... ")
  
  # Create a matrix to store the parameter sensitivities.
  tdps_mat = matrix(NA, nrow=length(times), ncol=length(unlist(skel)))
  
  # List to store the "current" values of the parameters. This is like skel,
  # except that during each time step, the function elements are evaluated at 
  # the current time step instead of times[1].
  parms_now = parms2
  
  # For a discrete-time model, we need to offset the time step in tdss by
  # one, because of the slight difference in the formula.
  if( model_type == "discrete" ){
    tdss = rbind(tdss[-1, , drop=F], rep(0, n_y))
  }
  
  # Calculate the parameter sensitivities.
  for(i in 1:n_t){
    parms_now[fun_elements] = lapply(parms2_fun, do.call, list(times[i]))
    
    numDeriv_arglist[["func"]] = dynamic_fn2
    numDeriv_arglist[["x"]] = unlist(parms_now)
    numDeriv_arglist[["t"]] = times[i]
    numDeriv_arglist[["y"]] = state[i, 1:n_y]
    tdps_mat[i,] = tdss[i, 1:n_y, drop=F] %*% do.call(jacobian, numDeriv_arglist)
  }
  
  
  
  # --------------------------------------------------------------------
  # Re-arranging the parameter sensitivities into to the desired format.
  # --------------------------------------------------------------------
  
  # Since skel is a list of numeric objects, we can use length to find out which
  # columns of tdps_mat to associate with each element of parms2.
  skel_length = lapply(skel, length)
  
  tdps = skel
  column_counter = 0
  for(i in 1:length(skel)){
    
    if( is.null(skel_dim[[i]]) ){
      
      # If the element of skel is a numeric vector, its dimension will be NULL.
      # In that case, leaving the associated columns as a matrix is fine.
      tdps[[i]] = tdps_mat[, column_counter + 1:skel_length[[i]], drop=F]
      colnames(tdps[[i]]) = names(skel[[i]])

    } else {
      
      # If the element of skel is an array, then convert the associated columns
      # into a higher-dimensional array.
      # This should work even if the user had artificially added attributes to
      # convert a numeric vector into a one-dimensional array.
      tdps[[i]] = 
        array( data = tdps_mat[, column_counter + 1:skel_length[[i]], drop=F],
               dim = c(n_t, skel_dim[[i]]),
               dimnames = c(list(NULL), skel_dimnames[[i]]) )
    
    }
    
    column_counter = column_counter + skel_length[[i]]

  }
  
  if(verbose)cat("Done!\n\n")
  
  
  
  if(verbose)cat("Time-dependent parameter sensitivity calculations complete.\n\n")
  
  # Return times and tdps.
  if(parms_not_list){
    # If parms wasn't originally not a list, return the first element of tdps.
    return(list(times=times, tdps=tdps[[1]]))
  } else {
    # If parms was originally a list, return tdps as it is.
    return(list(times=times, tdps=tdps))
  }
  
}