# Function to calculate time-dependent parameter sensitivities.
#
# Since the input arguments are rather complicated, it is easier to "handcraft"
# the .Rd file than to use roxygen2 to create a documentation with the desired
# look.
# Therefore, to avoid duplicate and possibly conflicting information, the
# function description should be placed directly in the .Rd file and not here.

parm_sens = function(
  state_sens_out,
  numDeriv_arglist = list(),
  verbose = TRUE
  ){
  
  
  
  # --------------------------
  # Check the input arguments.
  # --------------------------
  
  # Check verbose first so we know whether to display the progress indicators
  # for the subsequent checks.
  assert_choice(scalar=verbose, choices=c(TRUE, FALSE))
  
  if(verbose)cat("Checking input arguments... ")
  
  
  
  # Check input arguments that are unrelated to state_sens_out.
  assert_arglist(arglist=numDeriv_arglist)

  
  
  # Long series of checks involving state_sens_out.
  # Common error prefix to display.
  prefix = "Error with state_sens_out; please use the output of state_sens instead of trying to create it by hand.\nDetails: "
  
  # Check that state_sens_out is a list.
  if( !is.list(state_sens_out) )
    stop(paste0(prefix, "state_sens_out must be a list."))
  
  # Check that state_sens_out contains all required elements.
  element_names = c("model_type", "dynamic_fn", "parms", "dynamic_fn_arglist",
                    "times", "state", "tdss")
  if( !all( element_names %in% names(state_sens_out) ) )
    stop(paste0(prefix, "state_sens_out is missing required elements."))
  
  
  
  # Check individual elements of state_sens_out.
  # First, extract the elements.
  model_type = state_sens_out$model_type
  dynamic_fn = state_sens_out$dynamic_fn
  parms = state_sens_out$parms
  dynamic_fn_arglist = state_sens_out$dynamic_fn_arglist
  times = state_sens_out$times
  state = state_sens_out$state
  tdss = state_sens_out$tdss
  
  assert_choice(scalar=model_type, choices=c("continuous", "discrete"), prefix=prefix)
  assert_arglist(arglist=dynamic_fn_arglist, prefix=prefix)
  
  # This check should only be done after model_type has been checked.
  assert_times(times=times, model_type=model_type, prefix=prefix)
  
  
  
  # Check parms. This should only be done after times has been checked.
  if( is.null(parms) ){
    # If parms is NULL, there are no parameters to analyse.
    stop(paste0(prefix, "parms is NULL, so there are no parameters to analyse."))
    
  } else {
    # Run the other checks for parms.
    assert_parms(parms=parms, times=times, prefix=prefix)
  }
  
  
  
  # Check state and tdss. This should only be done after times has been checked.
  assert_mat(mat=state, times=times, prefix=prefix)
  assert_mat(mat=tdss, times=times, prefix=prefix)
  # Note that state can have more columns than tdss if there are additional
  # elements in the list returned by dynamic_fn(); these are called "global 
  # values" in deSolve.
  if( ncol(tdss) < ncol(state) )
    stop(paste0(prefix, "ncol(state) cannot be less than ncol(tdss)."))

  
  
  # Check dynamic_fn. This should only be done after parms, tdss, times and
  # dynamic_fn_arglist have been checked.
  # Note that for y, we use the state vector state[1,1:ncol(tdss)], because
  # ncol(tdss) should give the number of state vectors, whereas state might
  # contain additional columns corresponding to "global values".
  assert_dynamic_fn(dynamic_fn=dynamic_fn, parms=parms, y=state[1,1:ncol(tdss)],
                    times=times, dynamic_fn_arglist=dynamic_fn_arglist,
                    length_label="ncol(tdss)", prefix=prefix)
  
  
  
  if(verbose)cat("Done!\n\n")
  
  
  
  
  # ------------------------------------------------------------------------
  # Clone dynamic_fn in such a way that it becomes compatible with numDeriv.
  # ------------------------------------------------------------------------
  
  if(verbose)cat("Re-defining dynamic_fn to allow numerical derivatives with respect to parms... ")

  # Elements of state_sens_out. These assignments were already made during the
  # input checks, but they are repeated here just in case they get accidentally
  # removed in the future when editing the checks.
  model_type = state_sens_out$model_type
  dynamic_fn = state_sens_out$dynamic_fn
  parms = state_sens_out$parms
  dynamic_fn_arglist = state_sens_out$dynamic_fn_arglist
  times = state_sens_out$times
  state = state_sens_out$state
  tdss = state_sens_out$tdss

  # Use ncol(tdss) for n_y, since state might contain additional columns
  # corresponding to "global values".
  n_y = ncol(tdss)
  n_t = length(times)

  # Extract the argument names of dynamic_fn.
  dynamic_fn_argnames = names(formals(dynamic_fn))

  
  
  # First, clone parms but make the clone parms2 a list if not already one.
  # This is so that we can use the same procedure later on, regardless of the
  # structure of parms. We also use a logical variable to keep track of whether
  # parms was originally a list.
  if( is.list(parms) ){
    parms_not_list = FALSE
    parms2 = parms
  } else {
    parms_not_list = TRUE
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
  skel_array_elements = which( sapply(skel_dim, length) >= 3 )
  
  
  
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
      body(parms_relist2[[i]]) = parse(text=paste0("parms_relist[[",i,"]]"))
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
    tdss = rbind(tdss[-1, , drop=FALSE], rep(0, n_y))
  }
  
  # Calculate the parameter sensitivities.
  for(i in 1:n_t){
    parms_now[fun_elements] = lapply(parms2_fun, do.call, list(times[i]))
    
    numDeriv_arglist[["func"]] = dynamic_fn2
    numDeriv_arglist[["x"]] = unlist(parms_now)
    numDeriv_arglist[["t"]] = times[i]
    numDeriv_arglist[["y"]] = state[i, 1:n_y]
    tdps_mat[i,] = tdss[i, 1:n_y, drop=FALSE] %*%
      do.call(what=numDeriv::jacobian, args=numDeriv_arglist)
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
      tdps[[i]] = tdps_mat[, column_counter + 1:skel_length[[i]], drop=FALSE]
      colnames(tdps[[i]]) = names(skel[[i]])

    } else {
      
      # If the element of skel is an array, then convert the associated columns
      # into a higher-dimensional array.
      # This should work even if the user had artificially added attributes to
      # convert a numeric vector into a one-dimensional array.
      tdps[[i]] = 
        array( data = tdps_mat[, column_counter + 1:skel_length[[i]],
                               drop=FALSE],
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