# Unit tests.

##################################
# Baseline continuous-time model #
##################################

# We use the translocation model from Ng et al. (2023).

# Parameter values for the dynamic equations.
parms = list(
  b = 1,                                          # Per-capita birth rate.
  a = 0.1,                                        # Competition coefficient.
  mu = function(t){0.5 + 1/(1 + exp((t-10)/2))},  # Per-capita loss rate.
  sigma = 0.2                                     # Immigration rate.
)

# Function that returns the dynamic equations.
dynamic_fn = function(t, y, parms){
  b = parms[["b"]]
  a = parms[["a"]]
  sigma = parms[["sigma"]]
  mu = parms[["mu"]](t)

  dy = b*y*(1- a*y) - mu*y + sigma
  return( list(dy) )
}

# Initial conditions.
y_0 = 0.37  # Approximate steady-state population before restoration efforts.

# Function that returns the reward integrand.
reward_fn = function(t, y){
  w = 1  # Per-capita rate at which the ecosystem service is provided.
  return( w * y )
}

# Function that returns the terminal payoff.
terminal_fn = function(y){
  v = 1.74  # Ascribed value per individual at the end of the period.
  return( v * y )
}

# Time steps over management period. We discretise it into 1001 time steps
# (so the step size is 0.03).
times = seq(0, 30, length.out=1001)





#############################################################################
# Check that the numerical output are correct for the continuous-time model #
#############################################################################

# Load the target values.
state_sens_out1 = readRDS(file=system.file("testdata",
                                           "test1_state_sens_out.rds",
                                           package="tdsa"))
parm_sens_out1 = readRDS(file=system.file("testdata",
                                          "test1_parm_sens_out.rds",
                                          package="tdsa"))

# Calculate the current values.
state_sens_out2 = state_sens(model_type = "continuous",
                             dynamic_fn = dynamic_fn,
                             parms = parms,
                             reward_fn = reward_fn,
                             terminal_fn = terminal_fn,
                             y_0 = y_0,
                             times = times)
parm_sens_out2 = parm_sens(state_sens_out2)

# Test for equality. All the values being tested are relatively large, so it is
# fine to test the equality to a tolerance of 1e-2.
expect_equal(current = state_sens_out2[c("state", "tdss")],
             target = state_sens_out1[c("state", "tdss")],
             tolerance = 1e-2)
expect_equal(current = parm_sens_out2[["tdps"]],
             target = parm_sens_out1[["tdps"]],
             tolerance = 1e-2)






#############################################
# Catch errors in the continuous-time model #
#############################################

# Error in model_type.
# List of unacceptable inputs.
wrong_list = list(
  NULL,
  1,
  "unacceptable",
  c("continuous", "continuous")
)

message_list = list(
  "character",
  "character",
  "value",
  "length"
)

for(i in 1:length(wrong_list)){
  expect_error(
    state_sens(model_type = wrong_list[[i]],
               dynamic_fn = dynamic_fn,
               parms = parms,
               reward_fn = reward_fn,
               terminal_fn = terminal_fn,
               y_0 = y_0,
               times = times),
    pattern = paste0(".*", message_list[[i]], ".*")
  )
}





# Error in dynamic_fn.
# List of unacceptable inputs.
wrong_list = list(
  NULL,
  function(t, y, parms){ return( y ) },
  function(t, y, parms){ return( list("unacceptable") ) },
  function(t, y, parms){ return( list(c(y, y)) ) },
  function(t, y){ return( list( y ) )
  }
)

message_list = list(
  "function",
  "list",
  "numeric",
  "length",
  "argument"
)

for(i in 1:length(wrong_list)){
  expect_error(
    state_sens(model_type = "continuous",
               dynamic_fn = wrong_list[[i]],
               parms = parms,
               reward_fn = reward_fn,
               terminal_fn = terminal_fn,
               y_0 = y_0,
               times = times),
    pattern = paste0(".*", message_list[[i]], ".*")
  )
}

for(i in 1:length(wrong_list)){
  state_sens_out3 = state_sens_out2
  state_sens_out3["dynamic_fn"] = wrong_list[i]
  expect_error(
    parm_sens(state_sens_out=state_sens_out3),
    pattern = paste0(".*", message_list[[i]], ".*")
  )
}





# Error in parms.
# List of unacceptable inputs. Note that NULL is acceptable for state_sens(),
# but not parm_sens().
wrong_list = list(
  "unacceptable",
  list("unacceptable"),
  NA_real_,
  list(NA_real_),
  function(t){return("unacceptible")},
  list(function(t){return("unacceptible")}),
  function(t){return(NA_real_)},
  list(function(t){return(NA_real_)}),
  function(t,x){return(t*x)},
  list(function(t,x){return(t*x)})
)

message_list = list(
  "structure",
  "numeric",
  "NA",
  "NA",
  "numeric",
  "numeric",
  "NA",
  "NA",
  "argument",
  "argument"
)

for(i in 1:length(wrong_list)){
  expect_error(
    state_sens(model_type = "continuous",
               dynamic_fn = dynamic_fn,
               parms = wrong_list[[i]],
               reward_fn = reward_fn,
               terminal_fn = terminal_fn,
               y_0 = y_0,
               times = times),
    pattern = paste0(".*", message_list[[i]], ".*")
  )
}

for(i in 1:length(wrong_list)){
  state_sens_out3 = state_sens_out2
  state_sens_out3["parms"] = wrong_list[i]
  expect_error(
    parm_sens(state_sens_out = state_sens_out3),
    pattern = paste0(".*", message_list[[i]], ".*")
  )
}

state_sens_out3["parms"] = list(NULL)
expect_error(
  parm_sens(state_sens_out = state_sens_out3),
  pattern = ".*NULL.*"
)





# Error in reward_fn.
# List of unacceptable inputs.
wrong_list = list(
  NULL,
  function(t, y){ return( "unacceptable" ) },
  function(t, y){ return( c(y, y) ) },
  function(y){ return( y ) }
)

message_list = list(
  "function",
  "numeric",
  "length",
  "argument"
)

for(i in 1:length(wrong_list)){
  expect_error(
    state_sens(model_type = "continuous",
               dynamic_fn = dynamic_fn,
               parms = parms,
               reward_fn = wrong_list[[i]],
               terminal_fn = terminal_fn,
               y_0 = y_0,
               times = times),
    pattern = paste0(".*", message_list[[i]], ".*")
  )
}




# Error in terminal_fn.
# List of unacceptable inputs.
wrong_list = list(
  NULL,
  function(y){ return( "unacceptable" ) },
  function(y){ return( c(y, y) ) },
  function(){ return( 1 ) }
)

message_list = list(
  "function",
  "numeric",
  "length",
  "argument"
)

for(i in 1:length(wrong_list)){
  expect_error(
    state_sens(model_type = "continuous",
               dynamic_fn = dynamic_fn,
               parms = parms,
               reward_fn = reward_fn,
               terminal_fn = wrong_list[[i]],
               y_0 = y_0,
               times = times),
    pattern = paste0(".*", message_list[[i]], ".*")
  )
}





# Error in y_0.
# List of unacceptable inputs.
wrong_list = list(
  NULL,
  "unacceptable",
  NA_real_,
  numeric(0)
)

# List of keywords expected in error messages.
message_list = list(
  "numeric",
  "numeric",
  "NA",
  "length"
)

for(i in 1:length(wrong_list)){
  expect_error(
    state_sens(model_type = "continuous",
               dynamic_fn = dynamic_fn,
               parms = parms,
               reward_fn = reward_fn,
               terminal_fn = terminal_fn,
               y_0 = wrong_list[[i]],
               times = times),
    pattern = paste0(".*", message_list[[i]], ".*")
  )
}





# Error in times.
# List of unacceptable inputs.
wrong_list = list(
  NULL,
  "unacceptable",
  c(1, NA_real_),
  numeric(0),
  1,
  c(2,1)
  )

# List of keywords expected in error messages.
message_list = list(
  "numeric",
  "numeric",
  "NA",
  "length",
  "length",
  "ascending"
  )

for(i in 1:length(wrong_list)){
  expect_error(
    state_sens(model_type = "continuous",
               dynamic_fn = dynamic_fn,
               parms = parms,
               reward_fn = reward_fn,
               terminal_fn = terminal_fn,
               y_0 = y_0,
               times = wrong_list[[i]]),
    pattern = paste0(".*", message_list[[i]], ".*")
  )
}

for(i in 1:length(wrong_list)){
  state_sens_out3 = state_sens_out2
  state_sens_out3["times"] = wrong_list[i]
  expect_error(
    parm_sens(state_sens_out = state_sens_out3),
    pattern = paste0(".*", message_list[[i]], ".*")
  )
}





# Error in interpol.
# List of unacceptable inputs.
wrong_list = list(
  NULL,
  1,
  "unacceptable",
  c("spline", "spline")
)

# List of keywords expected in error messages.
message_list = list(
  "character",
  "character",
  "value",
  "length"
)

for(i in 1:length(wrong_list)){
  expect_error(
    state_sens(model_type = "continuous",
               dynamic_fn = dynamic_fn,
               parms = parms,
               reward_fn = reward_fn,
               terminal_fn = terminal_fn,
               y_0 = y_0,
               times = times,
               interpol = wrong_list[[i]]),
    pattern = paste0(".*", message_list[[i]], ".*")
  )
}





# Error in optional argument list.
# List of unacceptable inputs.
wrong_list = list(
  NULL,
  1,
  list(1, 2),
  list(x=1, x=2)
)

message_list = list(
  "list",
  "list",
  "name",
  "duplicate"
)

for(i in 1:length(wrong_list)){
  expect_error(
    state_sens(model_type = "continuous",
               dynamic_fn = dynamic_fn,
               parms = parms,
               reward_fn = reward_fn,
               terminal_fn = terminal_fn,
               y_0 = y_0,
               times = times,
               dynamic_fn_arglist = wrong_list[[i]]),
    pattern = paste0(".*", message_list[[i]], ".*")
  )
}

for(i in 1:length(wrong_list)){
  expect_error(
    state_sens(model_type = "continuous",
               dynamic_fn = dynamic_fn,
               parms = parms,
               reward_fn = reward_fn,
               terminal_fn = terminal_fn,
               y_0 = y_0,
               times = times,
               reward_fn_arglist = wrong_list[[i]]),
    pattern = paste0(".*", message_list[[i]], ".*")
  )
}

for(i in 1:length(wrong_list)){
  expect_error(
    state_sens(model_type = "continuous",
               dynamic_fn = dynamic_fn,
               parms = parms,
               reward_fn = reward_fn,
               terminal_fn = terminal_fn,
               y_0 = y_0,
               times = times,
               terminal_fn_arglist = wrong_list[[i]]),
    pattern = paste0(".*", message_list[[i]], ".*")
  )
}

for(i in 1:length(wrong_list)){
  expect_error(
    state_sens(model_type = "continuous",
               dynamic_fn = dynamic_fn,
               parms = parms,
               reward_fn = reward_fn,
               terminal_fn = terminal_fn,
               y_0 = y_0,
               times = times,
               state_ode_arglist = wrong_list[[i]]),
    pattern = paste0(".*", message_list[[i]], ".*")
  )
}

expect_error(
  state_sens(model_type = "continuous",
             dynamic_fn = dynamic_fn,
             parms = parms,
             reward_fn = reward_fn,
             terminal_fn = terminal_fn,
             y_0 = y_0,
             times = times,
             state_ode_arglist = list(method="iteration")),
  pattern = ".*iteration.*"
)

for(i in 1:length(wrong_list)){
  expect_error(
    state_sens(model_type = "continuous",
               dynamic_fn = dynamic_fn,
               parms = parms,
               reward_fn = reward_fn,
               terminal_fn = terminal_fn,
               y_0 = y_0,
               times = times,
               adjoint_ode_arglist = wrong_list[[i]]),
    pattern = paste0(".*", message_list[[i]], ".*")
  )
}

expect_error(
  state_sens(model_type = "continuous",
             dynamic_fn = dynamic_fn,
             parms = parms,
             reward_fn = reward_fn,
             terminal_fn = terminal_fn,
             y_0 = y_0,
             times = times,
             adjoint_ode_arglist = list(method="iteration")),
  pattern = ".*iteration.*"
)

for(i in 1:length(wrong_list)){
  expect_error(
    state_sens(model_type = "continuous",
               dynamic_fn = dynamic_fn,
               parms = parms,
               reward_fn = reward_fn,
               terminal_fn = terminal_fn,
               y_0 = y_0,
               times = times,
               numDeriv_arglist = wrong_list[[i]]),
    pattern = paste0(".*", message_list[[i]], ".*")
  )
}

for(i in 1:length(wrong_list)){
  state_sens_out3 = state_sens_out2
  state_sens_out3["dynamic_fn_arglist"] = wrong_list[i]
  expect_error(
    parm_sens(state_sens_out = state_sens_out3),
    pattern = paste0(".*", message_list[[i]], ".*")
  )
}

for(i in 1:length(wrong_list)){
  expect_error(
    parm_sens(state_sens_out = state_sens_out2,
                  numDeriv_arglist = wrong_list[[i]]),
    pattern = paste0(".*", message_list[[i]], ".*")
  )
}





# Error in verbose.
# List of unacceptable inputs.
wrong_list = list(
  NULL,
  1,
  NA,
  c(TRUE, TRUE)
)

message_list = list(
  "logical",
  "logical",
  "value",
  "length"
)

for(i in 1:length(wrong_list)){
  expect_error(
    state_sens(model_type = "continuous",
               dynamic_fn = dynamic_fn,
               parms = parms,
               reward_fn = reward_fn,
               terminal_fn = terminal_fn,
               y_0 = y_0,
               times = times,
               verbose = wrong_list[[i]]),
    pattern = paste0(".*", message_list[[i]], ".*")
  )
}

for(i in 1:length(wrong_list)){
  expect_error(
    parm_sens(state_sens_out = state_sens_out2,
              verbose = wrong_list[[i]]
    ),
    pattern = paste0(".*", message_list[[i]], ".*")
  )
}




# Error in matrices of state_sens_out.
# List of unacceptable inputs.
wrong_list = list(
  1,
  matrix("a", nrow=2, ncol=2),
  matrix(1, nrow=10, ncol=1),
  matrix(NA_real_, nrow=length(times), ncol=1)
)

message_list = list(
  "matrix",
  "numeric",
  "times",
  "NA"
)

for(i in 1:length(wrong_list)){
  state_sens_out3 = state_sens_out2
  state_sens_out3["state"] = wrong_list[i]
  expect_error(
    parm_sens(state_sens_out = state_sens_out3),
    pattern = paste0(".*", message_list[[i]], ".*")
  )
}

for(i in 1:length(wrong_list)){
  state_sens_out3 = state_sens_out2
  state_sens_out3["tdss"] = wrong_list[i]
  expect_error(
    parm_sens(state_sens_out = state_sens_out3),
    pattern = paste0(".*", message_list[[i]], ".*")
  )
}




# Error in state_sens_out being of the wrong type.
wrong_list = list(
  NULL,
  list()
)

message_list = list(
  "list",
  "missing"
)

for(i in 1:length(wrong_list)){
  expect_error(
    parm_sens(state_sens_out = wrong_list[[i]]),
    pattern = paste0(".*", message_list[[i]], ".*")
  )
}



################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

################################
# Baseline discrete-time model #
################################

# We use the pine looper moth model from Ng et al. (2023).

# Parameter values from Roseisle.
# We use a vector instead of a list to test parm_sens() more!
parms = c(r = 5.76/(10^2),
          s = 0.246,
          u = 3.644,
          xmin = 0.51,
          beta = 1.016)

dynamic_fn = function(t, y, parms){
  r = parms["r"]
  s = parms["s"]
  u = parms["u"]
  xmin = parms["xmin"]
  beta = parms["beta"]
  NN = y[1]
  XX = y[2]
  dNN = r*NN*XX*exp(-s*NN*XX + u*XX)
  dXX = xmin + exp(-beta*NN*XX)
  return( list(c(dNN, dXX)) )
}

reward_fn = function(t, y){
  NN = y[1]
  XX = y[2]
  return( -exp(-t/50) * NN )
}

terminal_fn = function(y){
  NN = y[1]
  XX = y[2]
  return( -exp(-200/50) * NN )  
}

y_0 = c(0.711, 0.543)

times = 1:200





###########################################################################
# Check that the numerical output are correct for the discrete-time model #
###########################################################################

# Load the target values.
state_sens_out1 = readRDS(file=system.file("testdata",
                                           "test2_state_sens_out.rds",
                                           package="tdsa"))
parm_sens_out1 = readRDS(file=system.file("testdata",
                                          "test2_parm_sens_out.rds",
                                          package="tdsa"))

# Calculate the current values.
state_sens_out2 = state_sens(model_type = "discrete",
                             dynamic_fn = dynamic_fn,
                             parms = parms,
                             reward_fn = reward_fn,
                             terminal_fn = terminal_fn,
                             y_0 = y_0,
                             times = times)
parm_sens_out2 = parm_sens(state_sens_out2)

# Test for equality. All the values being tested are relatively large, so it is
# fine to test the equality to a tolerance of 1e-2.
expect_equal(current = state_sens_out2[c("state", "tdss")],
             target = state_sens_out1[c("state", "tdss")],
             tolerance = 1e-2)
expect_equal(current = parm_sens_out2[["tdps"]],
             target = parm_sens_out1[["tdps"]],
             tolerance = 1e-2)





###########################################
# Catch errors in the discrete-time model #
###########################################

# Error in times.
# List of unacceptable inputs.
wrong_list = list(
  c(1, 3, 5),
  c(1.5, 2.5, 3.5)
)

# List of keywords expected in error messages.
message_list = list(
  "consecutive",
  "integer"
)

for(i in 1:length(wrong_list)){
  expect_error(
    state_sens(model_type = "discrete",
               dynamic_fn = dynamic_fn,
               parms = parms,
               reward_fn = reward_fn,
               terminal_fn = terminal_fn,
               y_0 = y_0,
               times = wrong_list[[i]]),
    pattern = paste0(".*", message_list[[i]], ".*")
  )
}

for(i in 1:length(wrong_list)){
  state_sens_out3 = state_sens_out2
  state_sens_out3["times"] = wrong_list[i]
  expect_error(
    parm_sens(state_sens_out = state_sens_out3),
    pattern = paste0(".*", message_list[[i]], ".*")
  )
}