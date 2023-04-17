library(tdsa)

# -----------
# Background.
# -----------
# Consider an organism in a sink habitat, where the per-capita loss rate
# (mortality and emigration combined) exceeds the per-capita unregulated birth
# rate, so the population is only maintained through immigration. However, the
# mortality rate is expected to decrease over time due to ongoing habitat
# restoration efforts, so the population should eventually become
# self-sustaining. The population dynamics is hence given by
#
#   dy(t)/dt = b*y(t)*(1 - a*y(t)) - mu(t)*y(t) + sigma,
#
# where y(t) is the population at time t, b the unregulated per-capita birth
# rate, a the coefficient for reproductive competition, mu(t) the time-varying
# per-capita loss rate, and sigma the immigration rate. We assume that mu(t) 
# starts off above b (so it is a sink habitat), but decreases as a sigmoidal
# and eventually falls below b (so the population becomes self-sustaining).
#
#
# The organism provides an important ecosystem service. Over a management period
# from t_0 to t_1, we ascribe an economic value to the organism
#
# J= integrate(w y(t), lower=t_0, upper=t_1) + v y(t_1).
#
# Here, w is the per-capita rate at which the service is provided, so the
# integral gives the total value of the service accumulated over the period.
# However, we also want to ascribe value to maintaining a large population at
# the end of the management period, so the second term corresponds to a terminal
# payoff where v is the ascribed value per individual.
#
#
# Say we want to translocate individuals to the habitat to speed up the
# population recovery and increase the reward J. What is the best time to do so
# in order to maximise the increase in the reward? As early as possible? Or only
# when the loss rate has become low enough that the population can sustain
# itself? A one-off translocation causes a small, sudden increase in the
# population size, so it is useful to look at the time-dependent state
# sensitivity. Alternatively, we can interpret the translocation as a brief
# spike in the immigration rate sigma, so we can also look at the time-dependent
# parameter sensitivity of sigma.



# ------------------------------
# Preparing the input arguments.
# ------------------------------
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
# (so the step size is 0.02).
times = seq(0, 30, length.out=1001)



# -----------------------------------------------
# Calculating time-dependent state sensitivities.
# -----------------------------------------------
state_sens_out = state_sens(
  model_type = "continuous",
  dynamic_fn = dynamic_fn,
  parms = parms,
  reward_fn = reward_fn,
  terminal_fn = terminal_fn,
  y_0 = y_0,
  times = times
)

# Plot the per-capita unregulated birth and loss rates.
plot(times, parms[["mu"]](times), type="l", lwd=2,
     xlab="Time (year)", ylab="Demographic rate (/year)")
abline(h=parms[["b"]], col="red", lwd=2)
legend("topright", col=c("red", "black"), lwd=2, bty="n",
       legend=c("Birth rate", "Loss rate"))

# Plot the population size.
plot(times, state_sens_out[["state"]][,1], type="l", lwd=2,
     xlab="Time (year)", ylab="Population size y")

# Plot the time-dependent state sensitivity. Peaks at around t=10, which is
# roughly when mu and b intersects, so the population has just become
# self-sustaining.
plot(times, state_sens_out[["tdss"]][,1], type="l", lwd=2,
     xlab="Time (year)", ylab="State sensitivity of y")



# ---------------------------------------------------
# Calculating time-dependent parameter sensitivities.
# ---------------------------------------------------
parm_sens_out = parm_sens(
  state_sens_out = state_sens_out
)

# Plot the parameter sensitivity of sigma.
plot(times, parm_sens_out[["tdps"]][["sigma"]][,1], type="l", lwd=2,
     xlab="Time (year)", ylab="Param. sensitivity of sigma")