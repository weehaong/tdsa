% File src/library/tdsa/man/state_sens.Rd
\name{state_sens}
\alias{state_sens}
\title{Time-Dependent State Sensitivities}

\description{
  \loadmathjax
  
  Function to calculate time-dependent state sensitivities. Both continuous- and discrete-time models are supported.
  
  \itemize{
    \item{\bold{Continuous-time models:} Assume that the dynamics of the system can be described by first-order ordinary differential equations (and initial conditions)
  \mjsdeqn{\frac{d\mathbf{y}(t)}{dt} = \mathbf{g}(t, \mathbf{y}(t), \mathbf{b}(t)), \quad \mathbf{y}(t_0)=\mathbf{y}_0,}
  where \mjseqn{\mathbf{y}(t)} is the \mjseqn{n_y}-dimensional state vector and \mjseqn{\mathbf{b}(t)} the model parameters at time \mjseqn{t}. Also assume there is some reward of interest (e.g., a management objective) that can be written as
  \mjsdeqn{J = \int_{t_0}^{t_1}f(t,\mathbf{y}(t)) \, dt + \Psi(\mathbf{y}(t_1)),}
  where \mjseqn{t_0} and \mjseqn{t_1} are the initial and final times, and \mjseqn{\Psi(\mathbf{y}(t_1))} represents a terminal payoff. (We will explain how to deal with non-standard objectives that cannot be expressed in such a form in a future vignette.)}
    
    \item{\bold{Discrete-time models:} Choose the units of time so that the time steps take consecutive integer values. Assume that the dynamics of the system can be described by first-order recurrence equations (and initial conditions)
  \mjsdeqn{\mathbf{y}(t+1) = \mathbf{g}(t, \mathbf{y}(t), \mathbf{b}(t)), \quad \mathbf{y}(t_0)=\mathbf{y}_0.}
  Also assume that the reward can be written as
  \mjsdeqn{J = \sum_{t=t_0}^{t_1-1}f(t,\mathbf{y}(t)) + \Psi(\mathbf{y}(t_1)).}}
  }
  
  We now consider a sudden perturbation of the \mjseqn{i}th state variable \mjseqn{y_i} at time \mjseqn{t}. Even though the perturbation only occurs explicitly at time \mjseqn{t}, it will "nudge" the system to a new state trajectory, so all state variables after time \mjseqn{t} will be affected. Hence the reward is affected by both the explicit perturbation as well as the "downstream" changes. We define the time-dependent state sensitivity \mjseqn{\lambda_i(t)} as the sensitivity of the reward to such a perturbation; the sensitivity is time-dependent because it depends on the time \mjseqn{t} at which the perturbation occurs. See Ng et al. (in press, submitted) for a more precise definition.
  
  This function calculates the sensitivity \mjseqn{\lambda_i(t)} for every \mjseqn{i} from \mjseqn{1} to \mjseqn{n_y}, at every \mjseqn{t} between \mjseqn{t_0} and \mjseqn{t_1}. Hence, the user can identify the state variable and the time of perturbation that would have the largest impact on the reward.
  
  The output of this function can also be used as the input argument of the function \code{\link{parm_sens}} to calculate time-dependent parameter sensitivities.
}

\usage{
state_sens(
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
)
}

\arguments{
  \item{model_type}{Whether the model is continuous- or discrete-time. Allowed values are \code{"continuous"} and \code{"discrete"}.}
  
  \item{dynamic_fn}{Dynamic equations of the state variables. Function of the form \code{function(t, y, parms, \dots)}, with arguments
  \describe{
    \item{\code{t}}{Time \mjseqn{t}, a single number.}
    
    \item{\code{y}}{State vector \mjseqn{\mathbf{y}}, a numeric vector of length \mjseqn{n_y}.}
        
    \item{\code{parms}}{Object used to specify the model parameters \mjseqn{\mathbf{b}(t)}. Allowed structures are:
    \itemize{
      \item{A numeric object. This can be a vector, matrix or array.}
      \item{A function of the form \code{function(t)}, that returns a numeric object. This is used for time-varying parameters. See "Details".}
      \item{A list containing any combination of the above.}
      \item{\code{NULL} if the user prefers to specify parameter values elsewhere.}
    }
    We have imposed these restrictions to facilitate parameter sensitivity calculations using \code{\link{parm_sens}}, but nonetheless they should be mild enough to permit most use cases. See "Details."}
 
    \item{\code{\dots}}{Additional arguments.}
  }
  Function must return a list, whose first element is \mjseqn{\mathbf{g}(t,\mathbf{y},\mathbf{b}(t))}, a numeric vector of length \mjseqn{n_y}. Other elements of the returned list are optional, and correspond to additional numeric quantities that the user wants to monitor at each time step.
  
  Note to users of the \pkg{deSolve} package: Any function that can be used as \code{func} in \code{\link[deSolve]{ode}} can be used as \code{dynamic_fn}, provided \code{parms} has one of the allowed structures described above.}
  
  \item{parms}{Argument passed to \code{dynamic_fn}.}
  
  \item{reward_fn}{Integrand (continuous-time model) or summand (discrete-time model) in reward function. Function of the form \code{function(t, y, \dots)}, with arguments
  \describe{
    \item{\code{t}}{Time \mjseqn{t}, a single number.}
    \item{\code{y}}{State vector \mjseqn{\mathbf{y}}, a numeric vector of length \mjseqn{n_y}.}
    \item{\code{\dots}}{Additional arguments.}
  }
  Function must return \mjseqn{f(t,\mathbf{y})}, a single number.}
  
  \item{terminal_fn}{Terminal payoff in reward function. Function of the form \code{function(y, \dots)}, with arguments
  \describe{
    \item{\code{y}}{State vector \mjseqn{\mathbf{y}}, a numeric vector of length \mjseqn{n_y}.}
    \item{\code{\dots}}{Additional arguments.}
  }
  Function must return \mjseqn{\Psi(\mathbf{y})}, a single number.}
  
  \item{y_0}{Initial conditions of the dynamical system \mjseqn{\mathbf{y}_0}, a numeric vector of length \mjseqn{n_y}.}
  
  \item{times}{Numeric vector containing the time steps at which the state variables and sensitivities will be evaluated. Must be in ascending order, and not contain duplicates. The first and last time steps must be \mjseqn{t_0} and \mjseqn{t_1}.
  
  For continuous-time models, this is the discretisation of the continuous interval between \mjseqn{t_0} and \mjseqn{t_1}, so the smaller the step sizes, the more accurate the numerical results.
  
  For discrete-time models, this must be a vector of consecutive integers, so \mjseqn{t_0} and \mjseqn{t_1} must themselves be integers.
  }
  
  \item{interpol}{Only used for continuous-time models. Whether to perform spline or linear interpolation of the numerical solutions of the state variables. Allowed values are \code{"spline"} (the default) and \code{"linear"}. The former uses the function \code{\link[stats]{splinefun}}, while the latter uses the function \code{\link[stats]{approxfun}}, both from the \pkg{stats} package.}
  
  \item{dynamic_fn_arglist, reward_fn_arglist, terminal_fn_arglist}{Optional lists of arguments passed to \code{dynamic_fn}, \code{reward_fn} and \code{terminal_fn}. Can be used to specify any additional arguments \code{\dots} that these functions were designed to accept.}
  
  \item{state_ode_arglist, adjoint_ode_arglist}{Only used for continuous-time models. Optional lists of arguments passed to the function \code{\link[deSolve]{ode}} from the \pkg{deSolve} package, when solving the dynamic and adjoint equations respectively. Can be used to specify the method, and arguments controlling the method. See "Details" for the definition of the adjoint equations. (Discrete-time models will always use the "iteration" method, so these arguments are ignored.)}
  
  \item{numDeriv_arglist}{Optional list of arguments passed to the functions \code{\link[numDeriv]{grad}} and \code{\link[numDeriv]{jacobian}} from the \pkg{numDeriv} package, when calculating derivatives. Can be used to specify the method, and arguments controlling the method. For example, if the adjoint equations take too long to solve, try setting \code{numDeriv_arglist = list(method="simple")} to replace Richardson's extrapolation by a simple one-sided epsilon difference.}
  
  \item{verbose}{Whether to display progress messages in the console. Either \code{TRUE} (the default) or \code{FALSE}.}
}

\details{

  \subsection{Algorithm}{
    This function uses the adjoint method to calculate the sensitivity for every state variable at every time step in \code{times}. It automates the following sequence of steps:
    \enumerate{
      \item{Obtain numerical solutions of the state variables at every time step, by solving the dynamic equations \code{dynamic_fn} forward in time using \code{\link[deSolve]{ode}} from \pkg{deSolve}, with initial conditions \code{y_0}. (Note that \code{\link[deSolve]{ode}} can also support discrete-time models using the "iteration" method.)}
  
      \item{For continuous-time models, create a function that interpolates the numerical solutions of the state variables, using either \code{\link[stats]{splinefun}} or \code{\link[stats]{approxfun}} from \pkg{stats}. This step is not required for discrete-time models.}
    
      \item{Define a function (internally called \code{adjoint_fn}) that returns the RHS of the adjoint equations.
      \itemize{
        \item{\bold{Continuous-time models:} The adjoint equations are the first-order ordinary differential equations
      \mjsdeqn{\frac{d\lambda_i(t)}{dt} = -\left.\frac{\partial f(t,\mathbf{y})}{\partial y_i}\right|_{\mathbf{y}=\mathbf{y}(t)} -\sum_j \lambda_j(t) \left.\frac{\partial g_j(t,\mathbf{y})}{\partial y_i}\right|_{\mathbf{y}=\mathbf{y}(t)}.}}
        \item{\bold{Discrete-time models:} The adjoint equations are the first-order recurrence equations
        \mjsdeqn{\lambda_i(t-1) = \left.\frac{\partial f(t-1,\mathbf{y})}{\partial y_i}\right|_{\mathbf{y}=\mathbf{y}(t-1)} + \sum_j \lambda_j(t) \left.\frac{\partial g_j(t-1,\mathbf{y})}{\partial y_i}\right|_{\mathbf{y}=\mathbf{y}(t-1)}.}}
      }
      Inside \code{adjoint_fn}, we use \code{\link[numDeriv]{jacobian}} and \code{\link[numDeriv]{grad}} from \pkg{numDeriv} to evaluate the Jacobian and gradient of \code{dynamic_fn} and \code{reward_fn}. For discrete-time models, the values of the state variables (at which these derivatives are evaluated) come directly from the numerical solutions from Step 1. For continuous-time model, ODE solvers need \code{adjoint_fn} to work at any time \mjseqn{t} and not just those in \code{times}, so the values of the state variables instead come from the interpolation function from Step 2.}
    
      \item{Calculate the terminal conditions of the adjoint system
      \mjsdeqn{\lambda_i(t_1)=\left.\frac{\partial \Psi(\mathbf{y})}{\partial y_i}\right|_{\mathbf{y}=\mathbf{y}(t_1)},}
      using \code{\link[numDeriv]{grad}} to evaluate the gradient of \code{terminal_fn}.}
    
      \item{Obtain numerical solutions of the adjoint variables, by solving the adjoint equations backward in time using \code{\link[deSolve]{ode}}, with the terminal conditions from Step 4. The values of the adjoint variables are equal to the time-dependent state sensitivities.}
    }
  }
  
  \subsection{Parameters in \code{dynamic_fn}}{
    As mentioned earlier, the output of \code{\link{state_sens}} can be used as the input argument of the function \code{\link{parm_sens}} to calculate parameter sensitivities. The following points are important if the user wants to do so, and can be ignored otherwise.
    \itemize{
      \item{There are four ways to specify parameters in \code{dynamic_fn}: (1) using \code{parms}, (2) using the additional arguments \code{\dots}, (3) within the environment of \code{dynamic_fn} itself, and (4) in the global environment. The function \code{\link{parm_sens}} will calculate sensitivities for \bold{all} the parameters specified using (1), and none of the parameters specified using (2), (3) or (4). These calculations involve taking numerical derivatives of \code{dynamic_fn} with respect to the parameters, which is why we have imposed some (relatively mild) restrictions on the structure of \code{parms}.}
      
      \item{The usual way to implement time-varying parameters is to have \code{parms} be a function of time (or a list containing such a function), which is then evaluated at \code{t} within \code{dynamic_fn} itself to return the current parameter values. When calculating parameter sensitivities, it is important that the evaluation be at \code{t} and not at a shifted time like \code{t-1}. This is because to us the user-specified \code{dynamic_fn} is a "black box", so there is no way we would know if \code{dynamic_fn} is using an evaluation like \code{parms(t-1)} to obtain the current parameter values instead of \code{parms(t)}.}
    }
  }
}

\value{
  A list with the following elements:
  \item{\code{model_type}, \code{dynamic_fn}, \code{parms}, \code{dynamic_fn_arglist}, \code{times}}{Same as the input arguments. Included in the output because they are needed for parameter sensitivity calculations using \code{\link{parm_sens}}.}
  
  \item{state}{Numerical solutions of the state variables evaluated at \code{times}. Matrix with as many rows as the length of \code{times}, and as many columns as \mjseqn{n_y} (and possibly more; see below). The \code{i}th row corresponds to \mjseqn{(y_1(t), y_2(t), ..., y_{n_y}(t))}, where \mjseqn{t} is the time step \code{times[i]}.
  
  If there are additional numeric quantitities that the user wants to monitor at each time step (these are the optional elements in the list returned by \code{dynamic_fn}), they will appear as additional columns to the right.
  
  Note to users of the \pkg{deSolve} package: \code{state} is the usual output returned by \code{\link[deSolve]{ode}}, except with the first column (corresponding to \code{times}) removed. This is for consistency with the output returned by \code{\link{parm_sens}}.}
  
  \item{\code{tdss}}{Time-dependent state sensitivities evaluated at \code{times}. Matrix with as many rows as the length of \code{times}, and as many columns as \mjseqn{n_y}. The \code{i}th row corresponds to \mjseqn{(\lambda_1(t), \lambda_2(t), ..., \lambda_{n_y}(t))}, where \mjseqn{t} is the time step \code{times[i]}.}
}

\references{
Ng, W. H., Myers, C. R., McArt, S., & Ellner, S. P. (2023). A time for every purpose: using time-dependent sensitivity analysis to help understand and manage dynamic ecological systems. American Naturalist, 202, 630-654. \doi{10.1086/726143}. eprint \doi{10.1101/2023.04.13.536769}.

Ng, W. H., Myers, C. R., McArt, S., & Ellner, S. P. (2023). tdsa: An R package to perform time-dependent sensitivity analysis. Methods in Ecology and Evolution, 14, 2758-2765. \doi{10.1111/2041-210X.14216}.
}

\seealso{
  \code{\link{parm_sens}} for time-dependent parameter sensitivities.
}

\examples{
# Load the TDSA package.
library(tdsa)



# We will consider an example involving the translocation of individuals into a
# sink habitat that is being restored.

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
# J = integrate(w y(t), lower=t_0, upper=t_1)
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
# (so the step size is 0.03).
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
}