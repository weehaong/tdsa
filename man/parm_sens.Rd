% File src/library/tdsa/man/parm_sens.Rd
\name{parm_sens}
\alias{parm_sens}
\title{Time-Dependent Parameter Sensitivities}

\description{
  \loadmathjax
  
  Function to calculate time-dependent parameter sensitivities.
  
  Assume the same model and reward as described in \code{\link{state_sens}}. Unlike perturbations of the state variables, since the model parameters are not treated as dynamic quantities (even if they may be time-varying), an explicit perturbation of a parameter will only temporarily change the parameter while the perturbation lasts. Now consider a very brief perturbation (i.e., a sharp spike or dip) of the parameter \mjseqn{b_i}, centered at time \mjseqn{t}. We define the time-dependent parameter sensitivity \mjseqn{\kappa_i(t)} as the sensitivity of the reward to such a perturbation. See Ng et al. (in review, in prep) for a more precise definition.
  
  This function uses the output returned by \code{\link{state_sens}} (which contain elements \code{parms} and \code{times}) to calculate the sensitivity for every parameter in \code{parms} at every time step in \code{times}.
  
  See \code{\link{state_sens}} for examples.
}



\usage{
parm_sens(
  state_sens_out,
  numDeriv_arglist = list()
)
}



\arguments{
  \item{state_sens_out}{Output returned by \code{\link{state_sens}}. List containing the elements \code{model_type}, \code{dynamic_fn}, \code{parms}, \code{dynamic_fn_arglist}, \code{times}, \code{state} and \code{tdss}.
  
  To make this help page easier to read, from now on, any time we mention \code{dynamic_fn}, \code{parms}, etc., we refer to the corresponding elements in \code{state_sens_out}.}
  
  \item{numDeriv_arglist}{Optional list of arguments passed to the function \code{\link[numDeriv]{jacobian}} from the \pkg{numDeriv} package, when calculating derivatives. Can be used to specify the method, and arguments controlling the method. For example, if the parameter sensitivities take too long to calculate, try setting \code{numDeriv_arglist = list(method="simple")} to replace Richardson's extrapolation by a simple one-sided epsilon difference.}
}



\details{
  Parameter sensitivities can be obtained from the state sensitivities using the following formulae.
  \itemize{
    \item{\bold{Continuous-time models:} 
    \mjsdeqn{ \kappa_i(t) = \sum_{j=1}^{n_y} \left. \frac{\partial g_j(t,\mathbf{y}(t), \mathbf{b})}{\partial b_i}\right\vert_{\mathbf{b}=\mathbf{b}(t)} \lambda_j(t), }
  where \mjseqn{\lambda_j(t)} is the state sensitivity of \mjseqn{y_j} at time \mjseqn{t}.}
    \item{\bold{Discrete-time models:} 
    \mjsdeqn{ \kappa_i(t) = \sum_{j=1}^{n_y} \left. \frac{\partial g_j(t,\mathbf{y}(t), \mathbf{b})}{\partial b_i}\right\vert_{\mathbf{b}=\mathbf{b}(t)} \lambda_j(t+1), }
    where \mjseqn{\lambda_j(t+1)} is the state sensitivity of \mjseqn{y_j} at time step \mjseqn{t+1}. This also means that the parameter sensitivities are always zero at the final time step \mjseqn{t_1}, because \mjseqn{\lambda_j(t_1+1)=0} for all \mjseqn{j}.}
  }
  
  To apply these formulae, we need to calculate derivatives of \code{dynamic_fn} with respect to \code{parms}, using the function \code{\link[numDeriv]{jacobian}} from \pkg{numDeriv}. The main coding challenge that we have addressed is to make this work even when the structure of \code{parms} is only under the relatively mild restrictions imposed in \code{\link{state_sens}}.
}



\value{
  A list with the following elements:
  \item{\code{times}}{Time steps at which the parameter sensitivities are evaluated, a numeric vector. Same as \code{times} from \code{state_sens_out}.}
  
  \item{\code{tdps}}{Time-dependent parameter sensitivities. An object whose structure depends on the structure of \code{parms}.
  \itemize{
    \item{If \code{parms} is a numeric object, then \code{tdps} is an array with one more index than the object, so a vector becomes a matrix, a matrix becomes a 3-index array, etc. The first index is new and is associated with the time step.}
    
    \item{If \code{parms} is a function of the form \code{function(t)} that returns a numeric object (i.e., time-varying parameters), then \code{tdps} is an array with one more index than the returned object. Again, the first index is new and is associated with the time step.}
    
    \item{If \code{parms} is a list containing any combination of numeric objects and functions, then \code{tdps} is a list of the same length, with the previous "rules" applied element-wise.}
  }
  
  As a concrete example, say \code{parms} is a matrix of dimension \code{c(3,2)}, and \code{times} a vector of length \code{50}. Then \code{tdps} is a 3-index array of dimension \code{c(50,3,2)}, and the array element \code{tdps[20,1,2]} gives the sensitivity for the parameter \code{parms[1,2]} at time step \code{times[20]}.
  }
}

\references{
Ng, W. H., Myers, C. R., McArt, S., & Ellner, S. P. (in review). A time for every purpose: using time-dependent sensitivity analysis to help understand and manage dynamic ecological systems. American Naturalist. \doi{10.1101/2023.04.13.536769}.

Ng, W. H., Myers, C. R., McArt, S., & Ellner, S. P. (in prep). tdsa: An R package to perform time-dependent sensitivity analysis. Methods in Ecology and Evolution.
}

\seealso{
  \code{\link{state_sens}} for time-dependent state sensitivities.
}