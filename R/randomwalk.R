#' @title randomwalk
#' @description This function generates random reward probabilities for the whole experiment
#' @param Narms Number of bandit arms, Default: 2
#' @param Ntrials Number of trials, Default: 100
#' @param tau Standard deviation for the noise normal distribution of each arm, Default: 0.02
#' @param rho True correlation between all arms. default should be zero, Default: 0
#' @param upper.bound Maximal limit of reward probability, Default: 0.85
#' @param lower.bound Minimal limit of reward probability, Default: 0.15
#' @return Returns a matrix (Ntrials X Narms) showing the reward probabilities in each trial.
#' @details The randomwalk function is responsible for generating the rewards in your experiment.
#' Each arm has a different reward probability and it drifts along the experiment. The rational behind the drift is to maintain learning.
#' @examples
#' \dontrun{
#' {
#'R=
#'randomwalk(Narms=4,
#'           Ntrials=50,
#'           tau            =.02,#standard deviation for the noise normal distribution of each arm
#'           rho            =0, #true correlation between all arms. default should be zero
#'           upper.bound   =0.85,
#'           lower.bound   =0.15)

#' #check correlations
#'cor(R)
#'  }
#' }
#' @seealso
#'  \code{\link[MASS]{mvrnorm}}
#' @rdname randomwalk
#' @export
#' @importFrom MASS mvrnorm
randomwalk<-function(Narms=2,Ntrials=100,tau=0,rho=0.02,
                     upper.bound=0.85,lower.bound=0.15){
  library(MASS)


  #calculate var-cov matrix (i.e., Sigma) according to tau and rho
  #this will be later used as the var-cov matrix for a multivariate normal distribution that will generate noise for the randomwalk
  tau        = rep(tau,Narms)
  cor_u      = matrix(rep(rho, Narms^2), nrow = Narms)
  diag(cor_u)= 1
  Sigma_u    = diag(tau, Narms, Narms) %*%  cor_u %*%  diag(tau, Narms, Narms) #converting cor matrix to var-cov matrix

  #pre-allocate matrix with random starting points
  R          =matrix(NA,Ntrials,Narms)
  R[1,]      =lower.bound+(upper.bound-lower.bound)*runif(Narms)


  #generate the randomwalk
  for(t in 2:Ntrials){
    R[t,]=R[t-1,]+MASS::mvrnorm(n = 1, rep(0, Narms), Sigma_u)

    R[t,R[t,]>upper.bound]=  upper.bound
    R[t,R[t,]<lower.bound]=  lower.bound
  }

  #add column names
  colnames(R) <- paste0("arm_", 1:Narms)

  return(R)

}
