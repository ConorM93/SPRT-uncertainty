#This code is used to determine the bounds on probability (and likelihood ratio) of an SPRT where Type I and Type II
#errors result in a fixed and known loss. We consider the case where we make one of two observations that can be characterised as
#a pass or a fail. The output of this program gives the bounds (if any) where we should observe before making a decision. They go one
#step into the future for bounds on an SPRT

Losses <- numeric(2)
Losses[1] <- 2 #Consequence of Type I error
Losses[2] <- 3 #Consequence of Type II error
LossEquality <- (Losses[2])/(Losses[1] + Losses[2])

Prior <- numeric(2) #Prior gives our prior probability of a system following null and alternative hypothesis respsectively to pass the test.
Prior[1] <- 0.5
Prior[2] <- 0.8
Obs.Cost <- 0.1

Bounds.To.Examine <- function(Prior, Losses){
  #Will give bounds on the prior to have the posterior be on either side of LossEquality for a success or failure observation.
  LossEquality <- (Losses[2])/(Losses[1] + Losses[2])
  InterBounds <- numeric(2)
  InterBounds[1] <- Prior[2]/(Prior[1]/LossEquality - Prior[1] + Prior[2])
  InterBounds[2] <- (1-Prior[2])/((1-Prior[1])/LossEquality - (1-Prior[1]) + (1-Prior[2]))
  return(InterBounds)
}

Observation.Bounds <- function(InterBounds, Prior, Losses, Obs.Cost){
  ## We assume that we are in the middle of the Bounds.To.Examine (where one posterior is on each side of the loss function), and calculate
  ## the losses in this interval. Rearrange to get probabilities where we should obeerve. Bounds[1] Will return a probability that we should be
  ## greater than, and Bounds[2] a probability we should be less than. The third and fourth components are the parts of the new loss (numerical
  ## and probability coefficient).
  Bounds <- numeric(4)
  if(InterBounds[2] < InterBounds[1]){ ##The risk will be 1-phi(Fail) and phi(Pass)
    Bounds[1] <- (Losses[2]*(Prior[2] - 1) - Obs.Cost)/(Losses[1]*(Prior[1]-1) + Losses[2]*(Prior[2] - 1))
    Bounds[2] <- (Losses[2]*Prior[2] - Obs.Cost)/(Losses[1]*Prior[1] + Losses[2]*Prior[2])
    Bounds[3] <- Losses[2]*(1 - Prior[2]) + Obs.Cost
    Bounds[4] <- Losses[1]*(Prior[1]) + Losses[2]*(Prior[2] - 1)
  }
  else{ ## Here it will be (1-phi)Pass + phi(Fail)
    Bounds[1] <- (Losses[1]*Prior[2] + Obs.Cost)/(Losses[2]*(Prior[1]-1) + Losses[1]*(Prior[2]+1))
    Bounds[2] <- (Losses[1]*(Prior[2]) + Obs.Cost - Losses[2])/(Losses[2]*(Prior[1]-2) + Losses[1]*(Prior[2]))
    Bounds[3] <- Losses[1]*(Prior[2]) + Obs.Cost
    Bounds[4] <- Losses[2]*(1 - Prior[1])-Losses[1]*(Prior[2])
  }
  return(Bounds)
  
}

InterBounds <- Bounds.To.Examine(Prior, Losses)
ObservationBounds <- Observation.Bounds(InterBounds, Prior, Losses, Obs.Cost)
Updated <- 0

Real.Bounds <- numeric(2)
Real.Bounds[1] <- min(InterBounds[1], InterBounds[2])
Real.Bounds[2] <- max(InterBounds[1], InterBounds[2])

#cat(LossEquality)
cat(InterBounds[1])
cat(InterBounds[2])
#cat(ObservationBounds[1])
#cat(ObservationBounds[2])


if(ObservationBounds[1] > ObservationBounds[2]){
  cat("Nothing to gain observing. Just decide on the initial risk profile.")
} else{
  if(ObservationBounds[1] > Real.Bounds[1] & ObservationBounds[1] < Real.Bounds[2]){
    Real.Bounds[1] <- ObservationBounds[1]
    Updated <- 1
  }
  if(ObservationBounds[2] < Real.Bounds[2] & ObservationBounds[2] > Real.Bounds[1]){
    Real.Bounds[2] <- ObservationBounds[2]
    Updated <- 1
  }
}

if(identical(Updated, 0)){
  cat("Nothing to gain observing. Just decide on the initial risk profile")
} else{
  cat("Observation can be beneficial, only observe if your prior on the null hypothesis being true is between the following probabilities, and sample until your posterior goes above or below them: ")
  cat(Real.Bounds[1])
  cat(" and ")
  cat(Real.Bounds[2])
  
  cat(". In between these bounds, the risk is equal to: ")
  cat(ObservationBounds[3])
  cat(" + ")
  cat(ObservationBounds[4])
  cat("*phi, phi the original prior probability")
}


