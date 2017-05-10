#A simulation to test the value of information formulation as in http://file.scirp.org/pdf/OJS_2016102116170119.pdf. 
#X random probs with truetheta = 1, and then with truetheta = 4. We test the classical formulation, then our adaptive formulation

theta = numeric(2) #The possible values of the unknown parameter theta
theta[1] = 1
theta[2] = 4
ExTheta = 2

Obs.Cost <- 0.1

ThetaPrior = numeric(2) #Initial beliefs on theta
ThetaPrior[1] = 2/3
ThetaPrior[2] = 1/3

Losses <- numeric(2) #How we initially view the consequence of losses for the purpose of an SPRT
Losses[1] <- 2
Losses[2] <- 2

UpdatePhi <- function(phi, result){ #This function takes in our belief on phi (that is, whether the coin is biased or not) and 
  #what happened on the last coin flip. It returns a new value for our belief on phi.
  if(identical(result, 0)){ # tails
    phi <- 5*phi/(2 + 3*phi)
  }
  else{
    phi <- 5*phi/(8-3*phi)
  }
  return(phi)
}

BoundsToExamine <- numeric(2) ##These are the Real.Bounds from SPRT_KnownLosses.R. This is in the case with our losses being (2,2)
BoundsToExamine[1] <- 5/14
BoundsToExamine[2] <- 15/26

AdaptiveBounds <- numeric(2) ##Bounds from SPRT_KnownLosses.R. The value for theta is from the TeX file, assuming initial phi of 0.5.
AdaptiveBounds[1] <- 0.3446602
AdaptiveBounds[2] <- 0.5588235

OneBounds <- numeric(2) ##These are the Real.Bounds from SPRT_KnownLosses.R. This is in the case with theta having been revealed to be 1.
OneBounds[1] <- 1/4
OneBounds[2] <- 7/18

FourBounds <- numeric(2) ##These are the Real.Bounds from SPRT_KnownLosses.R. This is in the case with theta having been revealed to be 4.
FourBounds[1] <- 1/2
FourBounds[2] <- 31/42

ClassicalCost <- numeric(3000000)
AdaptiveCost <- numeric(3000000)
trueTheta <- 1 ## We begin with the true value of theta = 1, later done with 4
TypeIIErrors <- 0
floatTol <- 1e-5
phi <- 0

TestResult <- function(trueDist){ #This function takes in the true distribution of the coin, and generates a flip.
  rnum <- runif(1)
  if(identical(trueDist, 0)){ #The coin is fair
    if(rnum <= 0.5 + floatTol){
      return(1) #Heads
    } else{
      return(0) #Tails
    }
  } else{
    if(rnum <= 0.8 + floatTol){ #The coin is biased
      return(1) #Heads
    } else{
      return(0)
    }
  }
}

while(TypeIIErrors < 2000000){
  #This bit runs an SPRT first. This is where we don't know the information. We keep repeating until we make a Type II error.
  #Only then we record the loss made, store it in the ClassicalCost vector, and learn the real value of theta and repeat.
  #cat("Trial begins\n")
  cost <- 0
  phi <- 0.5
  trueDist <- 0
  test <- 0
  while(phi >= BoundsToExamine[1] - floatTol && phi <= BoundsToExamine[2] + floatTol && test < 100 ){
    TestPass <- TestResult(trueDist)
    cost <- cost+Obs.Cost
    phi <- UpdatePhi(phi, TestPass)
    test <- test+1
    #cat(phi)
  } ##After this loop, we have exited the observation bounds and are ready to make a decision
  #cat("\nflag 2\n")
  if(phi < BoundsToExamine[1] - floatTol){
    guess <- 1 #Fair
  } else{
    guess <- 0 #Biased
  }
  if(identical(guess, 1) && identical(trueDist, 0)){ #Type II, otherwise we don't care.
      #cat("Made an Error!\n")
      TypeIIErrors <- TypeIIErrors + 1
      ClassicalCost[TypeIIErrors] <- cost + trueTheta
      #cat("Current Error is: ")
      #cat(ClassicalCost[TypeIIErrors])
      #cat("\n")
      Losses[2] <- trueTheta
      New.Bounds <- numeric(2)
      if(identical(trueTheta, 1)){
        New.Bounds[1] <- OneBounds[1]
        New.Bounds[2] <- OneBounds[2]
      } else{
        New.Bounds[1] <- FourBounds[1]
        New.Bounds[1] <- FourBounds[2]
      } ##The New bounds to examine have now been set, we can now do the new SPRT and add to the cost.
      cost <- 0
      phi <- 0.5
      phiStart <- phi
      test <- 0
      trueDist <- sample(0:1, 1)
      #cat("New Trial\n")
      while(phi >= BoundsToExamine[1] - floatTol && phi <= BoundsToExamine[2] + floatTol && test < 100 ){
        #cat("Flag 3\n")
        TestPass <- TestResult(trueDist)
        cost <- cost+Obs.Cost
        phi <- UpdatePhi(phi, TestPass)
        #cat(phi)
        test <- test+1
      }
      #cat("Out\n")
      if(phi < BoundsToExamine[1] - floatTol){
        guess <- 1 #Fair
      } else{
        guess <- 0 #Biased
      }
      #cat(guess)
      #cat(" ")
      #cat(trueDist)
      #cat("\n")
      if(guess == 1 && trueDist == 0){ #Type II error
        #cat("Type II\n")
        ClassicalCost[TypeIIErrors] <- ClassicalCost[TypeIIErrors] + cost + trueTheta
      } else if(guess == 1 && trueDist == 1){ #Type I error
        #cat("Type I\n")
        ClassicalCost[TypeIIErrors] <- ClassicalCost[TypeIIErrors] + cost + 2
      } else{
        #cat("Correct\n")
        ClassicalCost[TypeIIErrors] <- ClassicalCost[TypeIIErrors] + cost
      }
      #cat("Final Error is: ")
      #cat(ClassicalCost[TypeIIErrors])
      #cat("\n")
  }
}
TypeIIErrors <- 0

while(TypeIIErrors < 2000000){
  #This bit runs an SPRT first. This is where we don't know the information. We keep repeating until we make a Type II error.
  #Only then we record the loss made, store it in the ClassicalCost vector, and learn the real value of theta and repeat.
  #cat("Trial begins\n")
  cost <- 0
  phi <- 0.5
  trueDist <- 0
  test <- 0
  while(phi >= AdaptiveBounds[1] - floatTol && phi <= AdaptiveBounds[2] + floatTol && test < 100 ){
    TestPass <- TestResult(trueDist)
    cost <- cost+Obs.Cost
    phi <- UpdatePhi(phi, TestPass)
    test <- test+1
    #cat(phi)
  } ##After this loop, we have exited the observation bounds and are ready to make a decision
  #cat("\nflag 2\n")
  if(phi < AdaptiveBounds[1] - floatTol){
    guess <- 1 #Fair
  } else{
    guess <- 0 #Biased
  }
  if(identical(guess, 1) && identical(trueDist, 0)){ #Type II, otherwise we don't care.
    #cat("Made an Error!\n")
    TypeIIErrors <- TypeIIErrors + 1
    AdaptiveCost[TypeIIErrors] <- cost + trueTheta
    #cat("Current Error is: ")
    #cat(ClassicalCost[TypeIIErrors])
    #cat("\n")
    Losses[2] <- trueTheta
    New.Bounds <- numeric(2)
    if(identical(trueTheta, 1)){
      New.Bounds[1] <- OneBounds[1]
      New.Bounds[2] <- OneBounds[2]
    } else{
      New.Bounds[1] <- FourBounds[1]
      New.Bounds[1] <- FourBounds[2]
    } ##The New bounds to examine have now been set, we can now do the new SPRT and add to the cost.
    cost <- 0
    phi <- 0.5
    phiStart <- phi
    test <- 0
    trueDist <- sample(0:1, 1)
    #cat("New Trial\n")
    while(phi >= BoundsToExamine[1] - floatTol && phi <= BoundsToExamine[2] + floatTol && test < 100 ){
      #cat("Flag 3\n")
      TestPass <- TestResult(trueDist)
      cost <- cost+Obs.Cost
      phi <- UpdatePhi(phi, TestPass)
      #cat(phi)
      test <- test+1
    }
    #cat("Out\n")
    if(phi < BoundsToExamine[1] - floatTol){
      guess <- 1 #Fair
    } else{
      guess <- 0 #Biased
    }
    #cat(guess)
    #cat(" ")
    #cat(trueDist)
    #cat("\n")
    if(guess == 1 && trueDist == 0){ #Type II error
      #cat("Type II\n")
      AdaptiveCost[TypeIIErrors] <- AdaptiveCost[TypeIIErrors] + cost + trueTheta
    } else if(guess == 1 && trueDist == 1){ #Type I error
      #cat("Type I\n")
      AdaptiveCost[TypeIIErrors] <- AdaptiveCost[TypeIIErrors] + cost + 2
    } else{
      #cat("Correct\n")
      AdaptiveCost[TypeIIErrors] <- AdaptiveCost[TypeIIErrors] + cost
    }
    #cat("Final Error is: ")
    #cat(ClassicalCost[TypeIIErrors])
    #cat("\n")
  }
}


#####DONE FOR THETA = 1 NOW. NOW DO THETA = 4.

trueTheta <- 4
TypeIIErrors <- 0

while(TypeIIErrors < 1000000){
  #This bit runs an SPRT first. This is where we don't know the information. We keep repeating until we make a Type II error.
  #Only then we record the loss made, store it in the ClassicalCost vector, and learn the real value of theta and repeat.
  #cat("Trial begins\n")
  cost <- 0
  phi <- 0.5
  trueDist <- 0
  test <- 0
  while(phi >= BoundsToExamine[1] - floatTol && phi <= BoundsToExamine[2] + floatTol && test < 100 ){
    TestPass <- TestResult(trueDist)
    cost <- cost+Obs.Cost
    phi <- UpdatePhi(phi, TestPass)
    test <- test+1
    #cat(phi)
  } ##After this loop, we have exited the observation bounds and are ready to make a decision
  #cat("\nflag 2\n")
  if(phi < BoundsToExamine[1] - floatTol){
    guess <- 1 #Fair
  } else{
    guess <- 0 #Biased
  }
  if(identical(guess, 1) && identical(trueDist, 0)){ #Type II, otherwise we don't care.
    #cat("Made an Error!\n")
    TypeIIErrors <- TypeIIErrors + 1
    ClassicalCost[TypeIIErrors+2000000] <- cost + trueTheta
    #cat("Current Error is: ")
    #cat(ClassicalCost[TypeIIErrors])
    #cat("\n")
    Losses[2] <- trueTheta
    New.Bounds <- numeric(2)
    if(identical(trueTheta, 1)){
      New.Bounds[1] <- OneBounds[1]
      New.Bounds[2] <- OneBounds[2]
    } else{
      New.Bounds[1] <- FourBounds[1]
      New.Bounds[1] <- FourBounds[2]
    } ##The New bounds to examine have now been set, we can now do the new SPRT and add to the cost.
    cost <- 0
    phi <- 0.5
    phiStart <- phi
    test <- 0
    trueDist <- sample(0:1, 1)
    #cat("New Trial\n")
    while(phi >= BoundsToExamine[1] - floatTol && phi <= BoundsToExamine[2] + floatTol && test < 100 ){
      #cat("Flag 3\n")
      TestPass <- TestResult(trueDist)
      cost <- cost+Obs.Cost
      phi <- UpdatePhi(phi, TestPass)
      #cat(phi)
      test <- test+1
    }
    #cat("Out\n")
    if(phi < BoundsToExamine[1] - floatTol){
      guess <- 1 #Fair
    } else{
      guess <- 0 #Biased
    }
    #cat(guess)
    #cat(" ")
    #cat(trueDist)
    #cat("\n")
    if(guess == 1 && trueDist == 0){ #Type II error
      #cat("Type II\n")
      ClassicalCost[TypeIIErrors+2000000] <- ClassicalCost[TypeIIErrors+2000000] + cost + trueTheta
    } else if(guess == 1 && trueDist == 1){ #Type I error
      #cat("Type I\n")
      ClassicalCost[TypeIIErrors+2000000] <- ClassicalCost[TypeIIErrors+2000000] + cost + 2
    } else{
      #cat("Correct\n")
      ClassicalCost[TypeIIErrors+2000000] <- ClassicalCost[TypeIIErrors+2000000] + cost
    }
    #cat("Final Error is: ")
    #cat(ClassicalCost[TypeIIErrors])
    #cat("\n")
  }
}
TypeIIErrors <- 0

while(TypeIIErrors < 1000000){
  #This bit runs an SPRT first. This is where we don't know the information. We keep repeating until we make a Type II error.
  #Only then we record the loss made, store it in the ClassicalCost vector, and learn the real value of theta and repeat.
  #cat("Trial begins\n")
  cost <- 0
  phi <- 0.5
  trueDist <- 0
  test <- 0
  while(phi >= AdaptiveBounds[1] - floatTol && phi <= AdaptiveBounds[2] + floatTol && test < 100 ){
    TestPass <- TestResult(trueDist)
    cost <- cost+Obs.Cost
    phi <- UpdatePhi(phi, TestPass)
    test <- test+1
    #cat(phi)
  } ##After this loop, we have exited the observation bounds and are ready to make a decision
  #cat("\nflag 2\n")
  if(phi < AdaptiveBounds[1] - floatTol){
    guess <- 1 #Fair
  } else{
    guess <- 0 #Biased
  }
  if(identical(guess, 1) && identical(trueDist, 0)){ #Type II, otherwise we don't care.
    #cat("Made an Error!\n")
    TypeIIErrors <- TypeIIErrors + 1
    AdaptiveCost[TypeIIErrors] <- cost + trueTheta
    #cat("Current Error is: ")
    #cat(ClassicalCost[TypeIIErrors])
    #cat("\n")
    Losses[2] <- trueTheta
    New.Bounds <- numeric(2)
    if(identical(trueTheta, 1)){
      New.Bounds[1] <- OneBounds[1]
      New.Bounds[2] <- OneBounds[2]
    } else{
      New.Bounds[1] <- FourBounds[1]
      New.Bounds[1] <- FourBounds[2]
    } ##The New bounds to examine have now been set, we can now do the new SPRT and add to the cost.
    cost <- 0
    phi <- 0.5
    phiStart <- phi
    test <- 0
    trueDist <- sample(0:1, 1)
    #cat("New Trial\n")
    while(phi >= BoundsToExamine[1] - floatTol && phi <= BoundsToExamine[2] + floatTol && test < 100 ){
      #cat("Flag 3\n")
      TestPass <- TestResult(trueDist)
      cost <- cost+Obs.Cost
      phi <- UpdatePhi(phi, TestPass)
      #cat(phi)
      test <- test+1
    }
    #cat("Out\n")
    if(phi < BoundsToExamine[1] - floatTol){
      guess <- 1 #Fair
    } else{
      guess <- 0 #Biased
    }
    #cat(guess)
    #cat(" ")
    #cat(trueDist)
    #cat("\n")
    if(guess == 1 && trueDist == 0){ #Type II error
      #cat("Type II\n")
      AdaptiveCost[TypeIIErrors+2000000] <- AdaptiveCost[TypeIIErrors+2000000] + cost + trueTheta
    } else if(guess == 1 && trueDist == 1){ #Type I error
      #cat("Type I\n")
      AdaptiveCost[TypeIIErrors+2000000] <- AdaptiveCost[TypeIIErrors+2000000] + cost + 2
    } else{
      #cat("Correct\n")
      AdaptiveCost[TypeIIErrors+2000000] <- AdaptiveCost[TypeIIErrors+2000000] + cost
    }
    #cat("Final Error is: ")
    #cat(ClassicalCost[TypeIIErrors])
    #cat("\n")
  }
}

cat(mean(ClassicalCost))
cat("\n")
cat(mean(AdaptiveCost))


