########## SAM model configure, unchanaged in WGNSSK, determined in benchmark 2021 -------------
# BTS Q same >=5
# IBTSQ1: same Q for age 3-6, 7,8

rm(conf)

# Create a default parameter configuration object
# make a configuration file for the data, filled in with default.
conf <- defcon(dat)
names(conf)

# //1// MinAge, maxAge, maxAgePlusGroup
conf$minAge          <- minAge                  # minimum age class in assessment
conf$maxAge          <- pGrp                 # maximum age class in assessment
# here it might needs to be changed
names(surveys)
conf$maxAgePlusGroup <- c(1,0,1,1,0,0)  # last age group considered a plus group for each fleet (1 = yes, 0 =no) -> hier dus enkel catch bevat plusgroep
# first is catch
# SNS age 7 is not plus age
# IBTSQ1 age 8 is plus age
# BTS-IBTSQ3 age 10 is plus age
# //15// Define the fbar range
conf$fbarRange       <- meanFages

########## F@age in catch
# //2// Number of parameters describing F-at-age (voor de catch) = coupling of the fishing mortality states
# default
conf$keyLogFsta[1,]

# //3// # Correlation of fishing mortality across ages (0 independent, 1 compound symmetry, or 2 AR(1)
# Correlation of fishing mortality across ages
# not possible to decouple it for the time series
# (0 independent, 1 compound symmetry (= zelfde trends between age groups through time), 2 AR(1) (= age groups close together have similar F trend, when distance between ages is larger, then correlation declines),
# 3 seperable AR(1) (= parallel, one up, all up))
conf$corFlag         <- 2                     
# Compound Symmetry. This structure has constant variance and constant covariance.
# This is a first-order autoregressive structure with homogenous variances. The correlation between any two elements is equal to rho for adjacent elements, rho2 for elements that are separated by a third, and so on. is constrained so that -1<<1.


########## selectivity@age parameters in survey
# //4// Number of parameters in the survey processes - coupling of the survey catchability parameters (1st row not used - ze keyLogFsta)
# Coupling of the survey catchability parameters (nomally first row is not used, as that is covered by fishing mortality). 
conf$keyLogFpar 
names(surveys)

## BTS-Isis-early 1-8
conf$keyLogFpar[2,] <- c(0,1,2,3,4,5,6,7,-1,-1)
## BTS+IBTSQ3: 1-10+
conf$keyLogFpar[3,] <- c(8,9,10,11,12,12,12,12,12,12)
## IBTSQ1: 1-8+
conf$keyLogFpar[4,] <- c(13,14,15,15,15,15,16,17,-1,-1)
## SNS1: 1-6
conf$keyLogFpar[5,] <- c(18,19,20,21,22,23,-1,-1,-1,-1)
## SNS2: 1-6
conf$keyLogFpar[6,] <- c(24,25,26,27,28,29,-1,-1,-1,-1)
conf$keyLogFpar


# //5// Density dependent catchability power parameters (if any) 
conf$keyQpow  

########## process variance for F@age parameters
# //6// Variance of parameters on F - use a single parameter: catch process variance per age
# Coupling of process variance parameters for log(F)-process (normally only first row is used)
conf$keyVarF[1,]  
conf$keyVarF[1,] <- c(0,  1,  2,  2,  2,  2,  2,  3,  3,3)
## set 1
#conf$keyVarF[1,] <- c(0,1,2,3,4,5,6,7,8,9)
## set 2
#conf$keyVarF[1,] <- c(0,0,0,0,0,0,0,0,1,2)
# default:[1] 0 0 0 0 0 0 0 0 0 0

########## process variance for N@age parameters
# //7// Coupling of process variance parameters for log(N)-process -
conf$keyVarLogN 
#conf$keyVarLogN  <- c(0, 1, 1, 1, 1, 1, 1, 1, 1,2)
# default: [1] 0 1 1 1 1 1 1 1 1 1

########## Coupling of the variance parameters for the observations.
# //8// Coupling of the variance parameters on the observations
conf$keyVarObs
names(surveys)
## set 3
## catch 1-10+
conf$keyVarObs[1,] <- c(0,1,1,1,1,1,1,2,2,2)
## BTS-Isis-early 1-8
conf$keyVarObs[2,] <- c(3,4,4,4,4,4,5,5,-1,-1)
## BTS+IBTSQ3: 1-10+
conf$keyVarObs[3,] <- c(6,7,7,7,7,7,7,8,9,10)
## IBTSQ1: 1-8+
conf$keyVarObs[4,] <- c(11,12,12,12,12,12,13,13,-1,-1)
## SNS1: 1-6
conf$keyVarObs[5,] <- c(14,15,16,17,17,17,-1,-1,-1,-1)
## SNS2: 1-6
conf$keyVarObs[6,] <- c(18,19,20,21,22,22,-1,-1,-1,-1)
conf$keyVarObs

# Covariance structure for each fleet
# //9// Correlation at age between observations -> Covariance structure for each fleet (within fleet)
# ID = independent; AR = AR(1), US = unstructured (= they are correlated, but not in structured way, vb age 1 can correlate with age 7)
conf$obsCorStruct
levels(conf$obsCorStruct)
conf$obsCorStruct  <- c("ID", "ID", "ID", "AR", "AR", "AR")
conf$obsCorStruct  <- factor(conf$obsCorStruct, levels=c("ID", "AR", "US"))
# first is catch, and then rest are tuneing series

# //10// Coupling of correlation parameters can only be specified if the AR(1) structure is chosen above. 
# NA's indicate where correlation parameters can be specified (-1 where they cannot)
conf$keyCorObs
conf$keyCorObs[4,1:7] <- 0
conf$keyCorObs[5,1:5] <- 1
conf$keyCorObs[6,1:5] <- 2

# //11// Stock recruitment code (0 = plain random walk; 1 = Ricker; 2 = Beverton-Holt; 3 = piece-wise constant)
conf$stockRecruitmentModelCode
