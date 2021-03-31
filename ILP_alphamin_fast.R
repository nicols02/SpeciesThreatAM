#ILP_alphamin.fast implements the integer linear program in the alphamin-fast algorithm (Ferrer-Mestres et al,2020). 
#' @param N the desired number of alpha vectors (integer, user-defined)
#' @param C the candidate matrix (0,1 values), generated in alpha-min-fast
#' @param gamma_all a dataframe of all alpha-vectors, read from alpha-min-fast script (and from read.policy). (NB can actually just pass gamma_all$obsValue; no need to pass the actual alpha-vectors)
#' @return A 0-1 vector that codes which alpha vectors to keep.
solve_ILP_alphamin.fast <- function(N, gamma_all, C) {
  #install LP solve package
  #install.packages("lpSolveAPI")
  library("lpSolveAPI")
  
  n.obsVars <- length(levels(gamma_all$obsValue))
  n.alphavectors <- nrow(gamma_all)
  
 ### INPUT COEFFICIENTS MATRIX --------------------------------------------------
  # Set the coefficients of the decision variables -> C [for the objective function]
  zCoeffs <- rep(1, times= n.alphavectors)  #all coefficients in the objective function are 1
  
  # Create constraint matrix A
  #A <- matrix(NA, nrow= nrow(C)+1+n.obsVars, ncol=ncol(C))
  constraint1.coeffs <- C
  constraint2.coeffs <- rep(1, times= n.alphavectors)
  constraint3.coeffs <- matrix(0, nrow= n.obsVars, ncol= n.alphavectors)
  for(i in 1:n.obsVars){
    #constraint3.row <- rep(0, times= ncol(C))
    nonzero_ind <- which(gamma_all$obsValue==(i-1))
    constraint3.coeffs[i,nonzero_ind] <- 1
  }
  
  A <- rbind(constraint1.coeffs,  constraint2.coeffs,  constraint3.coeffs)
  
  # Right hand side for the constraints
  B <-c(rep(1, times= n.alphavectors), N, rep(1, times=n.obsVars))
  
  #create a vector with the inequalities
  inequalities.sign <- rep(">=", times= nrow(A))
  inequalities.sign[(length(inequalities.sign)-n.obsVars)] <- "<="  #can change to "=" to get exactly N alpha vectors
  
  ### INITIALIZE CONSTRAINTS MATRIX AND SET TYPE OF DECISION VARIABLES-------------
  # Set 4 constraints and 3 decision variables
  lprec <- make.lp(nrow = nrow(A), ncol = n.alphavectors)
  # Set the type of problem we are trying to solve
  lp.control(lprec, sense="min")
  
  
  ## Set type of decision variables to binary
  set.type(lprec, columns= 1:n.alphavectors, type=c("binary"))
  
  
  ### BUILD PROBLEM MATRICES/INITIALIZE MODEL ---------------------------------------------
  #switch to row entry mode (faster)
  row.add.mode(lprec, state="on")
  
  # Set objective function coefficients vector C
  set.objfn(lprec, zCoeffs)
  
  # Add constraints
  for (i in 1:nrow(A)){
    add.constraint(lprec, A[i,], inequalities.sign[i], B[i] )
  }
  
  #turn off row entry mode
  row.add.mode(lprec, state="off")

  write.lp(lprec, filename="./pomdp_solved/test.lp")  #this writes the lp setup to a file called "test.lp". Useful for debugging
  ### SOLVE PROBLEM -----------------------------------------------------------------------
  # Solve problem (for help, type ?solve.lpExtPtr)
  status <- solve(lprec)
  
  
  ### INTERROGATE OUTPUTS -----------------------------------------------------------------
  
  # Get the decision variables values
  get.variables(lprec)  
  z.vals <-  get.variables(lprec)  
  
  # Get the value of the objective function
  get.objective(lprec)  
  
 
  # Note that the default boundaries on the decision variable are c(0, 0, 0) and c(Inf, Inf, Inf)... for this problem we specified the variable type as binary so it's 0-1
  #get.bounds(lprec)
  # Boundaries can be set with following function
  #lpSolveAPI::set.bounds()
  
  #create a list with status code value and decision variables
  return.lprec <- list(status, z.vals)
  return(return.lprec)

}


#inputs for testing



# read.policy <- function(filename){
#   alphavectors.dat <- read.table(filename, sep="\t", header=FALSE, stringsAsFactors = FALSE)
#   alphavectors.dat <- alphavectors.dat$V1[-(1:3)] #drop the 3 preamble lines
#   alphavectors.dat <- alphavectors.dat[-length(alphavectors.dat)] #drop the end line
#   alphavectors.dat <- sapply(1:length(alphavectors.dat), function(i)  gsub(">", ">  ", alphavectors.dat[i], fixed=TRUE)) #adds a tab between the > and the first alpha vector value
#   alphavectors.dat <- sapply(1:length(alphavectors.dat), function(i)   strsplit(alphavectors.dat[i], "\\s+"))
#   alphavectors.dat <-data.frame(matrix(unlist(alphavectors.dat), nrow=length(alphavectors.dat), byrow=T), stringsAsFactors = FALSE)
#   alphavectors.dat <- alphavectors.dat[,-c(1,ncol(alphavectors.dat))]
#   alphavectors.dat[,1] <- gsub("action=", "", alphavectors.dat[,1])
#   alphavectors.dat[,2] <- gsub("obsValue=", "",alphavectors.dat[,2]) 
#   alphavectors.dat[,2] <- gsub(">", "",alphavectors.dat[,2])
#   alphavectors.dat <- data.frame(sapply(alphavectors.dat, as.numeric))
#   colnames(alphavectors.dat)[1:2] <- c("action", "obsValue")
#   return(alphavectors.dat) 
# }
# # N <- 7

# filename <- "./pomdp_solved/ShinySolution_TOY.policy"
# gamma_all <- read.policy(filename)
# #convert obsValue to factor for grouping
# gamma_all$obsValue <- as.factor(gamma_all$obsValue)
# 
# #make up a candidate C-matrix just for testing
# r <- nrow(gamma_all)
# c <- r
# C <- matrix(1, nrow=r, ncol= c)
# #C <- matrix(round(runif(r*c)), r, c)
# diag(C) <- 1
# 
# 
# z.Val <- solve_ILP_alphamin.fast(N, gamma_all, C)