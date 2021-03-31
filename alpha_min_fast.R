#alpha_min_fast 
#
#' #' #read.policy is a function that reads a sarsop .policy file and formats it into an R dataframe
#' #' @param filename is a pointer to the file name where the .policy file is stored
#' #' @example
#' #' outfileName <- paste("./pomdp_solved/", "potoroo","_", paste(benefitRatio, collapse="_"),".policy", sep="")
#' #' policy <- read.policy(outfileName)
#' read.policy <- function(filename){
#'   alphavectors.dat <- read.table(filename, sep="\t", header=FALSE, stringsAsFactors = FALSE)
#'   alphavectors.dat <- alphavectors.dat$V1[-(1:3)] #drop the 3 preamble lines
#'   alphavectors.dat <- alphavectors.dat[-length(alphavectors.dat)] #drop the end line
#'   alphavectors.dat <- sapply(1:length(alphavectors.dat), function(i)  gsub(">", ">  ", alphavectors.dat[i], fixed=TRUE)) #adds a tab between the > and the first alpha vector value
#'   alphavectors.dat <- sapply(1:length(alphavectors.dat), function(i)   strsplit(alphavectors.dat[i], "\\s+"))
#'   alphavectors.dat <-data.frame(matrix(unlist(alphavectors.dat), nrow=length(alphavectors.dat), byrow=T), stringsAsFactors = FALSE)
#'   alphavectors.dat <- alphavectors.dat[,-c(1,ncol(alphavectors.dat))]
#'   alphavectors.dat[,1] <- gsub("action=", "", alphavectors.dat[,1])
#'   alphavectors.dat[,2] <- gsub("obsValue=", "",alphavectors.dat[,2])
#'   alphavectors.dat[,2] <- gsub(">", "",alphavectors.dat[,2])
#'   alphavectors.dat <- data.frame(sapply(alphavectors.dat, as.numeric))
#'   colnames(alphavectors.dat)[1:2] <- c("action", "obsValue")
#'   return(alphavectors.dat)
#' }


#precision <- 0.01  #user-defined
#N <- 10 #user-define number of alphavectors

source('ILP_alphamin_fast.R')
library('Rfast') #package to get faster colMaxs

#policy.filename <- "./pomdp_solved/ShinySolution_0_20_20.policy"
#policy.filename <- "./pomdp_solved/ShinySolution_TOY.policy"
#policy.filename <- "./pomdp_solved/gouldian2.policy"
#policy.filename <- "./pomdp_solved/test.policy"

#read in beliefs
#beliefs.filename <- "./beliefs.txt"
#beliefs.filename <- "./beliefs_gouldian2_more.txt"
#beliefs.filename <- "./sampled_beliefs_gouldian4.txt"

#write a function s
s_func <- function(alpha, alpha_hat, s.arg){
  max_s.diff <- max(s.arg[alpha,]- s.arg[alpha_hat,])
  return(max_s.diff)
}

#write a function to automatically select the maximum initial upper bound (see Dujardin et al)
set_upper <- function(alpha, s.arg){
  max_s.diff <- rep(NA, times= nrow(s.arg))  #initialize
  for (i in 1:nrow(s.arg)){
    max_s.diff[i] <- max(abs(s.arg[alpha,]- s.arg[i,]))
    
  }
  epsilon_upper <- max(max_s.diff)
  return(epsilon_upper)
}

#' alpha_min_fast implements the alpha-min-fast algorithm in Ferrer-Mestres et al (2020). 
#' The algorithm takes a policy and a set of belief vectors and creates a reduced policy that is easier to interpret
#' Note that alpha-min-fast assumes positive rewards; if rewards include negative values, then you may need to normalise the rewards
#' before running sarsop to guarantee sensible policy graphs 
#' @param N the desired number of alpha vectors in the reduced policy (integer, user-defined)
#' @param precision a numeric representing the desired precision (stopping criterion for the binary search)
#' @param policy.filename the (uncompressed) policy file as an R object: created by running sarsop and then by running read.policy(policy.filename)
#' @param beliefs.filename  path to the sample beliefs file output by sarsop, by default stored in the working directory, probably called "beliefs.txt"  
#' @return A 2-element list: reducedPolicy[[1]] returns the epsilon (gap) of the reduced policy; reducedPolicy[[2]] contains a binary vector 
#' recording which alpha vectors to keep (1) or discard (0) in the reduced policy file.
alpha_min_fast <- function(policy.filename, beliefs.filename, precision, N){

  #read in subsets of alpha vectors 
  gamma_all <- read.policy(policy.filename)
  gamma_all <- cbind(1:nrow(gamma_all), gamma_all)
  colnames(gamma_all)[1] <- "id"
  #convert obsValue to factor for grouping

  gamma_x <- split(gamma_all,gamma_all$obsValue)
  gamma_vectors <- gamma_all[,-c(1:3)]
  
  #read in beliefs
  sample.beliefs <- read.table(beliefs.filename, header=FALSE, stringsAsFactors = FALSE)
  sample.beliefs <- as.matrix(sample.beliefs)
  #sample.beliefs <- sample.beliefs[1:200,] #use just the first 200 beliefs-- use a random sample later
  #sample.beliefs <- matrix(unlist(sample.beliefs),dim(sample.beliefs)[1], dim(sample.beliefs)[2] ,byrow=F)  # convert to matrix so we can do dot products
  
  #compute and store the dot product
  s.arg <- t(sample.beliefs %*% t(gamma_vectors))
  epsilon_upper <- max(s.arg) #this is a maximisation since rewards are assumed positive throughout the problem
  
  gamma_all$obsValue <- as.factor(gamma_all$obsValue)
  n.obsVars <- length(levels(gamma_all$obsValue))

  
  #select an alpha vector at random from the set
  set.seed(123)
  alpha <- sample(1:nrow(s.arg), size=1)
  #set the initial upper and lower bounds
  epsilon_upper <- set_upper(alpha, s.arg)
  epsilon_lower <- 0
  
  delta <- epsilon_upper-epsilon_lower
    
  iteration <- 0
  
  fill_C <- function(i){  #function to vectorize the 'i' loop 
    alpha <- ids.x[i]
    s.val <- sapply(1:n.alpha_x, function(y) max(s.arg[alpha,]- s.arg[ids.x[y],]))
    alpha_hat <- ids.x[1:n.alpha_x]
    C[alpha, alpha_hat] <- max(sign(epsilon- s.val),0)  #replace loop with max sign- this seems fastest way to assign without lopp
    return(C)
  }
    
  ## run the while loop that implements the binary search for alpha-min-fast
  C <- matrix(0, nrow= nrow(gamma_all), ncol= nrow(gamma_all))
  ptm <- proc.time()
    while (delta > precision){
      iteration <- iteration+1
      print(paste("iteration", as.character(iteration))) #print out the iteration number to console
      delta <- epsilon_upper-epsilon_lower
      epsilon <- (epsilon_lower+epsilon_upper)/2
      print(paste("delta=", delta))
      ptm2 <- proc.time()-ptm
      print(paste("time elapsed=", ptm2[3]))
      
      
      for (x in 1:n.obsVars){   #try making this a function and passing a vector argument 1:nobsVars?
        n.alpha_x <- nrow(gamma_x[[x]])       #number of alpha vectors for observable variable x
        ids.x <- gamma_x[[x]]$id              #id values of the alpha vectors relevant to observable variable x
        #gamma_x.matrix <- as.matrix(gamma_x[[x]][-c(1:3)]) #extract the alpha vectors
      
 
        #ptm <- proc.time()
        C <- fill_C(1:n.alpha_x)
        #ptm2 <- proc.time()-ptm
        
        # ptm <- proc.time()
        #  for (i in 1:n.alpha_x){ 
        # #   print(c("i=",i))
        #   alpha <- ids.x[i]    
        #    
        #    
        #    s.val <- sapply(1:n.alpha_x, function(y) max(s.arg[alpha,]- s.arg[ids.x[y],]))
        #    
        # #   #ptm <- proc.time()
        #    alpha_hat <- ids.x[1:n.alpha_x]
        #    C[alpha, alpha_hat] <- max(sign(epsilon- s.val),0)  #replace loop with max sign- this seems fastest way to assign without lopp
        # #   #C[alpha, alpha_hat] <- ifelse(epsilon- s.val>0, 1,0)
        # #   #ptm2 <- proc.time()-ptm
        # #    # ptm <- proc.time()
        # #    # for (j in 1:n.alpha_x){
        # #    #   alpha_hat <- ids.x[j]
        # #    # if (s.val[j]<= epsilon){
        # #    #   C[alpha, alpha_hat] <- 1
        # #    # } else {C[alpha, alpha_hat] <- 0}
        # #    # }
        # #    # ptm2 <- proc.time()-ptm
        # #   
        # #   # #get the row ID of the alpha vector we are testing
        # #   #  for (j in 1:n.alpha_x){
        # #   #    alpha_hat <- ids.x[j]                           #get the row ID of the alpha vector we are testing
        # #   #    
        # #   #    #s.val <- sapply(alpha_hat, function(x) max(s.arg[alpha,]- s.arg[alpha_hat,]))
        # #   #    s.val <- max(s.arg[alpha,]- s.arg[alpha_hat,])
        # #   #    print(s.val)
        # #   # # 
        # #   # #   #s.val <- s_func(alpha, alpha_hat, s.arg)
        # #   # #   if (s.val<= epsilon){
        # #   # #     C[alpha, alpha_hat] <- 1
        # #   # #   } else {C[alpha, alpha_hat] <- 0}
        # #   #  } #close j
        #  } #close i
        # ptm2 <- proc.time()- ptm
       print(paste("x=", x, "complete"))
       #print(paste("delta=", delta))
      } # close x
      
        return.lprec <- solve_ILP_alphamin.fast(N, gamma_all, C)  #first element is the status code (see ?solve.lpExtPtr), second element is the solution
        print("ILP solved")
        if (return.lprec[[1]]==0){
          epsilon_upper <- epsilon
          reduced.policy <- return.lprec[[2]] 
        }
        else {epsilon_lower <- epsilon}
         
  }  #close while
  ptm2 <- proc.time()-ptm
  print(paste("time elapsed=", ptm2[3]))
      
  reducedPolicy <- list(epsilon, reduced.policy)
  return(reducedPolicy)
    
} #close function
  
#run the alpha-min-fast function
#reducedPolicy <- alpha_min_fast(policy.filename, beliefs.filename, precision, N)



#' print.reduced.policy writes the reduced policy from alpha-min-fast into a policy file
#' @param N the desired number of alpha vectors in the reduced policy (integer, user-defined)
#' @param filename.out the name of the output path
#' @param policy.filename the filename of the (uncompressed) policy file created by running sarsop
#' @param reducedPolicy  output list from alpha-min-fast: reducedPolicy[[1]] returns the epsilon (gap) of the reduced policy; reducedPolicy[[2]] contains a binary vector 
#' @param maxDepthPolgraph the depth of the policy tree to be displayed (see options in sarsop polgraph command)
#' recording which alpha vectors to keep (1) or discard (0) in the reduced policy file.
print.reduced.policy <- function(policy.filename, reducedPolicy, filename.out, maxDepthPolgraph= 3){
  
  #check if filename.out exists, if not, create it
  if(!file.exists(filename.out)){
    f.created <- file.create(filename.out)
  }
  
  #read in subsets of alpha vectors 
  gamma_all <- read.policy(policy.filename)
  #convert obsValue to a factor to get the number of observable variables
  gamma_all$obsValue <- as.factor(gamma_all$obsValue)
  n.obsVars <- length(levels(gamma_all$obsValue))
  
  #reduce the policy by removing rows corresponding to zeros in the reducedPolicy solution
  pruned.policy <- gamma_all[as.logical(reducedPolicy[[2]]),]
  
  #get the name of the pomdp (or pomdpx) file from the policy file name (on line 2, after text "model=")
  modeline<- scan (policy.filename, skip = 1, nlines = 1, what= character())[4]
  pomdp.filename <- strsplit(modeline, "=")[[1]][2]
 
  #define the length of each belief vector:
  belief_size <- ncol(gamma_all)-2 #2 leading columns are action and obsValue
  
  fileConn<-file(filename.out, open= "wt") #open in writing text mode
  
  xml_header <-  '<?xml version="1.0" encoding="ISO-8859-1"?>';
  pomdp.filename.full <- paste(getwd(), substring(pomdp.filename,3, nchar(pomdp.filename)-1), sep="")
  version <-  paste('<Policy version=\"0.1\" type=\"value\" model=\"',pomdp.filename.full, '\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:noNamespaceSchemaLocation=\"policyx.xsd\">', sep="")
  alpha_header <-  paste('<AlphaVector vectorLength=\"', as.character(belief_size), '\" numObsValue=\"', as.character(n.obsVars), '\" numVectors=\"', as.character(nrow(pruned.policy)), '\">', sep="");
  
  
  writeLines(xml_header, fileConn)
  writeLines(version, fileConn)
  writeLines(alpha_header, fileConn)
  
  
  for (v in 1:nrow(pruned.policy)){
    vector <-  paste('<Vector action=\"', as.character(pruned.policy$action[v]), '\" obsValue=\"', as.character(pruned.policy$obsValue[v]), '\">', sep="")
    alpha_vector = paste(vector, paste(pruned.policy[v, 3:ncol(pruned.policy)], collapse="    "), ' </Vector>', collapse= " ")
    
    writeLines(alpha_vector, fileConn)
  }

  xml_end = '</AlphaVector> </Policy>';
  writeLines(xml_end, fileConn)
  #close the connection
  close(fileConn)
  
  
  #create a policy graph DOT file from the reduced policy
  #policyGraphFileName <- '\"./pomdp_solved/reducedPolicyGraph.dot\"'
  policyGraphFileName <- 'pomdp_solved/reducedPolicyGraph.dot'
  cmd <- paste("sarsop/src/polgraph.exe --policy-file ", filename.out, " --policy-graph ", policyGraphFileName, " --graph-max-depth ", maxDepthPolgraph, " ", pomdp.filename.full, sep="")
  
 # cmd <- paste("./sarsop/src/polgraph.exe \"", pomdp.filename, "\" --policy-file ", filename.out, " --policy-graph ", policyGraphFileName, sep="")
  system(cmd)
  
  return(policyGraphFileName) #return the filename so we can refer to it for plotting in the app
  
  #cmd <- "sarsop/src/polgraph.exe --policy-file pomdp_solved/reducedPolicy.policy --policy-graph pomdp_solved/reducedPolicyGraph.dot pomdpx_files/gouldian4Exp.pomdpx"
  
  #cmd <- paste("./sarsop/src/pomdpsol.exe \"", datfileName, "\" --precision ", precision, " --timeout 100 --output ", outfileName, sep="")
  
  #polgraph POMDPModelFileName --policy-file policyFileName --policy-graph policyGraphFileName [--fast] [--graph-max-depth maximumDepth]  [--graph-min-prob minimumProbability] [--graph-max-branch maximumNoOfBranches] 
  
}
  
#maxDepthPolgraph= 2
#filename.out <- "pomdp_solved/reducedPolicy.policy"
#pomdp.filename <- "pomdpx_files/sarsop_input_ShinyGrab.pomdpx"
#pomdp.filename <- "pomdpx_files/gouldian4Exp.pomdpx"  #read the pomdp filename automatically in the function
#print.reduced.policy(policy.filename, reducedPolicy,  filename.out)



#library(DiagrammeR)
#install.packages('DiagrammeR')
#policyGraphFileName <- 'pomdp_solved/reducedPolicyGraph.dot'
#grViz(policyGraphFileName, engine = "dot")



