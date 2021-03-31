# #draw_polgraph is a script containing functions to draw a policy graph given a "true" underlying model
# if (library(DiagrammeRsvg,logical.return=TRUE)==FALSE) {
#   install.packages("DiagrammeRsvg")
#   library("DiagrammeRsvg")
# }
# if (library(DiagrammeR,logical.return=TRUE)==FALSE) {
#   install.packages("DiagrammeR")
#   library("DiagrammeR")
# }
# if (library(rsvg,logical.return=TRUE)==FALSE) {
#   install.packages("rsvg")
#   library("rsvg")
# }
# library("dplyr")
# #library("reshape2")
# source('generate_transition_matrices.R')
# 
# threshold <- 0.05  #threshold for plotting-- remove probabilities less than threshold from polgraph
# specMatInit <- matrix(data= c(0.02, 0.3, 0.015, 0.35, 0.05, 0.25, 0.025, 0.3, 0.15, 0.1, 0.1, 0.15), nrow=6, ncol=2, byrow=TRUE)
# threatMat1 <- matrix(data= c(0.875, 0.283333), nrow=1, ncol=2) #values obtained from experts (AM_elicitation_combined.xlsx)
# threatMat2 <- matrix(data= c(0.433333333,	0.666666667,	0.675, 0.866666667),
#                      nrow=1, ncol=4) #values obtained from experts (AM_elicitation_combined.xlsx)
# recoverProb <- 0  #set a recovery probability-- the prob of moving from extinct to lowSp (default 0)
# Transition.matrices <- get.transition(specMatInit, threatMat1, threatMat2, recoverProb=0)
# 
# 
# polfileName <- paste("./pomdp_solved/reducedPolicy.policy", sep="")
# policy <- read.policy(polfileName)
# dot.file <- "./pomdp_solved/reducedPolgraph_mini.dot"
# 
# true.model <- c("F1", "S3")
# 
# 
# 
# 
# 
# #### preamble formatting stuff:
# FoxMod <- true.model[1]
# SpMod <- true.model[2]
# #extract the indices of the fox and species models
# FoxMod.ind <- as.numeric(substr(FoxMod, 2,2))
# SpMod.ind <- as.numeric(substr(SpMod, 2,2))
# 
# foxState.lookup2 <- c("LowF","HighF")
# #foxState.lookup <- c("HighF", "LowF")
# specState.lookup <- c("LocExtSp", "LowSp","HighSp")
# n.foxStates <- length(foxState.lookup2)
# n.speciesStates <- length(specState.lookup)
# nStates.unfactored <- (n.foxStates*n.speciesStates)
# states.enum <- data.frame(matrix(nrow= nStates.unfactored, ncol=2))
# 
# colnames(states.enum) <- c("fox_0", "species_0")
# states.enum[,2] <-  rep(specState.lookup, times=n.foxStates)
# states.enum[,1]  <-  as.vector(sapply(1:n.foxStates, function(i) rep(foxState.lookup2[i], times= n.speciesStates)))
# actions.list <- c("do_nothing", "a1", "a2", "a3")
# 
# ###get the stationary belief state
# initialState <- c("HighF", "LowSp", true.model[1], true.model[2])
# 
# n.Foxmodels <- 5
# n.Speciesmodels <- 3
# 
# initThreatBel <- matrix(data=rep(1/n.Foxmodels, times= n.Foxmodels), nrow=1, ncol= n.Foxmodels) #assume initial belief is uniform for prefill, allow reactive later
# initSpeciesBel <-  matrix(data=(1/n.Speciesmodels), nrow=1, ncol= n.Speciesmodels)
# colnames(initThreatBel) <- foxModel.names
# colnames(initSpeciesBel) <- speciesModel.names
# 
# CostRatio= matrix(c(0,1,1.2,2.2), nrow=1) #cost of actions-- maybe make this reactive?
# colnames(CostRatio) <- actions.list
# rownames(CostRatio) <- "Action cost"
# foxModel.names <- paste("F",1:n.Foxmodels, sep="")
# speciesModel.names <- paste("S",1:n.Speciesmodels, sep="")
# 
# #enumerate the (presumed--not sure how sarsop orders models) model ordering
# spM.enum <- rep(speciesModel.names, times= (n.Foxmodels))
# fM.enum <- as.vector(sapply(1:n.Foxmodels, function(i) rep(foxModel.names[i], times= (n.Speciesmodels))))
# models.enum <- sapply(1:length(fM.enum), function(i) paste(fM.enum[i], spM.enum[i], sep="_"))
# 
# #  #generate a list of possible output states
# foxStates.out <-  as.vector(sapply(1:n.foxStates, function(i) rep(foxState.lookup2[i], times= (n.speciesStates))))
# speciesStates.out <-  rep(specState.lookup, times= n.foxStates)
# outState.names <- sapply(1:(n.foxStates*n.speciesStates), function(i) paste(foxStates.out[i],"_",speciesStates.out[i], sep=""))
# 
# 
# beliefSpecies <- initSpeciesBel
# beliefThreat <- initThreatBel
# longBel <- getLongFormatBelief(beliefThreat, beliefSpecies) #convert to long format belief
# 
# set.seed(1234)
# maxT <- 10
# MOMDP.out <- simulate.MOMDP(nSims=30, maxT, initialState, longBel, policy, Transition.matrices, benefitRatio=c(0,20,20))
# belief.stationary <- MOMDP.out$meanBeliefs[maxT+1,] #this assumes that belief has reached stationary state after nSims (check on plot in app).

draw.polgraph.conditional <- function(belief.stationary, true.model, policy, Transition.matrices, threshold= 0.05){
 
  dot.file <- "./pomdp_solved/reducedPolgraph_mini.dot"   #filename for writing graphviz dot file to
  
  #enumerate all the states
  foxState.lookup2 <- c("LowF","HighF")
  specState.lookup <- c("LocExtSp", "LowSp","HighSp")
  n.foxStates <- length(foxState.lookup2)
  n.speciesStates <- length(specState.lookup)
  nStates.unfactored <- (n.foxStates*n.speciesStates)
  states.enum <- data.frame(matrix(nrow= nStates.unfactored, ncol=2))
  colnames(states.enum) <- c("fox_0", "species_0")
  states.enum[,2] <-  rep(specState.lookup, times=n.foxStates)
  states.enum[,1]  <-  as.vector(sapply(1:n.foxStates, function(i) rep(foxState.lookup2[i], times= n.speciesStates)))
  
  #enumerate the (presumed--not sure how sarsop orders models) model ordering
  spM.enum <- rep(speciesModel.names, times= (n.Foxmodels))
  fM.enum <- as.vector(sapply(1:n.Foxmodels, function(i) rep(foxModel.names[i], times= (n.Speciesmodels))))
  models.enum <- sapply(1:length(fM.enum), function(i) paste(fM.enum[i], spM.enum[i], sep="_"))
  
  
  #  generate a list of possible output states
  foxStates.out <-  as.vector(sapply(1:n.foxStates, function(i) rep(foxState.lookup2[i], times= (n.speciesStates))))
  speciesStates.out <-  rep(specState.lookup, times= n.foxStates)
  outState.names <- sapply(1:(n.foxStates*n.speciesStates), function(i) paste(foxStates.out[i],"_",speciesStates.out[i], sep=""))
  
  
  
  mod.Bel <- extract.modelBelief2(belief.stationary) #extract the partitioned belief for printing to graph
  #MOMDP.out.b <- simulate.MOMDP.belief(nSims=30, maxT=10, initialState, longBel, policy, Transition.matrices, benefitRatio=c(0,20,20))

  ###get the optimal action and cycle through the observed states based on policy
  
  optimal.action.inds <- policy[,1]+1
  optimal.state.inds <- policy[,2]+1 #add one to index from 1 instead of 0
  
  fileConn<-file(dot.file, open= "wt") #open in write mode (overwrites old doc)
  writeLines("digraph G", fileConn)
  writeLines("{", fileConn)
  for (s in 1:nrow(policy)){
    observed.state <- states.enum[optimal.state.inds[s],]
    opt.action <-  actions.list[optimal.action.inds[s]]  #actions_list is a global variable in the app
    pasteStr <- paste("x", s, " [label= \"X (", observed.state[1], "_",observed.state[2],")\\lA (", opt.action, ")\\l\"];", sep="")
    writeLines(pasteStr, fileConn)
  }



  for (s in 1:length(optimal.state.inds)){
    observed.state <- states.enum[optimal.state.inds[s],]
    opt.action <-  actions.list[optimal.action.inds[s]]
    
    Tx.weighted.cum <- matrix(0, nrow=1, ncol= nStates.unfactored)
    colnames(Tx.weighted.cum) <- outState.names
    for (a in 1:length(models.enum)){
      curr.model <- models.enum[a]
      initialState <- c(as.character(observed.state), substr(curr.model,1,2),substr(curr.model,4,5))   #cycle through all possible models instead of using true.model and weight by stationary.belief.
    
      Tx <- getRowprobT(initialState, opt.action, Transition.matrices)
      
      #weight Tx by the model belief
      Tx.weighted <- Tx * belief.stationary[a]
      
      Tx.weighted.cum <- Tx.weighted + Tx.weighted.cum
    }
  
    #remove the zero entries from the named vector so we can write to the dot file
    Tx2 <- Tx.weighted.cum[1,!(Tx.weighted.cum<= threshold)]
    #also get the indices of the nonzero entries so we can refer to the nodes by number in the dot file
    Tx2.inds <- which(!(Tx.weighted.cum<= threshold)) 
    
  
    
    for(ed in 1:length(Tx2)){
      dest <- which(outState.names==names(Tx2)[ed])
      #pasteStr <- paste("x", s, " -> x", Tx2.inds[ed], " [label= \" (", names(Tx2)[ed], ") ", round(Tx2[ed],3), "\\l\"];", sep="")
      pasteStr <- paste("x", s, " -> x", dest, " [label= \" (", names(Tx2)[ed], ") ", round(Tx2[ed],3), "\\l\"];", sep="")
      writeLines(pasteStr, fileConn)
    }
    
  } #end s loop

  pasteStr <-  paste(c("label=\"Simulated model ",true.model[1], "_", true.model[2],
                     "\n Probs < ", threshold, " not drawn",
                     "\n Stationary belief is:",
                     "\n Threat belief= (", round(mod.Bel[[1]],2), ")",
                     "\n Species belief= (", round(mod.Bel[[2]],2), ")",
                     "\n  \""), collapse=" ")
  
  #also print stationary belief-- but first convert to factored belief form via 
  writeLines(pasteStr, fileConn)
  writeLines("}", fileConn)
  
  #close the connection
  close(fileConn)

  return(dot.file)
}
# #draw the policy graph
# plotdf.policyGr <- grViz(dot.file, engine = "dot")
# 
# 
# #save policy graph plot as a pdf
# imgNameSVG <- paste('./pomdp_solved/polGraph_mini', true.model[1], '_', true.model[2],'.svg', sep="")
# plotdf.policyGr %>% export_svg %>% charToRaw %>% rsvg_svg(imgNameSVG)
# print(paste("Policy graph saved to", imgNameSVG, sep=""))
# 
