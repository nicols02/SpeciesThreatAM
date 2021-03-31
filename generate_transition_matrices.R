#generate transition_matrices creates transition matrices based on a set of input
#values

#This script writes factored transition matrices for the threat and species universal AM problem
#It accepts expert-derived data and formats them as factored transition matrices

#assume three actions: A1, A2 and A3=A1+A2. A3 is assumed to be effective if either A1 or A2 are effective.
#Viable threat models are (Do nothing is assumed to always be ineffective):
#F1	All actions ineffective
#F2	A1,A3 effective, A2 ineffective
#F3	A1 ineffective, A2,A3 effective
#F4	A3 effective (A1, A2 ineffective)
#F5	Any action is effective (A1-A3 effective)

#Viable species models are:
#S1	species respond negatively to any level of threat presence (high or low fox)
#S2	species respond negatively to high threat presence (no or limited impact of low fox density)
#S3	species don't respond to threat presence (no impact of either high or low fox density)



#create reward table is a function to generate a rewards table from benefit and cost values
#benefitRatio is a 3-element vector containing the benefits of being in LocExtSp, LowSp and HighSp respectively
#costRatio is a nactions-element vector containing the costs of each action. Default values are based on the lit review in
# "Cost summary.xlsx" in the 'Data/Cost estimates-- SoS Database' folder of the dropbox, e.g. = c(0,1,1.18,2.18) 
create.reward.table <- function(benefitRatio, CostRatio){
  nactions <- length(CostRatio)
  rew.table <- data.frame(matrix(nrow= nactions*3, ncol= 5 ))
  colnames(rew.table) <- c("actionsList", "species_1", "Benefit", "Cost", "Reward")
  rew.table$species_1 <- rep(c("LocExtSp", "LowSp", "HighSp"), times= nactions)
  action.names <- c( "do_nothing", sapply(1:(nactions-1), function(i) paste("a",i, sep="")))
  rew.table$actionsList <-  as.vector(sapply(1:nactions, function(i){rep(action.names[i], times=3)}))
  rew.table$Benefit <- rep(benefitRatio, times= nactions)
  rew.table$Cost <- as.vector(sapply(1:nactions, function(i){rep(CostRatio[i], times=3)}))
  rew.table$Reward <- rew.table$Benefit-rew.table$Cost + max(rew.table$Cost) #add on max cost to ensure all rewards are positive (necessary for alpha-min)
  return(rew.table)
}


#' get.transition is a function that converts the shiny inputs to an R list object
#' @param specMatInit The species matrix obtained from the Shiny App
#' @param threatMat1 The first threat matrix obtained from the Shiny App
#' @param threatMat2 The second threat matrix obtained from the Shiny App
#' @param recoverProb Allow a recovery prob-- the prob of moving from extinct to lowSp state
#' @return An R list object containing the 5 threat model transition matrices and the 3 Species transition matrices
#' @examples
#' specMatInit <- matrix(data= c(0.02, 0.3, 0.015, 0.35, 0.05, 0.25, 0.025, 0.3, 0.15, 0.1, 0.1, 0.15), nrow=6, ncol=2, byrow=TRUE)
#' threatMat1 <- matrix(data= c(0.875, 0.283333), nrow=1, ncol=2) #values obtained from experts (AM_elicitation_combined.xlsx)
#' threatMat2 <- matrix(data= c(0.433333333,	0.666666667,	0.675, 0.866666667),
#'                     nrow=1, ncol=4) #values obtained from experts (AM_elicitation_combined.xlsx)
#' recoverProb <- 0  #set a recovery probability-- the prob of moving from extinct to lowSp (default 0)
#' Transition.matrices <- get.transition(specMatInit, threatMat1, threatMat2, recoverProb=0)
get.transition <- function(specMatInit, threatMat1, threatMat2, recoverProb){
  
  #use a matrix to determine the effectiveness of actions for the 9 threat models
  #This might need to be revised later-- problem dependent
  model.effectiveness.Mat <- data.frame(matrix(c(0,0,0,0, #F1 All actions ineffective
                                                 0,1,0,1, #F2 A1, A3 effective
                                                 0,0,1,1, #F3 A2, A3 effective
                                                 0,0,0,1, #F4 A3 effective 
                                                 0,1,1,1), nrow= 5, ncol=4, byrow=TRUE)) #effectiveness of management action (col) by fox model (row)
  n.Foxmodels <- nrow(model.effectiveness.Mat)
  
  species.impacts.Mat <- data.frame(matrix(c(1,1,
                                             1,0,
                                             0,0), nrow= 3, ncol=2, byrow=TRUE)) #impact of fox state on species (col), by species model (row)
  
  n.Speciesmodels <- nrow(species.impacts.Mat)
  rownames(model.effectiveness.Mat) <- sapply(1:(n.Foxmodels), function(i) paste("F",i, sep=""))
  n.actions <- ncol(model.effectiveness.Mat)
  colnames(model.effectiveness.Mat) <- c( "do_nothing", sapply(1:(n.actions-1), function(i) paste("a",i, sep="")))
  
  
  foxStates <- c("HighF", "LowF")
  speciesStates <- c("LocExtSp", "LowSp", "HighSp")
  n.foxStates <- length(foxStates)
  n.speciesStates <- length(speciesStates)
  
  colnames(species.impacts.Mat) <- foxStates
  #make a vector of actionnames
  actionNames <- colnames(model.effectiveness.Mat) #c( "do_nothing", sapply(1:(n.actions-1), function(i) paste("a",i, sep="")))
  
  speciesImpactNames <- colnames(species.impacts.Mat)
  
  
  #############################################
  ## create the interpolated threat matrices
  #############################################
  threatMat1 <- as.data.frame(as.vector(threatMat1))
  threatMat1 <- cbind(matrix(data= c("do_nothing", "HighF",
                                     "a3", "HighF"), nrow=2, ncol=2,byrow=TRUE), threatMat1)
  colnames(threatMat1)[3] <- "Pr(High)"
  
  threatMat2 <- as.data.frame(as.vector(threatMat2))
  threatMat2 <- cbind(matrix(data= c("do_nothing", "LowF",
                                     "a1",  "LowF",
                                     "a2",  "LowF",
                                     "a3", "LowF"), nrow=4, ncol=2,byrow=TRUE), threatMat2)
  colnames(threatMat2)[3] <- "Pr(Low)"
  #create a matrix of the threat and species states and interpolate the values
  req_names_threat <- matrix(NA, nrow= n.actions * n.foxStates, ncol= 2)
  req_names_threat[,2] <- rep(foxStates,n.foxStates)
  req_names_threat[,1] <- as.vector(sapply(1:n.actions, function(i) rep(actionNames[i],times= length(foxStates))))
  
  foxMat <- as.data.frame(cbind(req_names_threat,NA,NA),stringsAsFactors =FALSE)
  colnames(foxMat) <- c("action", "threat", "High","Low")
  foxMat[c("High","Low")] <- sapply(foxMat[c("High","Low")],as.numeric)
  
  #populate with known values from threatMat1 and 2. First convert to strings for easier comparison
  threatMat1_asStr <- apply(threatMat1[,1:2], 1, function(x) paste(x, collapse=""))
  threatMat2_asStr <- apply(threatMat2[,1:2], 1, function(x) paste(x, collapse=""))
  foxMat_asStr <- apply(foxMat[,1:2], 1, function(x) paste(x, collapse=""))
  
  #find the indices of threatMat that correspond to the states elicited in threatMat1:
  for (i in 1:nrow(threatMat1)){
    threatMat1_inds <- which(foxMat_asStr %in% threatMat1_asStr[i])
    foxMat[threatMat1_inds,3] <- threatMat1[i,3]
    foxMat[threatMat1_inds,4] <- 1-threatMat1[i,3]
  }
  
  #find the indices of threatMat that correspond to the states elicited in threatMat2:
  for (i in 1:nrow(threatMat2)){
    threatMat2_inds <- which(foxMat_asStr %in% threatMat2_asStr[i])
    foxMat[threatMat2_inds,4] <- threatMat2[i,3]
    foxMat[threatMat2_inds,3] <- 1-threatMat2[i,3]
  }
  
  #interpolate the missing values (Cain 2001)
  #need to get the modifying factors for converting fox state from L->H:
  MF_LH<- (foxMat[7,3]-foxMat[1,3])/(foxMat[8,3]-foxMat[1,3])

  #interpolate 
  for (i in c(3,5)){  #indices are the H states, inferred from the L states
    foxMat[i,3] <- (foxMat[i+1,3]-foxMat[1,3])*MF_LH + foxMat[1,3]
    foxMat[i,4] <- 1- foxMat[i,3]
  }
  
  #create matrices of effective management (foxMat) and ineffective management (do nothing action for all states)
  dat.effective <- foxMat
  dat.ineffective <- foxMat
  dat.ineffective[,3] <- rep(foxMat[1:2,3], times=n.actions) 
  dat.ineffective[,4] <- rep(foxMat[1:2,4], times=n.actions) 
  
  
  # ## PSEUDOCODE
  # ## for model.effectiveness.Mat[model,a] (loop over model and action a)
  # #[make the inner loop a function with inputs {model,a}]
  # #    if model.effectiveness.Mat[model,a]==0 then (action ineffective):
  # ##      find relevant action range and
  # ##      read_excel dat from range in top matrix of forR_Threat
  # ##      declare.probMatrix with relevant data
  # ##   if model.effectiveness.Mat[model,a]==1 then (action effective):
  # ##      find relevant action range and
  # ##      read_excel dat from range in bottom matrix of forR_Threat
  # ##      declare.probMatrix with relevant data
  # #   end for 
  # # end for
  # ##
  
  #create a list of the transition matrices for each fox model F1-F5
  FoxModels <- vector(mode='list', length=n.Foxmodels)
  
  for (model in 1:n.Foxmodels){  #model loop goes from 2, since F1 is always ineffective
    count <- 1
    FoxModels.actions <- as.data.frame(matrix(NA, nrow=n.foxStates*n.actions, ncol= n.foxStates*2+1 ))
    for (a in 1:n.actions){     #action loop goes from 2, since do nothing is always ineffective. actionNames has do nothing as element 1 of the list
      #if action ineffective
      if (model.effectiveness.Mat[model,a]==0){
        range.vals <- which(dat.ineffective$action %in% actionNames[a])
        dat <- dat.ineffective[range.vals,]
        
        
      } else if (model.effectiveness.Mat[model,a]==1){
        #if action effective
        range.vals <- which(dat.effective$action %in% actionNames[a])
        dat <- dat.effective[range.vals,]
        
      }
      FoxModels.actions[count:(count+nrow(dat)-1),2:ncol(FoxModels.actions)] <- dat
      FoxModels.actions[count:(count+nrow(dat)-1),1] <- rep(paste("F", model, sep=""))
      count <- count+ nrow(dat)
    }
    FoxModels[[model]] <- FoxModels.actions
    
  }
  
  #######################################################
  #Create Species transition matrices
  #######################################################
  
  #we need to build a block matrix for the case when foxes have impacts and the case when they have no impact
  #create a matrix of all possible species states-- this is a template we'll fill with data from our elicitation sheets
  req_names_species <- matrix(NA, nrow= n.foxStates * length(speciesStates), ncol= 2)
  req_names_species[,2] <- rep(speciesStates,length(foxStates))
  req_names_species[,1] <- as.vector(sapply(1:n.foxStates, function(i) rep(foxStates[i],times= length(speciesStates))))
  
  SpeciesMat1 <- as.data.frame(as.vector(specMatInit))
  SpeciesMat1 <- as.data.frame(cbind(matrix(data= c("Not_present", "LowSp",
                                      "Not_present", "HighSp",
                                      "LowF", "LowSp", 
                                      "LowF", "HighSp",
                                      "HighF", "LowSp", 
                                      "HighF", "HighSp"), nrow=6, ncol=2,byrow=TRUE), specMatInit),stringsAsFactors =FALSE)
  colnames(SpeciesMat1) <- c("Threat", "Species", "Pr_Extinct", "Pr_High")
  SpeciesMat1[,3] <- as.numeric(SpeciesMat1[,3])
  SpeciesMat1[,4] <- as.numeric(SpeciesMat1[,4])
  #insert the probability of low species state
  medProb <- sapply(1:nrow(SpeciesMat1), function(i) 1-sum(SpeciesMat1[i,3:4]))
  SpeciesMat1 <- cbind(SpeciesMat1[,1:3], medProb, SpeciesMat1[,4])
  colnames(SpeciesMat1) <- c("Threat", "Species", "Pr_Extinct", "Pr_Low","Pr_High")
  
  #add in rows for local extinction probabilities
  for(i in c(4,2)){
    insertrow <- c(SpeciesMat1[i+1,1],"LocExtSp",1-recoverProb,recoverProb,0)
    SpeciesMat1 <- rbind(SpeciesMat1[1:i,], insertrow, SpeciesMat1[(i+1): nrow(SpeciesMat1),])
  }
  SpeciesMat1 <- rbind(c(SpeciesMat1[2,1],"LocExtSp",1-recoverProb,recoverProb,0),SpeciesMat1)
  rownames(SpeciesMat1) <- NULL #get rid of the weird rownames after insertion
  
  #case 1: foxes have an impact on species
  dat_impact <- SpeciesMat1[SpeciesMat1$Threat!= "Not_present",]
  #case 2: foxes have no impact on species
  dat_noimpact <- dat_impact
  dat_noimpact[,3:5] <- do.call("rbind", replicate(n.foxStates, SpeciesMat1[SpeciesMat1$Threat== "Not_present",3:5], simplify = FALSE))
  
  
  ## now, use the two case matrices to build the possible species models. The approach is the same as for the threat (see pseudocode above)
  #create a list of the transition matrices for each species model
  SpeciesModels <- vector(mode='list', length=n.Speciesmodels)
  
  for (model in 1:n.Speciesmodels){  
    count <- 1
    SpeciesModels.impacts <- as.data.frame(matrix(NA, nrow=n.speciesStates, ncol= n.speciesStates+3))
    for (f in 1:length(speciesImpactNames)){     
      #if foxState has no impact
      if (species.impacts.Mat[model,f]==0){
        range.vals <- which(dat_noimpact$Threat %in% speciesImpactNames[f])
        dat <- dat_noimpact[range.vals,]
      } 
      else if (species.impacts.Mat[model,f]==1){
        #if foxState has impact
        range.vals <- which(dat_impact$Threat %in% speciesImpactNames[f])
        dat <- dat_impact[range.vals,]
      }
      dat <- cbind(rep(paste("S", model, sep="")), dat)
      colnames(dat)[1] <- "modelName"
      #FoxModels[[model]][,,a] <- stateDescr
      SpeciesModels.impacts[count:(count+nrow(dat)-1),] <- dat
      SpeciesModels.impacts[count:(count+nrow(dat)-1),1] <- paste("S", model, sep="")
      count <- count+ nrow(dat)
    }
    SpeciesModels.impacts[,4:6] <- sapply(SpeciesModels.impacts[,4:6],as.numeric)
    SpeciesModels[[model]] <- SpeciesModels.impacts
    
  }
  
  Transition.matrices <- list(FoxModels, SpeciesModels)
  names(Transition.matrices) <- c("FoxTransitionModel", "SpeciesTransitionModel")
  return(Transition.matrices)
  
}


# outputfile <- paste(outputpath, "/", speciesName, "_transition_matrix.Rda", sep="")
# save(Transition.matrices, file=outputfile)

############ Simulation Functions ###########################################################################

#' getStateIndices converts a named state vector into a named vector containing the indicies for the state
#' @param state the input format for state is c("fox_0", "species_0", foxModel_0", "speciesModel_0")
#' @example 
#' state <- c("HighF", "LowSp", "F1", "S3")
#' getStateIndices(state)
getStateIndices <- function(state){
  
  names(state) <- c("fox_0", "species_0", "foxModel_0", "speciesModel_0")
  
  foxState.lookup <- c("HighF", "LowF")
  specState.lookup <- c("LocExtSp", "LowSp","HighSp")
  
  fm.index <- as.numeric(substring(state["foxModel_0"], 2,2))
  #foxPrev.lookup <- which(foxState.lookup== state["foxPrev_0"])
  fox_0.lookup <- which(foxState.lookup== state["fox_0"])
  
  sm.index <- as.numeric(substring(state["speciesModel_0"], 2,2))
  #specPrev.lookup <- which(specState.lookup== state["speciesPrev_0"])
  species_0.lookup <- which(specState.lookup== state["species_0"])
  
  stateIndicesOut <- c(fox_0.lookup, species_0.lookup, fm.index, sm.index)
  names(stateIndicesOut) <- c("fox_0", "species_0", "foxModel_0", "speciesModel_0")
  
  return(stateIndicesOut)
}

#' getActionIndex converts a named action into a numeric index for the state
#' @param state the input format for state is an action string, e.g. "a1"
#' @example 
#' action <- "a3"
#' getActionIndex(action)
getActionIndex <- function(action){
  actions.lookup <- c("do_nothing", "a1", "a2", "a3")
  action.index <- which(actions.lookup== action)
  return(action.index)
}

#' getProbT computes the probability of transition between two states for a given species, action and transition matrix
#' @param initialState a 4- element state vector in the format c("fox_0", "species_0", "foxModel_0", "speciesModel_0") 
#' @param endState a 4- element state vector in the format c("fox_0", "species_0", "foxModel_0", "speciesModel_0") 
#' @param Transition.matrices: a pre-loaded Transition.matrices list (from get.transition function)
#' @param action a string containing the action name, e.g. "a1" or "do_nothing" etc
#' @example 
#' initialState <- c("HighF", "LowSp", "F1", "S3")
#' endState <- c("HighF", "HighSp", "F1", "S3")
#' action <- "a3"
#' Transition.matrices <- get.transition(specMatInit, threatMat1, threatMat2, recoverProb) #(see get.transition documentation)
#' getprobT(initialState, endState, action, Transition.matrices)
#' output is probT, a numeric representing the probability of transition between the states
getprobT <- function(initialState, endState, action, Transition.matrices){
  #script
  names(initialState) <- c("fox_0", "species_0", "foxModel_0", "speciesModel_0")
  names(endState) <- c("fox_0", "species_0", "foxModel_0", "speciesModel_0")
  
  
  #find the probability of fox transition Pr(F_t+1|F_t, FPrev_t, A_t, FoxModel_t)
  is.ind <- getStateIndices(initialState)
  es.ind <- getStateIndices(endState)
  act.ind <- getActionIndex(action)
  
  nFoxState <- 2
  nSpState <- 3
  foxTrans.rownum <- (act.ind-1)*nFoxState +  is.ind["fox_0"]
  prob.FoxT <- Transition.matrices[[1]][[is.ind["foxModel_0"]]][foxTrans.rownum, 3+es.ind[1]]
  
  #find probability of species transition Pr(Sp_t+1|Sp_t, F_t, SpPrev_t, SpModel_t)
  speciesTrans.rownum <- (is.ind["fox_0"]-1)*nSpState + is.ind["species_0"]
  prob.SpeciesT <- Transition.matrices[[2]][[is.ind["speciesModel_0"]]][speciesTrans.rownum, 3+es.ind[2]]
  
  #find probability of models changing and Prev State update (identity matrices)
  if ((es.ind["foxModel_0"]== is.ind["foxModel_0"]) &  #models need to be the same for fox and species
      (es.ind["speciesModel_0"]== is.ind["speciesModel_0"]) ){ 
    probT <- prob.FoxT * prob.SpeciesT} else {
      probT <- 0}
  
  return(probT)
}


#' getRowprobT calculates the probabilities of transition for all reachable states from an initialState
#' @param initialState a 4- element state vector in the format c("fox_0", "species_0", "foxModel_0", "speciesModel_0") 
#' @param Transition.matrices: a pre-loaded Transition.matrices list (from get.transition function)
#' @param action a string containing the action name, e.g. "a1" or "do_nothing" etc
#' @example 
#' initialState <- c("HighF", "LowSp", "F1", "S3")
#' action <- "a5"
#' Transition.matrices <- get.transition(specMatInit, threatMat1, threatMat2, recoverProb) (see get.transition documentation)
#' getRowprobT(species, initialState, action, Transition_matrices)
#' output is outprobs, a vector representing the probability of transition to any state
getRowprobT <- function(initialState, action, Transition.matrices){
  
  names(initialState) <- c("fox_0", "species_0", "foxModel_0", "speciesModel_0")
  
  foxState.lookup <- c("HighF", "LowF")
  specState.lookup <- c("LocExtSp", "LowSp","HighSp")
  n.foxStates <- length(foxState.lookup)
  n.speciesStates <- length(specState.lookup)
  
  #generate a list of possible output states
  foxStates.out <-  as.vector(sapply(1:n.foxStates, function(i) rep(foxState.lookup[i], times= (n.speciesStates))))
  speciesStates.out <-  rep(specState.lookup, times= n.foxStates)
  outState.names <- sapply(1:(n.foxStates*n.speciesStates), function(i) paste(foxStates.out[i],"_",speciesStates.out[i], sep=""))
  
  #initialise
  outProbs <- matrix(nrow=1, ncol= length(outState.names))
  colnames(outProbs) <- outState.names
  rownames(outProbs) <- paste(initialState, collapse="_")
  
  #cycle through reachable endstates and compute all nonzero probabilities of transition
  for (i in 1:n.foxStates){
    for(j in 1:n.speciesStates){
      endState <- initialState
      endState[1:2] <- c(foxState.lookup[i], specState.lookup[j])
      
      outProbs[(i-1)*n.speciesStates + j] <- getprobT(initialState, endState, action, Transition.matrices)
    }
  }
  return(outProbs)
}

#' getTransitionMatrix is a function that builds a full unfactored transition matrix for a particular model
#' it returns a transition matrix with a state ordering that matches Jonathan Ferrer's matlab script (so we can compare Transition matrices)
#' inputs are:
#' @param Transition.Matrices a (factored) transition matrices list object
#' @param FoxMod a string containing the fox model, e.g. "F1" -> "F5"
#' @param SpMod a string containing the species model, e.g. "S1" -> "S3"
#' @example 
#' FoxMod <- "F1"
#' SpMod <- "S2"
#' getTransitionMatrix(Transition.matrices, FoxMod, SpMod)
#' output= a 12x12xn.actions unfactored transition matrix (stored as list)
getTransitionMatrix <- function(Transition.matrices, FoxMod, SpMod){
  
  actions.list <- unique(Transition.matrices[[1]][[1]][,2]) #extract action names from transition matrices
  n.actions <- length(actions.list) #6
  #actions.list <- c( "do_nothing", sapply(1:(n.actions-1), function(i) paste("a",i, sep="")))
  
  
  #extract the indices of the fox and species models
  FoxMod.ind <- as.numeric(substr(FoxMod, 2,2))
  SpMod.ind <- as.numeric(substr(SpMod, 2,2))
  
  foxState.lookup <- c("HighF", "LowF")
  specState.lookup <- c("LocExtSp", "LowSp","HighSp")
  n.foxStates <- length(foxState.lookup)
  n.speciesStates <- length(specState.lookup)
  nStates.unfactored <- (n.foxStates*n.speciesStates)
  states.enum <- data.frame(matrix(nrow= nStates.unfactored, ncol=2))
  
  
  #this creates state indexing consistent with getProbT, i.e. (Fox, Sp, FoxPrev)
  # colnames(states.enum) <- c("fox_0", "species_0", "foxPrev_0")
  # states.enum[,3]  <- rep(foxState.lookup, times= n.speciesStates* n.foxStates)
  # states.enum[,2]  <-  rep(as.vector(sapply(1:n.speciesStates, function(i) rep(specState.lookup[i], times= (n.foxStates)))), times= n.foxStates)
  # states.enum[,1] <- as.vector(sapply(1:n.foxStates, function(i) rep(foxState.lookup[i], times= (n.speciesStates * n.foxStates))))
  
  #this creates state indexing consistent with Jonathan's Matlab script, i.e. (Fox, Sp)
  foxState.lookup2 <- c("LowF","HighF")
  colnames(states.enum) <- c("fox_0", "species_0")
  states.enum[,2] <-  rep(specState.lookup, times=n.foxStates)
  states.enum[,1]  <-  as.vector(sapply(1:n.foxStates, function(i) rep(foxState.lookup2[i], times= n.speciesStates)))
  
  #initialise a list to store the transition matrix for each action
  transMat.model <- vector(mode = "list", length = n.actions)
  names(transMat.model) <- actions.list
  
  for (a in 1:n.actions){
    transMat <- matrix(0, nrow= nStates.unfactored, ncol=nStates.unfactored)
    rownames(transMat) <- sapply(1:nrow(transMat), function(i) paste(as.character(states.enum[i,]), collapse="_"))
    for (i in 1:nStates.unfactored){
      initState_JFM_index <- states.enum[i,]
      #convert Jonathan's index to the index needed for getRowProbT
      initState_SN_index <- c(initState_JFM_index$fox_0, initState_JFM_index$species_0)
      initialState1 <- c(as.character(initState_SN_index), FoxMod, SpMod)
      newState <- getRowprobT(initialState1, actions.list[a], Transition.matrices)
      #append FoxPrev and SpPrev to newState names
      #state.suffix <- paste(initialState1[1], initialState1[2], sep="_")
      #colnames(newState) <- paste(colnames(newState), state.suffix, sep="_")
      #lookup indices of colnames(newState) in states.enum so we know which column to put the values into
      for (ns in 1:ncol(newState)){
        #convert state name to a vector
        newState.as.vect <- strsplit(colnames(newState)[ns],"_")[[1]]
        newState.as.vect.full <- newState.as.vect
        #newState.as.vect.full <- c(newState.as.vect[1], newState.as.vect[2], initialState1[1]) #append FoxPrev and SpPrev to newState names (SN ordering)
        #newState.as.vect.full <- c(newState.as.vect[1], initialState1[1], newState.as.vect[2]) #append FoxPrev and SpPrev to newState names (JFM ordering)
        colnames(newState)[ns] <- paste(newState.as.vect.full, collapse="_")
        #find this vector in states.enum
        col.index <- which(sapply(1:nrow(states.enum), function(i) all(newState.as.vect.full== states.enum[i,])))
        #put the value into the transition matrix at row i, column col.index
        transMat[i, col.index] <- newState[ns]
      }
    }
    
    transMat.model[[a]] <- transMat
  }
  return(transMat.model)
}


#getTransitionMatrix.Species is a function that builds a species-only unfactored transition matrix for a particular model
#inputs are:
#Transition.Matrices= a (factored) transition matrices list object
#output= an 6x6 unfactored species transition matrix (stored as list)
getTransitionMatrix.Species <- function(Transition.matrices){
  
  speciesModel.names <- c("S1","S2", "S3")
  
  n.Speciesmodels <- length(speciesModel.names)
  transMat.model <- vector(mode = "list", length = n.Speciesmodels)
  names(transMat.model) <- speciesModel.names
  
  for (s in 1:n.Speciesmodels){
    #extract the indices of the fox and species models
    SpMod.ind <- as.numeric(substr(SpMod, 2,2))
    
    specState.lookup <- c("LocExtSp", "LowSp","HighSp")
    transMat.factored <- Transition.matrices$SpeciesTransitionModel[[SpMod.ind]]
    
    states.enum <-  transMat.factored[,2:3]
    nStates.unfactored <- nrow(states.enum)
    
    transMat <- matrix(0, nrow= nStates.unfactored, ncol=nStates.unfactored)
    rownames(transMat) <- sapply(1:nrow(transMat), function(i) paste(as.character(states.enum[i,]), collapse="_"))
    colnames(transMat) <- rownames(transMat)
    for (i in 1:nStates.unfactored){
      currstate <- states.enum[i,]
      for (ns in 1:length(specState.lookup)){
        newstate <- cbind(currstate[1], specState.lookup[ns]) #define the next state
        #find the index of the new state
        col.index <- which(sapply(1:nrow(states.enum), function(i) all(newstate== states.enum[i,])))
        #add the transition probability into the column given by col.index
        transMat[i,col.index] <- transMat.factored[i,ns+3]
      }
    }
    
    transMat.model[[s]] <- transMat
  }
  return(transMat.model)
}


#getStationaryT is a function that finds the stationary/limiting distribution of an unfactored transition matrix
#follows methods outlined here: https://www.stat.berkeley.edu/~mgoldman/Section0220.pdf
#inputs:
#transMat <-  a nxn transition matrix
#outputs
#statDistMat <- a nx1 stationary distribution
getStationaryT <- function(P){
  #https://stephens999.github.io/fiveMinuteStats/markov_chains_discrete_stationary_dist.html
  # K<-nrow(P)
  # A_basic <- t(diag(rep(1,K))-P)
  # b_basic <- rep(0,K)
  # 
  # # Now add the constraint 
  # A_constr <- rbind(A_basic,rep(1,K))
  # b_constr <- c(b_basic,1)
  # 
  # pi_lineq <- t(solve(t(A_constr)%*%A_constr,t(A_constr)%*%b_constr))
  # pi_lineq%*%P
  ##singular because there are multiple extinct absorbing states? Try for a limiting dist instead of a stationary dist?
  
  #A brute-force hack to finding the stationary distribution is simply to take the transition matrix to a high power and then extract out any row.
  pi_bru <- (P^30)[5,]
  #check if 
  check.val <- sum(pi_bru - pi_bru%*%P) #this sum should = 0 if the limiting dist has been reached.
  return(pi_bru)
}

# #############
# #get the unfactored species transition matrix and compute its stationary distribution (check on analysis)
# speciesMat.unfactored <- getTransitionMatrix.Species(Transition.matrices)
# speciesMat.limiting <- vector(mode="list", length=n.Speciesmodels) 
# for (i in 1:n.Speciesmodels){
#   speciesMat.limiting[[i]] <- getStationaryT(speciesMat.unfactored[[i]]) 
# }
# #############


#' simulate_trajectory generates a simulated trajectory of a species under a given management action
#' inputs: 
#' @param initialState the starting state for the simulation; format is c("fox_0", "species_0", "foxModel_0", "speciesModel_0")
#' @param action string denoting the action, e.g. "do_nothing", "a1", etc
#' @param Transition.matrices a pre-loaded Transition matrices list object, as calculated in the script above.
#' @param maxT length of the trajectory (integer)
#' @param benefitRatio 3-element vector containing the benefits of being in LocExtSp, LowSp and HighSp respectively
#' outputs: a list object containing simulation outcomes under different strategies
#' @example 
#' initialState <-  c("LowF", "LowSp", "F1", "S1")
#' action <- "a3"
#' maxT <- 30
#' benefitRatio <- c(-20,0,0)
#' CostRatio= c(0,1,1.18,2.18)
#' simulate_trajectory(initialState, action, Transition.matrices, maxT, benefitRatio)
simulate_trajectory <- function(initialState, action, Transition.matrices, maxT, benefitRatio, CostRatio){
  
  #build a rewards table
  reward.table <- create.reward.table(benefitRatio, CostRatio)
  
  names(initialState) <- c("fox_0", "species_0", "foxModel_0", "speciesModel_0")
  
  #specify the possible state names
  foxState.lookup <- c("HighF", "LowF")
  specState.lookup <- c("LocExtSp", "LowSp","HighSp")
  n.foxStates <- length(foxState.lookup)
  n.speciesStates <- length(specState.lookup)
  
  foxStates.out <-  as.vector(sapply(1:n.foxStates, function(i) rep(foxState.lookup[i], times= (n.speciesStates))))
  speciesStates.out <-  rep(specState.lookup, times= n.foxStates)
  outState.names <- sapply(1:(n.foxStates*n.speciesStates), function(i) paste(foxStates.out[i],"_",speciesStates.out[i], sep=""))
  
  #initialise
  #simulated.states <- vector(mode="list", length=length(actions.list))
  
  #for (a in 1:n.actions){
  # action <- actions.list[a]
  
  # simulated.states[[a]] <- data.frame(matrix(nrow=maxT+1, ncol= (length(initialState)+1)))
  # colnames(simulated.states[[a]]) <- c(names(initialState), "reward")
  # simulated.states[[a]][1,] <- c(initialState,0)
  
  simulated.states <- data.frame(matrix(nrow=maxT+1, ncol= (length(initialState)+1)))
  colnames(simulated.states) <- c(names(initialState), "reward")
  simulated.states[1,] <- c(initialState,0)
  
  
  for (t in 1:maxT){
    prob.dist <- getRowprobT(initialState, action, Transition.matrices)
    #simulate a next state:
    nextState <- sample(outState.names, size=1, replace=TRUE, prob=prob.dist)
    
    #update initialState using nextState. Note have to split the outState.names variable on "_" to recover the fox and species state
    #note that foxModel and SpeciesModel don't change  
    initialState[1:2] <- c(strsplit(nextState, split="_")[[1]][1], strsplit(nextState, split="_")[[1]][2])
    #simulated.states[[a]][t+1,1:length(initialState)] <- initialState
    simulated.states[t+1,1:length(initialState)] <- initialState
    
    reward.index <- which((reward.table$actionsList==action) & (reward.table$species_1== initialState["species_0"]))
    #simulated.states[[a]][t+1,ncol(simulated.states[[a]])] <- reward.table$Reward[reward.index] 
    simulated.states[t+1,ncol(simulated.states)] <- reward.table$Reward[reward.index] 
    
    #}
  }
  return(simulated.states)
}


#simulate.action wraps simulate.trajectory over nSims trajectories
simulate.action <- function(nSims, maxT, initialState, action, Transition.matrices, benefitRatio, CostRatio){
  #initialise
  simulation.out <- vector(mode="list", length=nSims)
  simulation.indices <- simulation.out
  #set seed for reproducible output
  
  
  for (i in 1:nSims){
    simulation.out[[i]] <- simulate_trajectory(initialState, action, Transition.matrices, maxT, benefitRatio,CostRatio)
    simulation.out[[i]][,6] <- cumsum(simulation.out[[i]][,5] )
    
    simulation.indices[[i]] <- data.frame(matrix(nrow=maxT+1, ncol= (length(initialState)+2))) #initialise
    #simulation.indices[[i]] <- matrix(nrow=maxT+1, ncol= (length(initialState)+1)) #initialise
    #get the average values for each column by first converting to index values
    for (t in 1:(maxT+1)){
      as.inds <- getStateIndices(unlist(simulation.out[[i]][t,1:4]))
      simulation.indices[[i]][t,] <- as.numeric(c(as.inds, simulation.out[[i]][t,5],simulation.out[[i]][t,6]))
    }
  }
  
  
  #simplify to an array
  simulation.indices.asArray <-array(unlist(simulation.indices), dim = c(nrow(simulation.indices[[1]]), ncol(simulation.indices[[1]]), length(simulation.indices)))
  
  #get average values across nSims for each timestep
  meanSims <- apply(simulation.indices.asArray, c(1,2), mean)
  #get the simulation SE across nSims for each timestep
  stdErrSims <- apply(simulation.indices.asArray, c(1,2), function(x) sd(x)/sqrt(length(x)))
  lower.meanSims <- meanSims- stdErrSims #get the upper and lower error bounds by adding/subtracting the SE of the mean
  upper.meanSims <- meanSims+ stdErrSims
  
  output <- list(meanSims, stdErrSims, lower.meanSims, upper.meanSims) #group the outputs into an output object
  return(output)
}


########################################## SIMULATION SCRIPT #########################
#' prepare.plot creates a formatted data object ready for plotting in ggplot
#' @param benefitRatio is a 3-element vector containing the benefit of being in species states LE,L,H c(-20,0,0)
#' @param nSims is the number of simulations (integer)
#' @param maxT is the length of simulation (integer)
#' @param true.model is a 2-element string vector containing the names of the "true" fox and species models that will be simulated
#' @param Transition.matrices is the transition matrices (from get.transition.matrcies function above)
#' @param actions.list is a list of the names of the actions that the user wants to plot
#' @param outfileName is the name of the policy file for the solved POMDP
#' @example
#'benefitRatio= c(-20,0,0)
#'nSims <- 30
#'maxT <- 10
#'f <- 4
#'s <- 2
#'true.model <- c(foxModel.names[f], speciesModel.names[s]) #model F8S2= species respond to high fox but not low fox, only action a5 is effective
#'initialState <- c("HighF", "LowSp", true.model[1], true.model[2])
#'actions.list <- c("do_nothing", "a5")
#'Transition.matrices <- get.transition(specMatInit, threatMat1, threatMat2, recoverProb=0)
#'prepare.plot(benefitRatio, nSims, maxT, true.model, initialState)
#'output is a dataframe df_all, that is ready for plotting in ggplot
prepare.plot <- function(benefitRatio, CostRatio, nSims, maxT, true.model, initialState,Transition.matrices, actions.list, initialBelief,outfileName ){
  
  #file name of the MOMDP solution (if it exists-- will check later)
  #outfileName <- paste("./pomdp_solved/", "ShinySolution","_", paste(benefitRatio, collapse="_"),".policy", sep="")
  
  #actions.list <- c( "do_nothing", "a5")
  #Transition.matrices <- get.transition(specMatInit, threatMat1, threatMat2)
  
  #library(ggplot2)
  #library(reshape2)
  #library(svglite)
  #library(gridExtra)
  #library(readr)
  
  n.Foxmodels <- 5
  n.Speciesmodels <- 3
  foxModel.names <- paste("F",1:n.Foxmodels, sep="")
  speciesModel.names <- paste("S",1:n.Speciesmodels, sep="")
  
  varnames <- c("fox_0", "species_0", "foxModel_0", "speciesModel_0", "reward")
  

  #run just for model F8, S2, no MOMDP
  
  # for (f in 8){#1:n.Foxmodels){
  #   for (s in 2){#1:n.Speciesmodels){
  

  
  #prevState <- initialState
  #nextState <- c("HighF", "LowSp", initialState[1:2], initialState[5:6])
  
  df_all <- data.frame() #data.frame(matrix(NA, nrow= (maxT+1)*length(varnames), ncol= 2))
  #colnames(df_all) <- c("time", "series")
  
  
  for (i in 1:length(actions.list)){
    action <- actions.list[i]
    
    set.seed(1234)
    out <- simulate.action(nSims, maxT, initialState, action, Transition.matrices, benefitRatio, CostRatio)
    
    
    mean.Sims <- as.data.frame(out[[1]])[,-(3:4)]  #mean values; drop the fox and species models since these don't change anyway
    lower.Sims <- as.data.frame(out[[3]])[,-(3:4)] #also drop the foxPrev and speciesPrev values--  same as fox and species but with lag t=1
    upper.Sims <- as.data.frame(out[[4]])[,-(3:4)]
    mean.Sims <- cbind(1:nrow(mean.Sims), mean.Sims)
    lower.Sims <- cbind(1:nrow(mean.Sims), lower.Sims)
    upper.Sims <- cbind(1:nrow(mean.Sims), upper.Sims)
    colnames(mean.Sims) <- c("time", varnames[-(3:4)], "sum_reward")
    colnames(lower.Sims) <- c("time",paste(varnames[-(3:4)],"_lower", sep=""), "sum_reward")
    colnames(upper.Sims) <- c("time",paste(varnames[-(3:4)],"_upper", sep=""), "sum_reward")
    
    
    
    #simulate.action uses the getStateInd function to convert labelled states into numerics. 
    #getStateInd maps the labelled states as follows: HighF -> 1; LowF -> 2; LocExtSp -> 1; LowSp -> 2; HighSp -> 3
    #for plotting, it's easier if the fox states are mapped st HighF->2; LowF ->1
    #I'll convert these over now (the function y= -x+3 will change the states as reqd):
    mean.Sims[,2] <- -mean.Sims[,2] +3
    lower.Sims[,2] <- -lower.Sims[,2] +3
    upper.Sims[,2] <- -upper.Sims[,2] +3
    
    #require(reshape2)
    df <- melt(mean.Sims ,  id.vars = 'time', variable.name = 'series')
    df2 <- melt(lower.Sims ,  id.vars = 'time', variable.name = 'series')
    df3 <- melt(upper.Sims ,  id.vars = 'time', variable.name = 'series')
    df <- cbind(df, df2$value,df3$value)
    colnames(df)[3:5] <- c("mean", "lower", "upper")
    
    
    df_incr <- cbind(rep(action, times= nrow(df)), df) #incrementally store the outputs for each action in a dataframe for plotting outside the loop
    colnames(df_incr)[1] <- "action"
    df_all <- rbind(df_all, df_incr)
  }
  
  if (file.exists(outfileName)){  #if the MOMDP has been solved already, plot the MOMDP simulation
    action <- "Optimal MOMDP"
    
    policy <- read.policy(outfileName)

    set.seed(1234)
    out <- simulate.MOMDP(nSims, maxT, initialState, initialBelief, policy, Transition.matrices, benefitRatio)

    
    mean.Sims <- as.data.frame(out[[1]])[,-(3:4)]  #mean values; drop the fox and species models since these don't change anyway
    lower.Sims <- as.data.frame(out[[3]])[,-(3:4)] #also drop the foxPrev and speciesPrev values--  same as fox and species but with lag t=1
    upper.Sims <- as.data.frame(out[[4]])[,-(3:4)]
    mean.Sims <- cbind(1:nrow(mean.Sims), mean.Sims)
    lower.Sims <- cbind(1:nrow(mean.Sims), lower.Sims)
    upper.Sims <- cbind(1:nrow(mean.Sims), upper.Sims)
    colnames(mean.Sims) <- c("time", varnames[-(3:4)], "sum_reward")
    colnames(lower.Sims) <- c("time",paste(varnames[-(3:4)],"_lower", sep=""), "sum_reward")
    colnames(upper.Sims) <- c("time",paste(varnames[-(3:4)],"_upper", sep=""), "sum_reward")
    
    #simulate.action uses the getStateInd function to convert labelled states into numerics. 
    #getStateInd maps the labelled states as follows: HighF -> 1; LowF -> 2; LocExtSp -> 1; LowSp -> 2; HighSp -> 3
    #for plotting, it's easier if the fox states are mapped st HighF->2; LowF ->1
    #I'll convert these over now (the function y= -x+3 will change the states as reqd):
    mean.Sims[,2] <- -mean.Sims[,2] +3
    lower.Sims[,2] <- -lower.Sims[,2] +3
    upper.Sims[,2] <- -upper.Sims[,2] +3
    
    #require(reshape2)
    df <- melt(mean.Sims ,  id.vars = 'time', variable.name = 'series')
    df2 <- melt(lower.Sims ,  id.vars = 'time', variable.name = 'series')
    df3 <- melt(upper.Sims ,  id.vars = 'time', variable.name = 'series')
    df <- cbind(df, df2$value,df3$value)
    colnames(df)[3:5] <- c("mean", "lower", "upper")
    
    
    df_incr <- cbind(rep(action, times= nrow(df)), df) #incrementally store the outputs for each action in a dataframe for plotting outside the loop
    colnames(df_incr)[1] <- "action"
    df_all <- rbind(df_all, df_incr)  #bind the MOMDP simulation results to the others
  }
  
  return(df_all)
}


########### belief updating functions  ##
#read.policy is a function that reads a sarsop .policy file and formats it into an R dataframe
#' @param filename is a pointer to the file name where the .policy file is stored
#' @example
#' outfileName <- paste("./pomdp_solved/", "potoroo","_", paste(benefitRatio, collapse="_"),".policy", sep="")
#' policy <- read.policy(outfileName)
read.policy <- function(filename){
  alphavectors.dat <- read.table(filename, sep="\t", header=FALSE, stringsAsFactors = FALSE)
  alphavectors.dat <- alphavectors.dat$V1[-(1:3)] #drop the 3 preamble lines
  alphavectors.dat <- alphavectors.dat[-length(alphavectors.dat)] #drop the end line
  alphavectors.dat <- sapply(1:length(alphavectors.dat), function(i)  gsub(">", ">  ", alphavectors.dat[i], fixed=TRUE)) #adds a tab between the > and the first alpha vector value
  alphavectors.dat <- sapply(1:length(alphavectors.dat), function(i)   strsplit(alphavectors.dat[i], "\\s+"))
  alphavectors.dat <-data.frame(matrix(unlist(alphavectors.dat), nrow=length(alphavectors.dat), byrow=T), stringsAsFactors = FALSE)
  alphavectors.dat <- alphavectors.dat[,-c(1,ncol(alphavectors.dat))]
  alphavectors.dat[,1] <- gsub("action=", "", alphavectors.dat[,1])
  alphavectors.dat[,2] <- gsub("obsValue=", "",alphavectors.dat[,2]) 
  alphavectors.dat[,2] <- gsub(">", "",alphavectors.dat[,2])
  alphavectors.dat <- data.frame(sapply(alphavectors.dat, as.numeric))
  colnames(alphavectors.dat)[1:2] <- c("action", "obsValue")
  return(alphavectors.dat) 
}


#' getLongFormatBelief converts a marginal belief into a long format belief
#' @param initThreatBel is a nFoxModel-element vector containing the belief in each fox model. It must sum to 1
#' @param initSpeciesBel is a nSpeciesModel-element vector containing the belief in each species model. It must sum to 1
#' @example
#' n.Foxmodels <- 5
#' n.Speciesmodels <- 3
#' initThreatBel <- matrix(data=rep(1/n.Foxmodels, times= n.Foxmodels), nrow=1, ncol= n.Foxmodels) #assume initial belief is uniform for prefill, allow reactive later
#' initSpeciesBel <-  matrix(data=(1/n.Speciesmodels), nrow=1, ncol= n.Speciesmodels) 
#' longBel <- getLongFormatBelief(initThreatBel, initSpeciesBel)
getLongFormatBelief <- function(initThreatBel, initSpeciesBel){
  nSpeciesMods <- length(initSpeciesBel)
  nThreatMods <- length(initThreatBel)
  longBel <- vector(mode= "numeric", length= nThreatMods*nSpeciesMods)
  for (s in 1:nSpeciesMods){
    for (t in 1:nThreatMods){
      longBel[(s-1)*nThreatMods+t] <- initThreatBel[t]*initSpeciesBel[s]
    }
  }
  return(longBel)
}


#getOptAction is a function that extracts the optimal action for a given belief state and sarsop policy solution data frame
#inputs:
#policy- a sarsop policy file, formatted as an R dataframe with read.policy
#belief- a (n.FoxModels*n.SpeciesModels)X1 numeric belief vector
getOptAction <- function(policy, belief, n.actions){
  actionNames <- c( "do_nothing", sapply(1:(n.actions-1), function(i) paste("a",i, sep="")))
  alphavects <- policy[,-(1:2)]
  V <- as.matrix(alphavects) %*% belief
  optAction <- policy$action[which.max(V)]+1 #+1 because sarsop indexes actions from zero
  optAction<- actionNames[optAction] 
  return(optAction)
}

#write a function that does belief updates (following the formulae in Martin Peron's AAAI-17 paper
#Fast-tracking stationary MOMDPs for adaptive management problems ). The belief update is given in eqn 5.
#since our observation function is just the identity matrix, we have:
#b_(t+1,y')= N* P(x'|x,y,a)b(t,y'). For each of the possible models y', this means we update the belief
#by simply multiplying the current belief by the probability of the observed transition between states.
#inputs: 
#initialBelief: the initial belief vector (a n.modelsx1 numeric vector containing the probability of each hidden model F1-5 and S1-3)
#prevState: the state at time t (current time); this is a 2x1 vector e.g. c("HighF"  "LowSp")
#nextState: the state at time t+1 (next time): a 2x1 vector containing the observed state in the next timestep, e.g c("HighF"  "HighSp")
#outputs:
#newBelief: the updated belief vector (n.modelsx1 numeric vector)
updateBelief <- function(initialBelief, prevState, nextState, action,Transition.matrices){
  #get probability of transition between prevState and nextState
  foxModelNames <- sapply(1:(n.Foxmodels), function(i) paste("F",i, sep=""))
  specModelNames <- sapply(1:(n.Speciesmodels), function(i) paste("S",i, sep=""))
  
  beliefOut <- vector(mode="numeric", length= length(initialBelief))
  # for (s in 1: n.Speciesmodels){
  #   for (f in 1:n.Foxmodels){
  #     prevState[3:4] <- c(foxModelNames[f], specModelNames[s]) #assign the relevant model index here
  #     nextState[3:4] <- prevState[3:4]
  #     #ordering of models is F1S1, F1S2, F1S3, F2S1, F2S2 ...etc
  #     i <- f +(s-1)*n.Foxmodels  ######!!!!!!!
  #     statetransPr <- getprobT(prevState, nextState, action, Transition.matrices) #get the transiton prob assuming a particular model
  #     beliefOut[i] <- statetransPr* initialBelief[i] #multiply prob of transition (given a model) by the belief in the model
  #   }
  # }
  for (f in 1:n.Foxmodels){
    for (s in 1: n.Speciesmodels){
      prevState[3:4] <- c(foxModelNames[f], specModelNames[s]) #assign the relevant model index here
      nextState[3:4] <- prevState[3:4]
      #ordering of models is F1S1, F1S2, F1S3, F2S1, F2S2 ...etc
      i <- s +(f-1)*n.Speciesmodels  ######!!!!!!!
      statetransPr <- getprobT(prevState, nextState, action, Transition.matrices) #get the transiton prob assuming a particular model
      beliefOut[i] <- statetransPr* initialBelief[i] #multiply prob of transition (given a model) by the belief in the model
    }
  }
  
  beliefOut <- beliefOut/sum(beliefOut)
  return(beliefOut)
}


#extract.ModelBelief is a function that finds the marginal belief of a model from the joint probability of
#fox and species models (because fox and species models are independent, i.e. P(F_i,S_i)= P(F_i)P(S_i|F_i))
#Using this, we have that, e.g., P(F_i, S)= sum_j{F_i,S_j} for all j.
#inputs: 
#modName= a string containing the model name, e.g. "F1" -> "F5" or "S1"-> "S3"
#belief= a (nFoxModels * n.SpeciesModels)x1 numeric vector containing the belief in each (joint) model
#outputs:
#a (nFoxModelsx1) or (nSpeciesModelsx1) numeric vector containing the belief in the model 'modName'
extract.ModelBelief <- function(modName, belief){
  
  foxModel.names <- paste("F",1:n.Foxmodels, sep="")
  speciesModel.names <- paste("S",1:n.Speciesmodels, sep="")
  
  #enumerate the (presumed--not sure how sarsop orders models) model ordering
  spM.enum <- rep(speciesModel.names, times= (n.Foxmodels))
  fM.enum <- as.vector(sapply(1:n.Foxmodels, function(i) rep(foxModel.names[i], times= (n.Speciesmodels))))
  models.enum <- sapply(1:length(fM.enum), function(i) paste(fM.enum[i], spM.enum[i], sep="_"))
  
  modtype <- substr(modName,1,1)
  modnum <- as.numeric(substr(modName,2,2))
  
  #extract the indices that match modName
  if (modtype== "F"){
    models.match <- which(substr(models.enum,1,2)== modName) #fox models are listed in first two elements of model enum
    bel.modName <- sum(belief[models.match])  
  } else if (modtype== "S"){
    models.match <- which(substr(models.enum,4,5)== modName) #fox models are listed in first two elements of model enum
    bel.modName <- sum(belief[models.match])  
  }
  return(bel.modName)
}

#extract.modelBelief2 is a function that wraps extract.ModelBelief, returning the beliefs in all of the models for a given
#belief state
#inputs:
#belief= a (nFoxModels * n.SpeciesModels)x1 numeric vector containing the belief in each (joint) model
#outputs:
#a 2-element list containing:
# 1: a (nFoxModelsx1) numeric vector containing the belief in each fox model
# 2: a (nSpeciesModelsx1) numeric vector containing the belief in each species model
extract.modelBelief2 <- function(belief){
  #spM.enum <- speciesModel.names#rep(speciesModel.names, times= (n.Foxmodels))
  #fM.enum <- foxModel.names#as.vector(sapply(1:n.Foxmodels, function(i) rep(foxModel.names[i], times= (n.speciesStates))))
  
  bel.Fox <- vector(mode="numeric", length= n.Foxmodels)
  for (f in 1:n.Foxmodels){
    #bel.Fox[f] <- extract.ModelBelief(fM.enum[f], belief)
    bel.Fox[f] <- extract.ModelBelief(foxModel.names[f], belief)
  }
  bel.Species <- vector(mode="numeric", length= n.Speciesmodels)
  for (s in 1:n.Speciesmodels){
    #bel.Species[s] <- extract.ModelBelief(spM.enum[s], belief)
    bel.Species[s] <- extract.ModelBelief(speciesModel.names[s], belief)
  }
  belief.out <- list("foxBelief"= bel.Fox, "speciesBelief"=bel.Species)
  return(belief.out)
}


#simulate_MOMDPtrajectory generates a simulated trajectory of a species under the optimal management strategy (assuming uncertainty about the true model)
#inputs: 
#' @param initialState the starting state for the simulation; format is c("fox_0", "species_0", "foxModel_0", "speciesModel_0")
#' @param initialBelief  a (nFoxModels * n.SpeciesModels)x1 numeric vector containing the initial belief in each model
#' @param policy the optimal policy, as an R data frame (generated by read.policy)
#' @param Transition_matrices  a pre-loaded Transition matrices Rda object, as calculated in the script above.
#' @param Tmax length of the trajectory
#' @param benefitRatio 3-element vector containing the benefits of being in LocExtSp, LowSp and HighSp respectively
#' @example 
#' n.Foxmodels <- 5
#' n.Speciesmodels <- 3
#' initialState <- c("LowF", "HighSp", "F2", "S3")
#' initialBelief <- rep(1/(n.Foxmodels * n.Speciesmodels), times= (n.Foxmodels*n.Speciesmodels)) #assume initial belief is uniform
#' benefitRatio <- c(-20,0,0)
#' outfileName <- paste("./pomdp_solved/", "potoroo","_", paste(benefitRatio, collapse="_"),".policy", sep="")
#' policy <- read.policy(outfileName)
#' Transition.matrices <- get.transition(specMatInit, threatMat1, threatMat2, recoverProb=0) #run from get.transition example 
#' maxT <- 10
#' #outputs:
#a list object containing simulation outcomes under different strategies
simulate_MOMDPtrajectory <- function(initialState, initialBelief, policy, Transition.matrices, maxT, benefitRatio){
  
  #build a rewards table
  reward.table <- create.reward.table(benefitRatio, CostRatio)
  
  names(initialState) <- c("fox_0", "species_0", "foxModel_0", "speciesModel_0")
  
  ##get a list of actions
  actions.list <-unique(Transition.matrices$FoxTransitionModel[[1]][,2]) #extract the actions from Transition.matrices
  n.actions <- length(actions.list)

  #specify the possible state names
  foxState.lookup <- c("HighF", "LowF")
  specState.lookup <- c("LocExtSp", "LowSp","HighSp")
  n.foxStates <- length(foxState.lookup)
  n.speciesStates <- length(specState.lookup)
  
  foxStates.out <-  as.vector(sapply(1:n.foxStates, function(i) rep(foxState.lookup[i], times= (n.speciesStates))))
  speciesStates.out <-  rep(specState.lookup, times= n.foxStates)
  outState.names <- sapply(1:(n.foxStates*n.speciesStates), function(i) paste(foxStates.out[i],"_",speciesStates.out[i], sep=""))
  
  #initialise
  simulated.states <- data.frame(matrix(nrow=maxT+1, ncol= (length(initialState)+1)))
  colnames(simulated.states) <- c(names(initialState), "reward")
  simulated.states[1,] <- c(initialState,0)
  beliefMat <- data.frame(matrix(nrow=maxT+1, ncol= (n.Foxmodels*n.Speciesmodels)))
  actionMat <- data.frame(matrix(nrow=maxT+1, ncol=1))
  
  belief <- initialBelief
  beliefMat[1,] <- initialBelief
  
  for (t in 1:maxT){
    action <- getOptAction(policy, belief, n.actions)
    
    prob.dist <- getRowprobT(initialState, action, Transition.matrices)
    #simulate a next state:
    nextState <- sample(outState.names, size=1, replace=TRUE, prob=prob.dist)
    
    #update initialState using nextState. Note have to split the outState.names variable on "_" to recover the fox and species state
    #note that foxModel and SpeciesModel don't change and that speciesPrev -> species_0 (same for fox)
    nextState[1:2] <- c(strsplit(nextState, split="_")[[1]][1], strsplit(nextState, split="_")[[1]][2])
    nextState[3:4] <- initialState[3:4]
    simulated.states[t+1,1:length(initialState)] <- nextState
    names(nextState) <- names(initialState)
    
    #update belief
    belief <- updateBelief(belief, initialState, nextState, action, Transition.matrices)
    beliefMat[t+1,] <- belief  #iteratively store belief. Rows represent time
    actionMat[t+1,1] <- action
    
    #update initial state
    initialState <- nextState
    
    reward.index <- which((reward.table$actionsList==action) & (reward.table$species_1== initialState["species_0"]))
    simulated.states[t+1,ncol(simulated.states)] <- reward.table$Reward[reward.index] 
    
  }
  simulated.statesMOMDP <- list(simulated.states, beliefMat, actionMat)
  names(simulated.statesMOMDP) <- c("simulated.states", "belief", "MOMDPaction")
  return(simulated.statesMOMDP)
}

## simulate_MOMDP wraps simulate_MOMDPtrajectory for nSims and creates some summary stats for plotting
simulate.MOMDP <- function(nSims, maxT, initialState, initialBelief, policy, Transition.matrices, benefitRatio){
  ##get a list of actions
  actions.list <-unique(Transition.matrices$FoxTransitionModel[[1]][,2]) #extract the actions from Transition.matrices
  n.actions <- length(actions.list)
  
  #initialise
  simulation.out <- vector(mode="list", length=nSims)
  simulation.indices <- simulation.out
  simulated.beliefs <- vector(mode="list", length=nSims)
  simulated.actions <- matrix(nrow=nSims, ncol= maxT+1)
  as.action.inds <- matrix(nrow=nSims, ncol= maxT)
  
  for (i in 1:nSims){
    MOMDPsim<- simulate_MOMDPtrajectory(initialState, initialBelief, policy, Transition.matrices, maxT, benefitRatio)
    
    simulation.out[[i]] <- MOMDPsim$simulated.states
    simulation.out[[i]][,6] <- cumsum(simulation.out[[i]][,5] )
    
    simulated.beliefs[[i]] <- MOMDPsim$belief
    simulated.actions[i,] <- unlist(MOMDPsim$MOMDPaction)
    
    simulation.indices[[i]] <- data.frame(matrix(nrow=maxT+1, ncol= (length(initialState)+2))) #initialise
    #simulation.indices[[i]] <- matrix(nrow=maxT+1, ncol= (length(initialState)+1)) #initialise
    #get the average values for each column by first converting to index values
    for (t in 1:(maxT+1)){
      as.inds <- getStateIndices(unlist(simulation.out[[i]][t,1:4]))
      simulation.indices[[i]][t,] <- as.numeric(c(as.inds, simulation.out[[i]][t,5], simulation.out[[i]][t,6]))
    }
    for (t in 1:maxT){
      as.action.inds[i,t] <- getActionIndex(simulated.actions[i,t+1])
    }
  }
  
  
  #simplify to an array
  simulation.indices.asArray <-array(unlist(simulation.indices), dim = c(nrow(simulation.indices[[1]]), ncol(simulation.indices[[1]]), length(simulation.indices)))
  simulation.beliefs.asArray <-array(unlist(simulated.beliefs), dim = c(nrow(simulated.beliefs[[1]]), ncol(simulated.beliefs[[1]]), length(simulated.beliefs)))
  simulation.actions.asArray <-as.action.inds #array(unlist(simulated.actions), dim = c(nrow(simulated.actions[[1]]), ncol(simulated.actions[[1]]), length(simulated.actions)))
  
  #get average values across nSims for each timestep
  meanSims <- apply(simulation.indices.asArray, c(1,2), mean)
  meanBeliefs <- apply(simulation.beliefs.asArray, c(1,2), mean) #sums to 1
  meanActions <- apply(simulation.actions.asArray, 2, mean) 
  #get the simulation SE across nSims for each timestep
  stdErrSims <- apply(simulation.indices.asArray, c(1,2), function(x) sd(x)/sqrt(length(x)))
  stdErrBelSims <- apply(simulation.beliefs.asArray, c(1,2), function(x) sd(x)/sqrt(length(x)))
  lower.meanSims <- meanSims- stdErrSims #get the upper and lower error bounds by adding/subtracting the SE of the mean
  upper.meanSims <- meanSims+ stdErrSims
  
  lower.meanBeliefs <- meanBeliefs- stdErrBelSims #get the upper and lower error bounds by adding/subtracting the SE of the mean
  upper.meanBeliefs <- meanBeliefs+ stdErrBelSims
  
  #get the average frequency of selection for the different management actions in each timestep
  simulated.actions2 <- data.frame(simulated.actions)
  action.counts <- matrix(nrow= length(actions.list), ncol= maxT+1)
  rownames(action.counts) <- actions.list
  for (i in 1:(maxT+1)){ #make sure all factor levels are represented
    simulated.actions2[,i] <- factor(simulated.actions2[,i], levels = actions.list)
    action.counts[,i] <- t(as.numeric(table(simulated.actions2[,i])))
  }
  action.counts <- action.counts/nSims
  
  
  output <- list(meanSims, stdErrSims, lower.meanSims, upper.meanSims, meanBeliefs, stdErrBelSims, lower.meanBeliefs, upper.meanBeliefs, action.counts) #group the outputs into an output object
  names(output) <- c("meanSims", "stdErrSims", "lower.meanSims", "upper.meanSims", "meanBeliefs", "stdErrBelSims", "lower.meanBeliefs", "upper.meanBeliefs", "action.freq") 
  return(output)
}

#########

#simulate.MOMDP.belief is a function that simulated belief evolution over time and prepares the
# outputs for plotting with ggplot. It wraps simulate.MOMDP and stores the belief outputs.
#importantly it uses the same seed as prepare.plot set.seed= (1234), so if the seed is changed in one function,
# it should also be changed in the other
simulate.MOMDP.belief <- function(nSims, maxT, initialState, initialBelief, policy, Transition.matrices, benefitRatio){
  set.seed(1234)
    MOMDP.out <- simulate.MOMDP(nSims, maxT, initialState, initialBelief, policy, Transition.matrices, benefitRatio)  #policy is loaded when the POMDPX is solved
  
    mean.BelSims <- as.data.frame(MOMDP.out[[5]]) 
    lower.BelSims <- as.data.frame(MOMDP.out[[7]]) 
    upper.BelSims <- as.data.frame(MOMDP.out[[8]]) 
    mean.BelSims <- cbind(1:nrow(mean.BelSims), mean.BelSims)
    lower.BelSims <- cbind(1:nrow(mean.BelSims), lower.BelSims)
    upper.BelSims <- cbind(1:nrow(mean.BelSims), upper.BelSims)
    
    colnames(mean.BelSims) <- c("time", paste("model_", 1:(n.Foxmodels*n.Speciesmodels), sep=""))
    colnames(lower.BelSims) <- c("time", paste("model_", 1:(n.Foxmodels*n.Speciesmodels), "_lower", sep=""))
    colnames(upper.BelSims) <- c("time", paste("model_", 1:(n.Foxmodels*n.Speciesmodels), "_upper", sep=""))
    
    mean.BelSims.fox <- as.data.frame(matrix(nrow=nrow(mean.BelSims), ncol= n.Foxmodels))
    mean.BelSims.species <- as.data.frame(matrix(nrow=nrow(mean.BelSims), ncol= n.Speciesmodels))
    colnames(mean.BelSims.fox) <- foxModel.names
    colnames(mean.BelSims.species) <- speciesModel.names
    lower.BelSims.fox <- mean.BelSims.fox
    lower.BelSims.species <- mean.BelSims.species
    upper.BelSims.fox <- mean.BelSims.fox
    upper.BelSims.species <- mean.BelSims.species
    
    for (t in 1:(maxT+1)){
      mod.Bel <- extract.modelBelief2(mean.BelSims[t,seq(2,n.Foxmodels*n.Speciesmodels+1,1)])
      mean.BelSims.fox[t,] <- mod.Bel$foxBelief
      mean.BelSims.species[t,] <- mod.Bel$speciesBelief
      
      mod.Bel_L <- extract.modelBelief2(lower.BelSims[t,seq(2,n.Foxmodels*n.Speciesmodels+1,1)])
      lower.BelSims.fox[t,] <- mod.Bel_L$foxBelief
      lower.BelSims.species[t,] <- mod.Bel_L$speciesBelief
      
      mod.Bel_H <- extract.modelBelief2(upper.BelSims[t,seq(2,n.Foxmodels*n.Speciesmodels+1,1)])
      upper.BelSims.fox[t,] <- mod.Bel_H$foxBelief
      upper.BelSims.species[t,] <- mod.Bel_H$speciesBelief
    }
    mean.BelSims.fox <- cbind(1:nrow(mean.BelSims.fox), mean.BelSims.fox)
    mean.BelSims.species <- cbind(1:nrow(mean.BelSims.species), mean.BelSims.species)
    upper.BelSims.fox <- cbind(1:nrow(upper.BelSims.fox), upper.BelSims.fox)
    upper.BelSims.species <- cbind(1:nrow(upper.BelSims.species), upper.BelSims.species)
    lower.BelSims.fox <- cbind(1:nrow(lower.BelSims.fox), lower.BelSims.fox)
    lower.BelSims.species <- cbind(1:nrow(lower.BelSims.species), lower.BelSims.species)
    colnames(mean.BelSims.fox)[1] <- "time"
    colnames(mean.BelSims.species)[1] <- "time"
    colnames(upper.BelSims.fox)[1] <- "time"
    colnames(upper.BelSims.species)[1] <- "time"
    colnames(lower.BelSims.fox)[1] <- "time"
    colnames(lower.BelSims.species)[1] <- "time"
    
    #format for ggplot
    df.Belief <- melt(mean.BelSims, id.vars= "time", variable.name = "series")
    df.Belief2 <- melt(lower.BelSims, id.vars= "time", variable.name = "series")
    df.Belief3 <- melt(upper.BelSims, id.vars= "time", variable.name = "series")
    df.Belief <- cbind(df.Belief, df.Belief2$value,df.Belief3$value)
    colnames(df.Belief)[3:5] <- c("mean", "lower", "upper")
    
    df.Belief.fox <- melt(mean.BelSims.fox, id.vars= "time", variable.name = "series")
    df.Belief2.fox <- melt(lower.BelSims.fox, id.vars= "time", variable.name = "series")
    df.Belief3.fox <- melt(upper.BelSims.fox, id.vars= "time", variable.name = "series")
    df.Belief.fox <- cbind(df.Belief.fox, df.Belief2.fox$value,df.Belief3.fox$value)
    colnames(df.Belief.fox)[3:5] <- c("mean", "lower", "upper")
    
    df.Belief.species <- melt(mean.BelSims.species, id.vars= "time", variable.name = "series")
    df.Belief2.species <- melt(lower.BelSims.species, id.vars= "time", variable.name = "series")
    df.Belief3.species <- melt(upper.BelSims.species, id.vars= "time", variable.name = "series")
    df.Belief.species <- cbind(df.Belief.species, df.Belief2.species$value,df.Belief3.species$value)
    colnames(df.Belief.species)[3:5] <- c("mean", "lower", "upper")
    
    output <- list(df.Belief, df.Belief.fox, df.Belief.species) #group the outputs into an output object
    names(output) <- c("Belief", "ThreatBelief", "SpeciesBelief") 
    return(output)
  }



  ###################################################
  #plot simulated outputs
  
#prepare.plot(benefitRatio, nSims, maxT, true.model, initialState,Transition.matrices)
  

  # #plot simulation variables
  # modelName <- paste("model",initialState[4],initialState[5], sep="_")
  # plotdf.all <- ggplot(data= df_all, aes(x=time)) +
  #   geom_line(aes(y=lower, group=action, colour= factor(action)), linetype="dashed", size=0.8) +
  #   geom_line(aes(y=upper,group=action, colour= factor(action)), linetype="dashed", size=0.8)+
  #   geom_line(aes(y=mean,group=action, colour= factor(action)), size=1.2) +
  #   geom_blank(data= ddummy, aes(x=time, y=value)) +
  #   facet_wrap(vars(series), scales="free_y")+
  #   ggtitle(modelName) +
  #   ylab("") +
  #   theme(plot.title = element_text(hjust = 0.5))  #centres the plot title
  # plotdf.all <- plotdf.all+ labs(color='Action') 
  # print(plotdf.all)
  


#actions.list <- c( "do_nothing", "a5")
#Transition.matrices <- get.transition(specMatInit, threatMat1, threatMat2, recoverProb)
#need to write the output of the app to pomdpx then call the resulting sarsop
  

### Running sarsop as part of the R code:
# basedir <- getwd()
# setwd(basedir)
#sarsopdir <- './sarsop/src'
# datadir <- paste(basedir, "/pomdpx_files", sep="")
# outputdir <- paste(basedir, "/pomdpx_solved", sep="")
# 
# datfileName <- "./pomdpx_files/sarsop_input_potoroos_ShinyGrab.pomdpx"
# #datfile <- paste(datadir, "/", datfileName, sep="")
# 
# outfileName <- paste("./pomdp_solved/", "potoroo","_", paste(benefitRatio, collapse="_"),".policy", sep="")
# 
# #cmd <- paste("./sarsop/src/pomdpsol.exe \"", datfile, "\" --precision 3e-1 --timeout 100 --output ./pomdp_solved/potoroo_-20_0_0.policy", sep="")
# cmd <- paste("./sarsop/src/pomdpsol.exe \"", datfileName, "\" --precision 1e-2 --timeout 100 --output ", outfileName, sep="")
# 
# system(cmd)
# setwd(basedir)
