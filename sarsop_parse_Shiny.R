#This script writes an xml input file using the pomdpx format as described here:
#http://bigbird.comp.nus.edu.sg/pmwiki/farm/appl/index.php?n=Main.PomdpXDocumentation

#assume three actions: A1, A2 and A3=A1+A2. A3 is assumed to be effective if either A1 or A2 are effective.
#Viable threat models are (Do nothing is assumed to always be ineffective):
#F1	All actions ineffective
#F2	A1,A3 effective, A2 ineffective
#F3	A1 ineffective, A2,A3 effective
#F4	A3 effective (A1, A2 ineffective)
#F5	Any action is effective (A1-A3 effective)


#Viable species models are:
#S1	species respond negatively to any level of fox presence (high or low fox)
#S2	species respond negatively to high fox presence (no or limited impact of low fox density)
#S3	species don't respond to fox presence (no impact of either high or low fox density)



#benefitRatio is a 3-element vector containing the benefits of being in LocExtSp, LowSp and HighSp respectively
#costRatio is a nactions-element vector containing the costs of each action. Default values are based on the lit review in
# "Cost summary.xlsx" in the 'Data/Cost estimates-- SoS Database' folder of the dropbox 
# create.reward.table <- function(benefitRatio= c(-2000,1,10), CostRatio= c(0,0.5,1,1.18,1.68,2.18)){
#   nactions <- length(CostRatio)
#   rew.table <- data.frame(matrix(nrow= nactions*3, ncol= 5 ))
#   colnames(rew.table) <- c("actionsList", "species_1", "Benefit", "Cost", "Reward")
#   rew.table$species_1 <- rep(c("LocExtSp", "LowSp", "HighSp"), times= nactions)
#   action.names <- c( "do_nothing", sapply(1:(nactions-1), function(i) paste("a",i, sep="")))
#   rew.table$actionsList <-  as.vector(sapply(1:nactions, function(i){rep(action.names[i], times=3)}))
#   rew.table$Benefit <- rep(benefitRatio, times= nactions)
#   rew.table$Cost <- as.vector(sapply(1:nactions, function(i){rep(CostRatio[i], times=3)}))
#   rew.table$Reward <- rew.table$Benefit-rew.table$Cost
#   return(rew.table)
# }

#setwd("C:/Users/nic24k/Documents/FoxAM")
#source('generate_transition_matrices.R', local=TRUE) #maybe remove this and paste into the same script later?

#sarsop_parse is a function that generates a POMDPX file from a predefined set of inputs.
#' @param specMatInit The species matrix obtained from the Shiny App
#' @param threatMat1 The first threat matrix obtained from the Shiny App
#' @param threatMat2 The second threat matrix obtained from the Shiny App
#' @param recoverProb Allow a recovery prob-- the prob of moving from extinct to lowSp state
#' @param benefitRatio 3-vector; the benefits of being each each species state (LExt, Low, High; from app)
#' @param outputFileName the file name to output to
#' @return A pomdpx file, written to /pomdpx_files
#' @examples
#' specMatInit <- matrix(data= c(0.02, 0.8, 0.015, 0.8, 0.05, 0.25, 0.025, 0.3, 0.15, 0.02, 0.1, 0.01), nrow=6, ncol=2, byrow=TRUE)
#' threatMat1 <- matrix(data= c(0.875, 0.283333333333333), nrow=1, ncol=2) #values obtained from experts (AM_elicitation_combined.xlsx)
#' threatMat2 <- matrix(data= c(0.433333333,	0.666666667,	0.675, 0.866666667),
#'                     nrow=1, ncol=4) #values obtained from experts (AM_elicitation_combined.xlsx)
#' recoverProb <- 0
#' ExtinctCost <- 20
#' CostRatio <-  c(0,1,1.18,2.18)
#' benefitRatio= c(-20,1,10)
sarsop_parse <- function(specMatInit, threatMat1, threatMat2, recoverProb, benefitRatio, CostRatio, outputFileName){
  
  source('sarsop_parse_functions.R')  #source some functions that we need to write the pomdpx files
  
  #speciesName <- "potoroos"
  #runName <- "ShinyGrab"
  discount <- 0.9
  
  #define an output file that we will write to.
  #output.file <- paste("pomdpx_files/sarsop_input_", runName,".pomdpx", sep="")
  output.file <- outputFileName 
  
  
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
  #input the number of viable fox models (depends on whether some actions dominate others)
  FoxmodelNames <- rownames(model.effectiveness.Mat)# sapply(1:(n.Foxmodels), function(i) paste("F",i, sep=""))
  
  speciesImpactNames <- colnames(species.impacts.Mat)
  
  ## write the pomdpx file---------
  
  #write the pomdpx header
  description <- "This is an input file for the AM pomdp, generated from Shiny"

  write.header(output.file, description, discount)
  
  #write the Variable tag
  declare.tagOpen("Variable", output.file)
  declare.state(vnamePrev="fox_0", vnameCurr="fox_1", varnames=c("HighF", "LowF"), fullyObs='true', output.file) 
  #declare.state(vnamePrev="foxPrev_0", vnameCurr="foxPrev_1", varnames=c("HighF", "LowF"), fullyObs='true', output.file) 
  declare.state(vnamePrev="species_0", vnameCurr="species_1", varnames=c("LocExtSp", "LowSp", "HighSp"), fullyObs='true', output.file) 
  #declare.state(vnamePrev="speciesPrev_0", vnameCurr="speciesPrev_1", varnames=c("LocExtSp", "LowSp", "HighSp"), fullyObs='true', output.file) 
  declare.state(vnamePrev="foxModel_0", vnameCurr="foxModel_1", varnames=FoxmodelNames, fullyObs='false', output.file) 
  declare.state(vnamePrev="speciesModel_0", vnameCurr="speciesModel_1", varnames=c("S1", "S2", "S3"), fullyObs='false', output.file) 
  declare.observation(obsvname="obs_fox", obsvarnames= c("oHighF", "oLowF"), output.file)
  declare.observation(obsvname="obs_species", obsvarnames= c("oLocExtSp","oLowSp", "oHighSp"), output.file)
  declare.action(actionName= "actionsList", actionNameList = actionNames,output.file)
  declare.reward(rewardName= 'reward_sp', output.file)
  declare.tagClosed("Variable", output.file)
  
  
  #write the <InitialStateBelief> tag: 2 unobserved variables (i.e. the models)
  declare.tagOpen("InitialStateBelief", output.file)
  #observable vars
  declare.condProb(varName= 'fox_0', parentNames= 'null', stateDescr= as.matrix(""), stateProb= matrix(c(1, 0), nrow=1, ncol=2), output.file)  #assume initial state is fox_0=HIGHF
  declare.condProb(varName= 'species_0', parentNames= 'null', stateDescr= as.matrix(""), stateProb= matrix(c(0, 1, 0), nrow=1, ncol=3), output.file)  #assume initial state is species_0=LOWSp
  #declare.condProb(varName= 'foxPrev_0', parentNames= 'null', stateDescr= as.matrix(""), stateProb= matrix(c(1, 0), nrow=1, ncol=2), output.file)  #assume initial state is foxTrend_0=stable
  #declare.condProb(varName= 'speciesPrev_0', parentNames= 'null', stateDescr= as.matrix(""), stateProb= matrix(c(0, 1, 0), nrow=1, ncol=3), output.file)  #assume initial state is speciesTrend0=stable
  
  declare.condProb(varName= 'foxModel_0', parentNames= 'null', stateDescr= as.matrix(""), stateProb= matrix(normalizeIt(n.Foxmodels,3), nrow=1), output.file)
  declare.condProb(varName= 'speciesModel_0', parentNames= 'null', stateDescr= as.matrix(""), stateProb= matrix(c(0.33, 0.33, 0.34), nrow=1, ncol=3), output.file)
  declare.tagClosed("InitialStateBelief", output.file)
  
  #write the <StateTransitionFunction> tag
  declare.tagOpen("StateTransitionFunction", output.file)
  
  ########## FOX TRANSITION MODEL ##################
  
  #############################################
  ## create the interpolated threat matrices
  #############################################
  
  #write a function to change the name of a model (based on the specific data format, so beware if you change the input style!)
  change.modelName <- function(dat, changeModelNameTo){
    stateDescr <-  dat[,1:3]
    stateDescr[,1] <- rep(changeModelNameTo, times= nrow(stateDescr))
    return(stateDescr)
  }
  
  change.actionName <- function(dat, changeActionNameTo){
    stateDescr <-  dat[,1:3]
    stateDescr[,2] <- rep(changeActionNameTo, times= nrow(stateDescr))
    return(stateDescr)
  }
  
  
  
  Transition.matrices <- get.transition(specMatInit, threatMat1, threatMat2, recoverProb)
  
  foxMat <- Transition.matrices$FoxTransitionModel
  for (i in 1:length(foxMat)){
    colnames(foxMat[[i]]) <- c("foxModel_0","actionsList", "fox_0", "High","Low")
  }
  
  declare.condProbOpen(varName = "fox_1", parentNames= colnames(foxMat[[1]])[1:3], output.file)
  
  #since all models have the same transition function when the action is do nothing:
  range.vals <- which(foxMat[[1]]$actionsList %in% 'do_nothing')
  dat <- foxMat[[1]][range.vals,]
  stateDescr <- change.modelName(dat, "*")
  declare.probMatrix(stateDescr= stateDescr, stateProb= dat[, 4:5], output.file)
  
  #for model F1, all actions are ineffective, so can use sarsop shorthand syntax
  #baiting ineffective, action 1
  range.vals <- which(foxMat[[1]]$actionsList %in% 'do_nothing')
  #dat <-read_excel(path= datapath, sheet= "ForR_Threat", range= 'A6:F10' )
  stateDescr <- change.actionName(dat, "*")
  declare.probMatrix(stateDescr= stateDescr, stateProb= dat[, 4:5], output.file)
  
  for (model in 2:n.Foxmodels){  #model loop goes from 2, since F1 is always ineffective
    for (a in 2:n.actions){     #action loop goes from 2, since do nothing is always ineffective. actionNames has do nothing as element 1 of the list
      range.vals <- which(foxMat[[model]]$actionsList %in% actionNames[a])
      dat <- foxMat[[model]][range.vals,]
      
      stateDescr <- dat[,1:3]
      #stateDescr <- change.modelName(dat, paste("F",model, sep=""))  #don't really need to do the paste here-- could replace this
      declare.probMatrix(stateDescr= stateDescr, stateProb= dat[, 4:5], output.file)
    }
  }
  
  #close the <CondProb> for fox_1
  declare.condProbClosed(output.file)
  
  ########## SPECIES TRANSITION MODEL ##################
  
  SpeciesMat <- Transition.matrices$SpeciesTransitionModel
  for (i in 1:length(SpeciesMat)){
    colnames(SpeciesMat[[i]]) <- c("speciesModel_0", "fox_0", "species_0", "LocExt","Low","High")
  }

  #open the transition matrix for variable species_1
  declare.condProbOpen(varName = "species_1", parentNames= colnames(SpeciesMat[[1]])[1:3], output.file)
  

  #####################
  #declare the special case when the species is locally extinct (allow a small recovery probability when fox state is low)
  declare.probMatrix(stateDescr= matrix(c("*", "LowF", "LocExtSp"), nrow=1, ncol=3), 
                     stateProb= matrix(c(1-recoverProb, recoverProb, 0), nrow=1, ncol=3), output.file)
  
  declare.probMatrix(stateDescr= matrix(c("*", "HighF", "LocExtSp"), nrow=1, ncol=3), 
                     stateProb= matrix(c(1, 0, 0), nrow=1, ncol=3), output.file)
  
 #loop over remaining state-model pairs
  
  for (model in 1:n.Speciesmodels){  #model loop goes from 1
    #for (a in 2:n.actions){     #action loop goes from 2, since do nothing is always ineffective. actionNames has do nothing as element 1 of the list
      range.vals <- which(SpeciesMat[[model]]$species_0 != "LocExtSp")
      dat <- SpeciesMat[[model]][range.vals,]
      
     stateDescr <- dat[,1:3]
     declare.probMatrix(stateDescr= stateDescr, stateProb= dat[, 4:6], output.file)
    }
  
  
  
 
  #close the <CondProb> for species_1
  declare.condProbClosed(output.file)
  
  
  
  ########## FOX/SPECIES_MODEL TRANSITION MODELS ##################
  #these are partially observable but stationary-- so the transition matrices are just identity matrices
  declare.condProb(varName= "foxModel_1", parentNames= "foxModel_0", stateDescr= matrix("-"),
                   stateProb= matrix("identity"), output.file)
  
  declare.condProb(varName= "speciesModel_1", parentNames= "speciesModel_0", stateDescr= matrix("-"),
                   stateProb= matrix("identity"), output.file)
  
  #close the <StateTransitionFunction> tag
  declare.tagClosed("StateTransitionFunction", output.file)
  
  #####ObsFunction Tag
  declare.tagOpen("ObsFunction", output.file)
  
  declare.condProb(varName= "obs_fox", parentNames= c("fox_1"), stateDescr= matrix("-"),
                   stateProb= matrix(c(1, 0, 0, 1), nrow=1, ncol=4), output.file)
  
  declare.condProb(varName= "obs_species", parentNames= c("species_1"), stateDescr= matrix("-"),
                   stateProb= matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), nrow=1, ncol=9), output.file)
  
  declare.tagClosed("ObsFunction", output.file)
  
  #### RewardFunction Tag

  dat.reward <-create.reward.table(benefitRatio, CostRatio)
  rew.stateDescr <- dat.reward[,1:2]
  rew.stateValue <- as.matrix(dat.reward[,5], ncol=1)
  
  declare.tagOpen("RewardFunction", output.file)
  
  #make the reward to minimise the probability of extinction (set reward= -ve if locally extinct)
  declare.rewardFunc(varName="reward_sp", parentNames=c("actionsList","species_1"), stateDescr=rew.stateDescr, stateValue=rew.stateValue, output.file)
  declare.tagClosed("RewardFunction", output.file)
  
  declare.tagClosed("pomdpx", output.file)
  
  
  #save a copy of the file into the sarsop 'pomdpx_files' directory
  #new.location <- '../../sarsop/'
  #file.copy(from= output.file, to = paste(new.location,"/", output.file, sep=""), overwrite= TRUE)
  
  
  
}
  
# #----------------------------------------
# 
# #sarsop_parse is set up to take different values of benefitRatio and generate POMDPX files for each.
# #input arguments are:
# # speciesName= the name of the target species: either "potoroos" or "bandicoots"
# # benefitRatio= a 3-element numerical vector containing the benefits of being in LocExtSp, LowSp and HighSp respectively
# # costRatio is a nactions-element numerical vector containing the costs of each action
# #runName= a string that determines what the output file will be called
# sarsop_parse <- function(speciesName="potoroos", benefitRatio, CostRatio= c(0,1,2,10,11,12), runName){
# 
#   #speciesName <- "bandicoots"  #must be either "bandicoots" or "potoroos"
#   #speciesName <- "potoroos"
#   pathname= 'C:/Users/nic24k/Dropbox/OEH AI tools for adaptive management and monitoring/R code'
#   
#   model.effectiveness.Mat <- data.frame(matrix(c(0,0,0,0,0,0,
#                                       0,1,1,0,1,1,
#                                       0,0,1,1,1,1,
#                                       0,0,1,0,1,1,
#                                       0,0,1,0,0,1,
#                                       0,0,0,1,1,1,
#                                       0,0,0,0,1,1,
#                                       0,0,0,0,0,1,
#                                       0,1,1,1,1,1), nrow= 9, ncol=6, byrow=TRUE))
#   n.Foxmodels <- nrow(model.effectiveness.Mat)
#   rownames(model.effectiveness.Mat) <- sapply(1:(n.Foxmodels), function(i) paste("F",i, sep=""))
#   n.actions <- ncol(model.effectiveness.Mat)
#   colnames(model.effectiveness.Mat) <- c( "do_nothing", sapply(1:(n.actions-1), function(i) paste("a",i, sep="")))
#   
#   #the input file is for the SoS adaptive management project
#   library(readxl)
#   
#   source('sarsop_parse_functions.R')
#   
#   #define an output file that we will write to.
#   output.file <- paste("pomdpx_files/sarsop_input_",speciesName,"_", runName,".pomdpx", sep="")
#   
#   description <- "This is an input file for the trend version of the AM pomdp"
#   discount <- 0.9
#   
#   #make a vector of actionnames
#   actionNames <- colnames(model.effectiveness.Mat) #c( "do_nothing", sapply(1:(n.actions-1), function(i) paste("a",i, sep="")))
#   
#   #input the number of viable fox models (depends on whether some actions dominate others)
#   FoxmodelNames <- rownames(model.effectiveness.Mat)# sapply(1:(n.Foxmodels), function(i) paste("F",i, sep=""))
#   
#   #we will read in a probability matrix from excel-- set the path to this file here
#   #fix pathname later- make it relative rather than absolute
#   ###pathname <- 'C:/Users/nic24k/Dropbox/OEH AI tools for adaptive management and monitoring/proposed approach/Elicitation'
#   #pathname <- 'C:/Users/nic24k/Dropbox/OEH AI tools for adaptive management and monitoring/R code'
#   
#   datapath <- paste(pathname, "/AM Elicitation-combined.xlsx",sep="")
#   #datapath <- paste(pathname, dataFileName,sep="")
#   
#   #rewardpath <-  paste(pathname, "/AM_Rewards.xlsx",sep="")
#   #rewardpath <-  paste(pathname, rewardFileName,sep="")
#   
#   
#   #write the pomdpx header
#   write.header(output.file, description, discount)
#   
#   #write the Variable tag
#   declare.tagOpen("Variable", output.file)
#   declare.state(vnamePrev="fox_0", vnameCurr="fox_1", varnames=c("HighF", "LowF"), fullyObs='true', output.file) 
#   declare.state(vnamePrev="foxPrev_0", vnameCurr="foxPrev_1", varnames=c("HighF", "LowF"), fullyObs='true', output.file) 
#   declare.state(vnamePrev="species_0", vnameCurr="species_1", varnames=c("LocExtSp", "LowSp", "HighSp"), fullyObs='true', output.file) 
#   declare.state(vnamePrev="speciesPrev_0", vnameCurr="speciesPrev_1", varnames=c("LocExtSp", "LowSp", "HighSp"), fullyObs='true', output.file) 
#   declare.state(vnamePrev="foxModel_0", vnameCurr="foxModel_1", varnames=FoxmodelNames, fullyObs='false', output.file) 
#   declare.state(vnamePrev="speciesModel_0", vnameCurr="speciesModel_1", varnames=c("S1", "S2", "S3"), fullyObs='false', output.file) 
#   declare.observation(obsvname="obs_fox", obsvarnames= c("oHighF", "oLowF"), output.file)
#   declare.observation(obsvname="obs_species", obsvarnames= c("oLocExtSp","oLowSp", "oHighSp"), output.file)
#   declare.action(actionName= "actionsList", actionNameList = actionNames,output.file)
#   declare.reward(rewardName= 'reward_sp', output.file)
#   declare.tagClosed("Variable", output.file)
#   
#   
#   #write the <InitialStateBelief> tag: 2 unobserved variables (i.e. the models)
#   declare.tagOpen("InitialStateBelief", output.file)
#   #observable vars
#   declare.condProb(varName= 'fox_0', parentNames= 'null', stateDescr= as.matrix(""), stateProb= matrix(c(1, 0), nrow=1, ncol=2), output.file)  #assume initial state is fox_0=HIGHF
#   declare.condProb(varName= 'species_0', parentNames= 'null', stateDescr= as.matrix(""), stateProb= matrix(c(0, 1, 0), nrow=1, ncol=3), output.file)  #assume initial state is species_0=LOWSp
#   declare.condProb(varName= 'foxPrev_0', parentNames= 'null', stateDescr= as.matrix(""), stateProb= matrix(c(1, 0), nrow=1, ncol=2), output.file)  #assume initial state is foxTrend_0=stable
#   declare.condProb(varName= 'speciesPrev_0', parentNames= 'null', stateDescr= as.matrix(""), stateProb= matrix(c(0, 1, 0), nrow=1, ncol=3), output.file)  #assume initial state is speciesTrend0=stable
#   
#   declare.condProb(varName= 'foxModel_0', parentNames= 'null', stateDescr= as.matrix(""), stateProb= matrix(normalizeIt(n.Foxmodels,3), nrow=1), output.file)
#   declare.condProb(varName= 'speciesModel_0', parentNames= 'null', stateDescr= as.matrix(""), stateProb= matrix(c(0.33, 0.33, 0.34), nrow=1, ncol=3), output.file)
#   declare.tagClosed("InitialStateBelief", output.file)
#   
#   #write the <StateTransitionFunction> tag
#   declare.tagOpen("StateTransitionFunction", output.file)
#   
#   ########## FOX TRANSITION MODEL ##################
#   
#   #write a function to change the name of a model (based on the specific data format, so beware if you change the input style!)
#   change.modelName <- function(dat, changeModelNameTo){
#     stateDescr <-  dat[,1:4]
#     stateDescr[,1] <- rep(changeModelNameTo, times= nrow(stateDescr))
#     return(stateDescr)
#   }
#   
#   change.actionName <- function(dat, changeActionNameTo){
#     stateDescr <-  dat[,1:4]
#     stateDescr[,2] <- rep(changeActionNameTo, times= nrow(stateDescr))
#     return(stateDescr)
#   }
#   
#   
#   
#   #get the relevant data from the input spreadsheet
#   rangestr.noteff <- paste('A2:F',2+(n.actions*4), sep="")  #number of rows in the input spreadsheet is n.actions *n.states (4 states)
#   rangestr.eff <- paste('A',5+(n.actions*4),':F',5+2*(n.actions*4), sep="") 
#   dat.ineffective <-read_excel(path= datapath, sheet= "ForR_Threat", range= rangestr.noteff )
#   dat.effective <-  read_excel(path= datapath, sheet= "ForR_Threat", range= rangestr.eff )
#   dat.effective <- dat.effective[order(dat.effective$actionsList),] #sort the effective values by action to make reading ranges easier
#   
#   #open the transition matrix for variable fox_1
#   declare.condProbOpen(varName = "fox_1", parentNames= colnames(dat.effective)[1:4], output.file)
#   
#   
#   #since all models have the same transition function when the action is do nothing:
#   range.vals <- which(dat.ineffective$actionsList %in% 'do_nothing')
#   dat <- dat.ineffective[range.vals,]
#   stateDescr <- change.modelName(dat, "*")
#   declare.probMatrix(stateDescr= stateDescr, stateProb= dat[, 5:6], output.file)
#   
#   
#   #for model F1, all actions are ineffective, so can use sarsop shorthand syntax
#   #baiting ineffective, action 1
#   range.vals <- which(dat.ineffective$actionsList %in% 'do_nothing')
#   #dat <-read_excel(path= datapath, sheet= "ForR_Threat", range= 'A6:F10' )
#   stateDescr <- change.actionName(dat, "*")
#   declare.probMatrix(stateDescr= stateDescr, stateProb= dat[, 5:6], output.file)
#   
#   
#   
#   for (model in 2:n.Foxmodels){  #model loop goes from 2, since F1 is always ineffective
#     for (a in 2:n.actions){     #action loop goes from 2, since do nothing is always ineffective. actionNames has do nothing as element 1 of the list
#       #if action ineffective
#       if (model.effectiveness.Mat[model,a]==0){
#         range.vals <- which(dat.ineffective$actionsList %in% actionNames[a])
#         dat <- dat.ineffective[range.vals,]
#         
#         stateDescr <- change.modelName(dat, paste("F",model, sep=""))
#         declare.probMatrix(stateDescr= stateDescr, stateProb= dat[, 5:6], output.file)
#         
#       } 
#       else if (model.effectiveness.Mat[model,a]==1){
#       #if action effective
#         range.vals <- which(dat.effective$actionsList %in% actionNames[a])
#         dat <- dat.effective[range.vals,]
#         
#         stateDescr <- change.modelName(dat, paste("F",model, sep=""))
#         declare.probMatrix(stateDescr= stateDescr, stateProb= dat[, 5:6], output.file)
#         
#       }
#     }
#   }
#   
#   #close the <CondProb> for fox_1
#   declare.condProbClosed(output.file)
#   
#   ## PSEUDOCODE
#   #first print the do nothing case, since that has a wild card symbol *
#   ## for model.effectiveness.Mat[model,a] (loop over model and action a (other than do nothing))
#   #[make the inner loop a function with inputs {model,a}]
#   #    if model.effectiveness.Mat[model,a]==0 then (action ineffective):
#   ##      find relevant action range and
#   ##      read_excel dat from range in top matrix of forR_Threat
#   ##      declare.probMatrix with relevant data
#   ##   if model.effectiveness.Mat[model,a]==1 then (action effective):
#   ##      find relevant action range and
#   ##      read_excel dat from range in bottom matrix of forR_Threat
#   ##      declare.probMatrix with relevant data
#   #   end for 
#   # end for
#   ##
#   
#   
#   # ### ALL MODELS, ACTION= DO NOTHING
#   # dat <-read_excel(path= datapath, sheet= "ForR_Threat", range= 'A2:F6' )
#   # #open the transition matrix for variable fox_1
#   # declare.condProbOpen(varName = "fox_1", parentNames= colnames(dat)[1:4], output.file)
#   # 
#   # #since all models have the same transition function when the action is do nothing:
#   # stateDescr <- change.modelName(dat, "*")
#   # declare.probMatrix(stateDescr= stateDescr, stateProb= dat[, 5:6], output.file)
#   # 
#   # 
#   # ### MODEL F1 (baiting ineffective for all actions)
#   # #baiting ineffective, action 1
#   # dat <-read_excel(path= datapath, sheet= "ForR_Threat", range= 'A6:F10' )
#   # stateDescr <- change.actionName(dat, "*")
#   # declare.probMatrix(stateDescr= stateDescr, stateProb= dat[, 5:6], output.file)
#   # 
#   # 
#   # ### MODEL F2 (baiting only effective for gold standard)
#   # #baiting ineffective, action 1 (equivalent to model F1, action 1)
#   # dat <-read_excel(path= datapath, sheet= "ForR_Threat", range= 'A6:F10' )
#   # stateDescr <- change.modelName(dat, "F2")
#   # declare.probMatrix(stateDescr= stateDescr, stateProb= dat[, 5:6], output.file)
#   # 
#   # #baiting effective, gold standard
#   # dat <-read_excel(path= datapath, sheet= "ForR_Threat", range= 'A25:F29' )
#   # stateDescr <- change.modelName(dat, "F2")
#   # declare.probMatrix(stateDescr= stateDescr, stateProb= dat[, 5:6], output.file)
#   # 
#   # ### MODEL F3 (both a1 and gold standard are effective)
#   # #baiting effective, action 1
#   # dat <-read_excel(path= datapath, sheet= "ForR_Threat", range= 'A21:F25' )
#   # stateDescr <- change.modelName(dat, "F3")
#   # declare.probMatrix(stateDescr= stateDescr, stateProb= dat[, 5:6], output.file)
#   # 
#   # #baiting effective, gold standard (equivalent to model F2, gold standard)
#   # dat <-read_excel(path= datapath, sheet= "ForR_Threat", range= 'A25:F29' )
#   # stateDescr <- change.modelName(dat, "F3")
#   # declare.probMatrix(stateDescr= stateDescr, stateProb= dat[, 5:6], output.file)
#   # 
#   # #close the <CondProb> for fox_1
#   # declare.condProbClosed(output.file)
#   
#   ########## SPECIES TRANSITION MODEL ##################
#   
#   #write a function to change the species state of a model (based on the specific data format, so beware if you change the input style!)
#   change.foxstatusName <- function(dat, changeModelNameTo, changeFoxstatusTo){
#     stateDescr <-  dat[,1:4]
#     stateDescr[,1] <- rep(changeModelNameTo, times= nrow(stateDescr))
#     stateDescr[,2] <- rep(changeFoxstatusTo, times= nrow(stateDescr))
#     return(stateDescr)
#   }
#   
#   ### S1: species respond negatively to any level of fox presence
#   sheetname.species <- paste("ForR_", speciesName, sep="")
#   #read in parent names and data separately so we can manipulate the locally extinct entries (these are all the same)
#   parentNames_Sp <- as.matrix(read_excel(path= datapath, sheet= sheetname.species, range= 'A3:D3', col_names=FALSE))
#   dat <-read_excel(path= datapath, sheet= sheetname.species, range= 'A12:G15',col_names=FALSE )
#   
#   #read in the data for no impact-- this is the same for any fox level (but not applicable nor the same for all models)
#   dat_noImpact <-read_excel(path= datapath, sheet= sheetname.species, range= 'A26:G29',col_names=FALSE )
#   
#   #open the transition matrix for variable species_1
#   declare.condProbOpen(varName = "species_1", parentNames= parentNames_Sp, output.file)
#   
#   # #declare the special case when the species is locally extinct (always stays extinct)
#   # declare.probMatrix(stateDescr= matrix(c("*", "*", "LocExtSp", "*"), nrow=1, ncol=4), 
#   #                    stateProb= matrix(c(1, 0, 0), nrow=1, ncol=3), output.file)
#   # 
#   # declare.probMatrix(stateDescr= matrix(c("*", "*", "*", "LocExtSp"), nrow=1, ncol=4), 
#   #                    stateProb= matrix(c(1, 0, 0), nrow=1, ncol=3), output.file)
#   
#   #####################
#   #declare the special case when the species is locally extinct (allow a small recovery probability when fox state is low)
#   declare.probMatrix(stateDescr= matrix(c("*", "LowF", "LocExtSp", "*"), nrow=1, ncol=4), 
#                      stateProb= matrix(c(1-recoverProb, recoverProb, 0), nrow=1, ncol=3), output.file)
#   
#   declare.probMatrix(stateDescr= matrix(c("*", "HighF", "LocExtSp", "*"), nrow=1, ncol=4), 
#                      stateProb= matrix(c(1, 0, 0), nrow=1, ncol=3), output.file)
#   
#   declare.probMatrix(stateDescr= matrix(c("*", "*", "*", "LocExtSp"), nrow=1, ncol=4), 
#                      stateProb= matrix(c(1, 0, 0), nrow=1, ncol=3), output.file)
#   #####################
#   
#     #write remaining transition matrix entries for S1, fox high
#     stateDescr_S <- change.modelName(dat, "S1")
#     declare.probMatrix(stateDescr= stateDescr_S, stateProb= dat[, 5:7], output.file)
#     #S1, fox low
#     dat <-read_excel(path= datapath, sheet= sheetname.species, range= 'A16:G19',col_names=FALSE )
#     stateDescr_S <- change.modelName(dat, "S1")
#     declare.probMatrix(stateDescr= stateDescr_S, stateProb= dat[, 5:7], output.file)
#   
#   
#   
#   ### S2: species respond negatively to high level of fox presence (no impact of low fox density)
#     #S2, fox high
#     dat <-read_excel(path= datapath, sheet= sheetname.species, range= 'A12:G15',col_names=FALSE )
#     stateDescr_S <- change.modelName(dat, "S2")
#     declare.probMatrix(stateDescr= stateDescr_S, stateProb= dat[, 5:7], output.file)
#     
#     #S2, fox low
#     stateDescr_S <- change.foxstatusName(dat_noImpact, changeModelNameTo="S2", changeFoxstatusTo="LowF")
#     declare.probMatrix(stateDescr= stateDescr_S, stateProb= dat_noImpact[, 5:7], output.file)
#     
#   
#   ### S3: species don't respond fox presence (no impact of fox density)
#     stateDescr_S <- change.foxstatusName(dat_noImpact, changeModelName="S3", changeFoxstatusTo="*")
#     declare.probMatrix(stateDescr= stateDescr_S, stateProb= dat_noImpact[, 5:7], output.file)
#     
#   #close the <CondProb> for species_1
#   declare.condProbClosed(output.file)
#   
#   
#   ########## FOX/SPECIES PREV TRANSITION MODELS ##################
#   declare.condProb(varName="foxPrev_1", parentNames= c("fox_0"), stateDescr=as.matrix("-"), stateProb=as.matrix(c("identity")), output.file)
#   
#   declare.condProb(varName="speciesPrev_1", parentNames= c("species_0"), stateDescr=as.matrix("-"), stateProb=as.matrix(c("identity")), output.file)
#   
#   
#   ########## FOX/SPECIES_MODEL TRANSITION MODELS ##################
#   #these are partially observable but stationary-- so the transition matrices are just identity matrices
#   declare.condProb(varName= "foxModel_1", parentNames= "foxModel_0", stateDescr= matrix("-"),
#                    stateProb= matrix("identity"), output.file)
#   
#   declare.condProb(varName= "speciesModel_1", parentNames= "speciesModel_0", stateDescr= matrix("-"),
#                    stateProb= matrix("identity"), output.file)
#   
#   #close the <StateTransitionFunction> tag
#   declare.tagClosed("StateTransitionFunction", output.file)
#   
#   #####ObsFunction Tag
#   declare.tagOpen("ObsFunction", output.file)
#   
#   declare.condProb(varName= "obs_fox", parentNames= c("fox_1"), stateDescr= matrix("-"),
#                    stateProb= matrix(c(1, 0, 0, 1), nrow=1, ncol=4), output.file)
#   
#   declare.condProb(varName= "obs_species", parentNames= c("species_1"), stateDescr= matrix("-"),
#                    stateProb= matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), nrow=1, ncol=9), output.file)
#   
#   declare.tagClosed("ObsFunction", output.file)
#   
#   #### RewardFunction Tag
#   #range.rewards <- paste('A1:E',1+3*n.actions, sep="") #range is n.actions* 3 species states (LE,L,H) 
#   #dat.reward <-read_excel(path= rewardpath, sheet= "Sheet1", range= range.rewards)
#   
#   dat.reward <-create.reward.table(benefitRatio, CostRatio)
#   rew.stateDescr <- dat.reward[,1:2]
#   rew.stateValue <- as.matrix(dat.reward[,5], ncol=1)
#   
#   declare.tagOpen("RewardFunction", output.file)
#   
#   #make the reward to minimise the probability of extinction (set reward= -1 if locally extinct)
#   declare.rewardFunc(varName="reward_sp", parentNames=c("actionsList","species_1"), stateDescr=rew.stateDescr, stateValue=rew.stateValue, output.file)
#   declare.tagClosed("RewardFunction", output.file)
#   
#   declare.tagClosed("pomdpx", output.file)
#   
#   
#   #save a copy of the file into the sarsop 'pomdpx_files' directory
#   new.location <- '../../sarsop/'
#   file.copy(from= output.file, to = paste(new.location,"/", output.file, sep=""), overwrite= TRUE)
#   
#   
# } 
# 
# 
# # #generate a set of different benefit ratios
# #  benefitRatio.matrix <- data.frame(matrix( c(-50, 0, 0,
# #                                              -20, 0, 0,
# #                                              -17,0,0,
# #                                              -15,0,0,
# #                                              -12,0,0
# #                                           ), ncol= 3, byrow=TRUE))
# # #benefitRatio.matrix <- data.frame(matrix( c(-10, 0, 0,
# # #                                            -100, 0, 0), ncol= 3, byrow=TRUE))
# # recoverProb <- 0.05
# # 
# # colnames(benefitRatio.matrix) <- c("LocExtBen", "LowSpBen", "HighSpBen")
# # rownames(benefitRatio.matrix) <-  sapply(1:nrow(benefitRatio.matrix), function(i) paste(benefitRatio.matrix[i,], collapse="_"))
# # 
# # #generate a POMDPX file for each of the benefitRatios in BenefitRatio.matrix
# # for (i in 1:nrow(benefitRatio.matrix)){
# #   sarsop_parse(speciesName="potoroos", benefitRatio= as.numeric(benefitRatio.matrix[i,]), CostRatio= c(0,0.5,1,1.18,1.68,2.18), runName= paste(rownames(benefitRatio.matrix)[i],"_rPr", recoverProb*100, sep=""))
# # }
