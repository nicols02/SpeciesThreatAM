#sarsop_parse_functions contains the functions for the sarsop_parse script


#write.header is a function that writes the header and description of a pomdpx file to output.file. 
#'Discount' is the discount factor to apply to the problem: use 0.95 if unsure what to use
write.header <- function(output.file, description, discount){
  fileConn<-file(output.file, open= "wt") #open in write mode (overwrites old doc)
  
  writeLines("<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>", fileConn)
  
  pomdpxTag <- "<pomdpx version=\"0.1\" id=\"adaptiveMgmt\"
     xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
     xsi:noNamespaceSchemaLocation=\"pomdpx.xsd\"> \n"
  writeLines(pomdpxTag, fileConn)
  
  descriptionTag <- c("<Description>", description, "</Description>","\n")
  writeLines(descriptionTag, fileConn)
  
  writeLines(paste("<Discount>", discount, "</Discount>"), fileConn)
  
  #close the connection
  close(fileConn)
}


declare.state <- function(vnamePrev, vnameCurr, varnames, fullyObs, output.file){
  fileConn<-file(output.file, open= "at") #open in appending text mode
  
  writeLines(paste("\t <StateVar vnamePrev= \"", vnamePrev, "\" vnameCurr= \"", vnameCurr, "\"", 
                   " fullyObs= \"", fullyObs, "\">", sep= ""), fileConn)
  
  
  cat("\t \t <ValueEnum> ", file= fileConn)
  cat(varnames, file= fileConn)
  writeLines(" </ValueEnum>", fileConn)
  writeLines("\t </StateVar>", fileConn)
  #close the connection
  close(fileConn)
}

declare.observation <- function(obsvname, obsvarnames, output.file){
  fileConn<-file(output.file, open= "at") #open in appending text mode
  
  writeLines(paste("\t <ObsVar vname= \"", obsvname, "\">", sep= ""), fileConn)
  
  
  cat("\t \t <ValueEnum> ", file= fileConn)
  cat(obsvarnames, file= fileConn)
  writeLines(" </ValueEnum>", fileConn)
  writeLines("\t </ObsVar>", fileConn)
  #close the connection
  close(fileConn)
}

declare.action <- function(actionName, actionNameList, output.file){
  fileConn<-file(output.file, open= "at") #open in appending text mode
  
  writeLines(paste("\t <ActionVar vname= \"", actionName, "\">", sep= ""), fileConn)
  
  
  cat("\t \t <ValueEnum> ", file= fileConn)
  cat(actionNameList, file= fileConn)
  writeLines(" </ValueEnum>", fileConn)
  writeLines("\t </ActionVar>", fileConn)
  #close the connection
  close(fileConn)
}

declare.reward <- function(rewardName, output.file){
  fileConn<-file(output.file, open= "at") #open in appending text mode
  
  writeLines(paste("\t <RewardVar vname= \"", rewardName, "\"/>", sep= ""), fileConn)
  
  #close the connection
  close(fileConn)
}

#declare.tagOpen writes the syntax for opening a Tag category, inputs are e.g. tagName= "Variable", or "InitialStateBelief"
declare.tagOpen <- function(tagName, output.file){
  fileConn<-file(output.file, open= "at") #open in appending text mode
  writeLines(paste("\n <",tagName, ">", sep=""), fileConn)
  #close the connection
  close(fileConn)
}

#declare.tagClosed writes the syntax for closing a Tag category, inputs are e.g. tagName= "Variable", or "InitialStateBelief"
declare.tagClosed <- function(tagName, output.file){
  fileConn<-file(output.file, open= "at") #open in appending text mode
  writeLines(paste("</",tagName, "> \n", sep=""), fileConn)
  #close the connection
  close(fileConn)
}

#declare.condProbOpen writes the syntax for opening a CondProb entry
declare.condProbOpen <- function(varName, parentNames, output.file){
  fileConn<-file(output.file, open= "at") #open in appending text mode
  
  writeLines(paste("\n \t <CondProb>", sep=""), fileConn)
  writeLines(paste("\t \t <Var>", varName, "</Var>", sep= ""), fileConn)
  
  
  cat("\t \t <Parent> ", file= fileConn)
  cat(parentNames, file= fileConn)
  cat(" </Parent> \n", file=fileConn)
  
  writeLines(paste("\t \t <Parameter type= \"TBL\">", sep=""), fileConn)
  #close the connection
  close(fileConn)
}

declare.condProbClosed <- function(output.file){
  fileConn<-file(output.file, open= "at") #open in appending text mode
  
  writeLines("\t \t </Parameter>", fileConn)
  
  #declare.parameter(parameterTable, output.file)
  writeLines("\t </CondProb>", fileConn)
  
  #close the connection
  close(fileConn)
  }

#declare.probMatrix writes a transition matrix to the POMDPX format. It does the same as declare.condProb
#but doesn't include the full preamble (i.e. condprob, var, parent, parameter tags)
declare.probMatrix <- function(stateDescr, stateProb, output.file){
    fileConn<-file(output.file, open= "at") #open in appending text mode

    for (i in 1:nrow(stateProb)){
      writeLines("\t \t \t <Entry>", fileConn)
      
      #instance
      cat("\t \t \t \t <Instance>", file= fileConn)
      cat(as.character(stateDescr[i,]), file= fileConn)
      cat(" - </Instance> \n", file= fileConn)
      
      #probtable
      cat("\t \t \t \t <ProbTable>", file= fileConn)
      cat(as.character(stateProb[i,]), file= fileConn)
      cat(" </ProbTable>", file= fileConn)
      
      writeLines("\n \t \t \t </Entry>", fileConn)
    }

    #close the connection
    close(fileConn)
  }

#declares a full conditional probability table (useful when you have the whole transition matrix)
declare.condProb <- function(varName, parentNames, stateDescr, stateProb, output.file){
  
  declare.condProbOpen(varName, parentNames, output.file)
  declare.probMatrix(stateDescr, stateProb, output.file)
  declare.condProbClosed(output.file)
  
}
  
declare.rewardFunc <- function(varName, parentNames, stateDescr, stateValue, output.file){
  fileConn<-file(output.file, open= "at") #open in appending text mode
  
  writeLines(paste("\n \t <Func>", sep=""), fileConn)
  writeLines(paste("\t \t <Var>", varName, "</Var>", sep= ""), fileConn)
  
  
  cat("\t \t <Parent> ", file= fileConn)
  cat(parentNames, file= fileConn)
  cat(" </Parent> \n", file=fileConn)
  
  writeLines(paste("\t \t <Parameter type= \"TBL\">", sep=""), fileConn)
  
  for (i in 1:NROW(stateValue)){
    writeLines("\t \t \t <Entry>", fileConn)
    
    #instance
    cat("\t \t \t \t <Instance>", file= fileConn)
    cat(as.character(stateDescr[i,]), file= fileConn)
    cat(" </Instance> \n", file= fileConn)
    
    #probtable
    cat("\t \t \t \t <ValueTable>", file= fileConn)
    cat(as.character(stateValue[i,]), file= fileConn)
    cat(" </ValueTable>", file= fileConn)
    
    writeLines("\n \t \t \t </Entry>", fileConn)
  }
  
  writeLines("\t \t </Parameter>", fileConn)
  
  writeLines(paste("\n \t </Func>", sep=""), fileConn)
  #close the connection
  close(fileConn)
}

#normalizeIt is a trivial function that takes a number of states/models and creates a vector of length n.states which sums to 1
#values are rounded to n.dp decimal places and the excess value is assigned to the last state.
#we use this function to assign initial beliefs to the models
normalizeIt <- function(n.states, n.dp){
  ib.fm <- round(rep(1/n.states, n.states),n.dp)  #make a vector of length n states and round to 3dp
  ib.fm[length(ib.fm)] <-  ib.fm[length(ib.fm)] + (1- sum(ib.fm))  #make sure the values sum to 1 (sarsop won't use exact vals)
  return(ib.fm)
}