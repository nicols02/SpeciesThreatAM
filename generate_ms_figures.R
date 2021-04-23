#this script creates the figures for the AM manuscript

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

  library("ggplot2")
  library("reshape2")
  library("DiagrammeR")
  library("Rfast")
  library("rsvg")
  library("DiagrammeRsvg")
  library("magrittr")
  library("plyr")
  library("dplyr")
  library('svglite')

  source("generate_transition_matrices.R", local=TRUE)#local=environment() )
  source("sarsop_parse_Shiny.R", local=TRUE)#local=environment() )
  source("alpha_min_fast.R", local=TRUE)
  source("draw_polgraph.R", local=TRUE)

  
  ######### SET UP THE DATA COLLECTION MATRICES ###################
  #clean/empty matrix
  specMatInit <- matrix(data=0, nrow= 6, ncol=2)
  #prefilled for species threat (from expert elicited 'stable' trend data in sheet 'AM Elicitation-combined_optimistic.xlsx')
  specMatInit <- matrix(data= c(0.05, 0.33, 0.03, 0.84, 0.17, 0.14, 0.10, 0.67, 0.22, 0.04, 0.17, 0.45), nrow=6, ncol=2, byrow=TRUE)
  #specMatInit <- matrix(data= c(0.02, 0.8, 0.015, 0.8, 0.05, 0.25, 0.025, 0.3, 0.15, 0.02, 0.1, 0.01), nrow=6, ncol=2, byrow=TRUE)

  
  
  colnames(specMatInit) <- c("P(Extinct)", "P(High)")
  rownames(specMatInit) <- c("(Not Present, Low)",
                             "(Not Present, High)",
                             "(Low, Low)",
                             "(Low, High)",
                             "(High, Low)",
                             "(High, High)")
  
  
  #clean/empty matrix from  'AM Elicitation-combined_optimistic.xlsx' (Fox management direct estimate)
  threatMat1 <- matrix(data=0, nrow=1, ncol=2) 
  #prefilled for fox threat
  threatMat1 <- matrix(data= c(0.875, 0.283333), nrow=1, ncol=2) #values obtained from experts (AM_elicitation_combined_optimistic.xlsx)
  colnames(threatMat1) <- c("P(High|A0)", "P(High|A3)")
  rownames(threatMat1) <- c("High")
  
  #clean/empty matrix
  threatMat2 <- matrix(data=0, nrow=1, ncol=4)
  #prefilled for fox threat
  threatMat2 <- matrix(data= c(0.433333333,	0.666666667,	0.675, 0.866666667),
                       nrow=1, ncol=4) #values obtained from experts (AM_elicitation_combined_optimistic.xlsx)
  colnames(threatMat2) <- c("P(Low|A0)", "P(Low|A1)","P(Low|A2)","P(Low|A3)")
  rownames(threatMat2) <- c("Low")
  
  n.Foxmodels <- 5
  n.Speciesmodels <- 3
  foxModel.names <- paste("F",1:n.Foxmodels, sep="")
  speciesModel.names <- paste("S",1:n.Speciesmodels, sep="")
  
  n.actions <- 4
  actions.list <- c( "do_nothing", sapply(1:(n.actions-1), function(i) paste("a",i, sep="")))
  
  CostRatio= matrix(c(0,1,1.2,2.2), nrow=1) #cost of actions-- maybe make this reactive?
  colnames(CostRatio) <- actions.list
  rownames(CostRatio) <- "Action cost"
  
  #specify initial belief
  initThreatBel <- matrix(data=rep(1/n.Foxmodels, times= n.Foxmodels), nrow=1, ncol= n.Foxmodels) #assume initial belief is uniform for prefill, allow reactive later
  initSpeciesBel <-  matrix(data=(1/n.Speciesmodels), nrow=1, ncol= n.Speciesmodels) 
  colnames(initThreatBel) <- foxModel.names
  colnames(initSpeciesBel) <- speciesModel.names
  longBel <- getLongFormatBelief(initThreatBel, initSpeciesBel) #convert to long format belief
  
  benValue <- 20
  benefitRatio <- c(0,benValue,benValue)
  recoverProb <- 0
  

  Transition.matrices <- get.transition(specMatInit, threatMat1, threatMat2, recoverProb)
  
  ######## SOLVE MOMDP ###################
  
  #write the pomdpx file
  outputFileName <- paste("pomdpx_files/sarsop_input_experts.pomdpx", sep="")
  sarsop_parse(specMatInit, threatMat1, threatMat2, recoverProb, benefitRatio, CostRatio, outputFileName)
  
  outfileName <- paste("./pomdp_solved/ExpertSolution_", paste(benefitRatio, collapse="_"),".policy", sep="")
  
  #write an external command for sarsop to run and call it using system(cmd)

  precision <- 0.5  #set precision for sarsop-- this determines the number of alpha vectors and hence the time required to solve alpha-min-fast
  datfileName <- paste("./", outputFileName, sep="")
  ptm <- proc.time()
  #cmd <- paste("./sarsop/src/pomdpsol.exe \"", datfileName, "\" --precision ", precision, " --timeout 100 --output ", outfileName, sep="")  #works for cygwin
  cmd <- paste("wsl ./sarsop/src/pomdpsol ", datfileName, " --precision ", precision, " --timeout ", 100," --output ", outfileName, sep="") #works for wsl2
  
  system(cmd)
  proc.time() - ptm
  print(paste("finished solving. Writing to:", outfileName, collapse=""))
  
  policy.filename <- outfileName
  policy <- read.policy(policy.filename)
  
  ##### SIMULATE MOMDP & PLOT (FIG 1)  ###########
  true.model <- c("F2", "S2")  #i.e. assume the true model is "A1,A3 effective, A2 ineffective, potoroos respond negatively to high fox presence"
  initialState <- c("HighF", "LowSp", true.model[1], true.model[2])
  
  actions.list <- c( "do_nothing", sapply(1:(n.actions-1), function(i) paste("a",i, sep="")))
  #actions.list <- c( sapply(1:(n.actions-1), function(i) paste("a",i, sep="")))
  
  nSims <-  100
  maxT <- 15
  #simulate the actions then the MOMDP
  df_all <- prepare.plot(benefitRatio, CostRatio, nSims, maxT, true.model, initialState,Transition.matrices,actions.list, longBel, policy.filename)
  #simulate the belief and actions for the MOMDP
  df_belief <- simulate.MOMDP.belief(nSims, maxT, initialState, longBel, policy, Transition.matrices, benefitRatio)
  df_simMOMDPactions <- simulate.MOMDP(nSims, maxT, initialState, longBel, policy, Transition.matrices, benefitRatio)
  
  
  
  #remove the 'reward' variable since we don't want to plot immediate reward. rename the other variables
  df_all2 <- df_all %>% filter(series != "reward") 
  #drop the factor level for reward
  df_all2$series <- factor(df_all2$series)
  
  df_all2$series <-  mapvalues(df_all2$series, from = c("fox_0", "species_0", "sum_reward"), to = c("(a) Threat", "(b) Species", "(c) Sum Rewards"))
  df_all2$action[which(df_all2$action== "do_nothing")] <- "Do nothing"
  
  varnames <- levels(df_all2$series)
   ddummy <-  data.frame(time=1, series=rep(varnames[1:2], each=2), 
                         value=c(rep(c(1:2,1,3), times=1)))#, #fox_0 ranges from 1:2, species_0 from 1:3, 
                                 #-20,0)) #limit on the reward (arbitrary lower bound, what should this be?)

  #plot simulation variables
   # Colorblind palette with black: 
   cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#000000")
   #cbbPalette <- c("#E69F00", "#56B4E9", "#009E73",  "#0072B2", "#CC79A7","#000000")
   
   
  modelName <- paste("Model",initialState[3],initialState[4], sep=" ")
  plotdf.all <- ggplot(data= df_all2, aes(x=time)) +
    geom_line(aes(y=lower, group=action, colour= factor(action)), linetype="dashed", size=0.8) +
    geom_line(aes(y=upper,group=action, colour= factor(action)), linetype="dashed", size=0.8)+
    geom_line(aes(y=mean,group=action, colour= factor(action)), size=1.2) +
    geom_blank(data= ddummy, aes(x=time, y=value)) +
    xlab("Time") +
    facet_wrap(vars(series), scales="free_y", ncol=1)+
    ggtitle(modelName) +
    ylab("") +
    scale_colour_manual(values=cbbPalette)+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5))  #centres the plot title
  plotdf.all <- plotdf.all+ labs(color='Action')  
  print(plotdf.all)
  
  #save as svg so we can edit in inkscape
  imgNameSVG1 <- paste('./pomdp_solved/FiguresMS/Fig1_simulation.svg', sep="")
  ggsave(file=imgNameSVG1, plot=plotdf.all, width=10, height=8)
  
  ### FIGURE 2 ########
  ### action plot
  actionDat.long<-melt(df_simMOMDPactions$action.freq[,-1])
  colnames(actionDat.long)[3] <- "Frequency"
  plotAction <- ggplot(actionDat.long, aes(x = Var2, y = Var1)) + 
    geom_raster(aes(fill=Frequency)) + 
    #scale_fill_gradient(low="grey90", high="red") +
    scale_fill_gradient(low="grey90", high= cbbPalette[1]) +
    labs(x="Time", y="Action", title="MOMDP Action Selection Freq.") +
    theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                       axis.text.y=element_text(size=9),
                       plot.title=element_text(size=11))
  print(plotAction)
  
  #save as svg so we can edit in inkscape
  imgNameSVG2 <- paste('./pomdp_solved/FiguresMS/Fig2_action.svg', sep="")
  ggsave(file=imgNameSVG2, plot=plotAction, width=20, height=8)
  
  ### belief plots
  df.Belief.plot.fox <- df_belief[[2]]
  #output text with terminal beliefs
  end.BelT <- df_belief[[2]][df_belief[[2]]$time== maxT+1,]$mean  #get the mean beliefs of each model at the terminal time
  endBelThreat <- paste(c("Terminal Threat Belief:", round(end.BelT,2)), collapse=" ")
  #plot Fox marginal belief
  df.Belief.plot.fox$series <- factor(df.Belief.plot.fox$series)
  plotdf.Bel.fox <- ggplot(data= df.Belief.plot.fox, aes(x=time))+
    geom_line(aes(y=lower, group=series, colour= factor(series)), linetype="dashed", size=0.8) +
    geom_line(aes(y=upper,group=series, colour= factor(series)), linetype="dashed", size=0.8)+
    geom_line(aes(y=mean,group=series, colour= factor(series)), size=1.2) +
    ylim(0,1) +
    ggtitle("Threat Model Belief") +
    ylab("Belief")+
    xlab("Time")+
    annotate("text", x=maxT*0.66, y=0.95, label=endBelThreat) +
    scale_colour_manual(values=cbbPalette)+
    theme_bw() 
  plotdf.Bel.fox <- plotdf.Bel.fox+ labs(color='Model') 
  print(plotdf.Bel.fox)
  
  imgNameSVG3 <- paste('./pomdp_solved/FiguresMS/Fig2_threatBel.svg', sep="")
  ggsave(file=imgNameSVG3, plot=plotdf.Bel.fox, width=10, height=8)
  
  
  
  #plot Species marginal belief
  df.Belief.plot.species <- df_belief[[3]]
  #output text with terminal beliefs
  end.BelS <- df_belief[[3]][df_belief[[3]]$time== maxT+1,]$mean  #get the mean beliefs of each model at the terminal time
  endBelSpecies <- paste(c("Terminal Species Belief:", round(end.BelS,3)), collapse=" ")
  df.Belief.plot.species$series <- factor(df.Belief.plot.species$series)
  plotdf.Bel.species <- ggplot(data= df.Belief.plot.species, aes(x=time))+
    geom_line(aes(y=lower, group=series, colour= factor(series)), linetype="dashed", size=0.8) +
    geom_line(aes(y=upper,group=series, colour= factor(series)), linetype="dashed", size=0.8)+
    geom_line(aes(y=mean,group=series, colour= factor(series)), size=1.2) +
    ylim(0,1) +
    ggtitle("Species Model Belief") +
    ylab("Belief") +
    xlab("Time")+
    annotate("text", x=maxT*0.66, y=0.95, label=endBelSpecies) +
    scale_colour_manual(values=cbbPalette)+
    theme_bw()
  plotdf.Bel.species <- plotdf.Bel.species+ labs(color='Model') 
  print(plotdf.Bel.species)
  
  imgNameSVG4 <- paste('./pomdp_solved/FiguresMS/Fig2_speciesBel.svg', sep="")
  ggsave(file=imgNameSVG4, plot=plotdf.Bel.species, width=10, height=8)

  
  ## solve alpha-min-fast
  maxDepthPolgraph <- 2 #depth of policy tree (recommend not greater than 4 for readability)  

  beliefs.filename <- "beliefs.txt"
  precision_a <- 0.05  #user-defined
  N <- 15 #user-define number of alphavectors
  #run the alpha-min-fast function
  print("Commencing alpha-min-fast algorithm. Please wait")
  print(c("precision_a=", precision_a))
  print(c("policy.filename=", policy.filename))
  print(c("beliefs.filename=", beliefs.filename))
  print(c("N=", N))
  reducedPolicy <- alpha_min_fast(policy.filename, beliefs.filename, precision_a, N)
  
  filename.out <- "pomdp_solved/reducedPolicy_forMS.policy"
  print(paste("Alpha-min-fast completed. Saving output file in .dot format to: ", filename.out, sep=""))
  policyGraphFileName <- print.reduced.policy(policy.filename, reducedPolicy,  filename.out)  #writes to 
  print("Policy file saved. Rendering plot")
  # policyGraphFileName <- 'pomdp_solved/reducedPolicyGraph.dot'
  plotdf.policyGr <- grViz(policyGraphFileName, engine = "dot")
  
  #save policy graph plot as a pdf-- tends not to display well if polgraph is big: use svg instead
  imgName <- paste('./pomdp_solved/FiguresMS/polGraphMS_depth_', maxDepthPolgraph,'_precision_',precision_a,".pdf", sep="")
  plotdf.policyGr %>% export_svg %>% charToRaw %>% rsvg_pdf(imgName)
  
  #save policy graph plot as a pdf
  imgNameSVG <- paste('./pomdp_solved/FiguresMS/polGraphMS_depth_', maxDepthPolgraph,'_precision_',precision_a,".svg", sep="")
  plotdf.policyGr %>% export_svg %>% charToRaw %>% rsvg_svg(imgNameSVG)
  print(paste("Policy graph saved to", imgNameSVG, sep=""))
  
  
  #plot the policy graph for the current 'true' model from the input
 # true.model <- c(input$foxModLabel, input$spModLabel)
  belief.stationary <- df_belief[[1]][df_belief[[1]]$time== maxT+1,]$mean  #get mean beliefs of each model at the terminal time
  #Transition.matrices <- get.transition(SpeciesMat, threatMat1a, threatMat2a, input$recoverProb)
  
  reducedPol <- read.policy(filename.out)
  
  policyGraphFileName.cond <- draw.polgraph.conditional(belief.stationary, true.model, reducedPol, Transition.matrices, threshold= 0.05)
  #draw the policy graph
  plotdf.policyGr <- grViz(policyGraphFileName.cond, engine = "dot")
  
  # #save policy graph plot as an svg
  imgNameSVG <- paste('./pomdp_solved/FiguresMS/polGraph_mini', true.model[1], '_', true.model[2],'.svg', sep="")
  plotdf.policyGr %>% export_svg %>% charToRaw %>% rsvg_svg(imgNameSVG)
  print(paste("Policy graph saved to", imgNameSVG, sep=""))
  