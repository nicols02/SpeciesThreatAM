#run this app from GitHub by typing the commands:
#install.packages('shiny')
#install.packages('shinyMatrix')
#install.packages('ggplot2')
#install.packages('reshape2')
#shiny::runGitHub( "FoxAM", "nicols02")
#also needs sarsop installed

#check if required packages are installed, if not, install and attach to the library
if (library(shiny,logical.return=TRUE)==FALSE) {
  install.packages("shiny")
  library("shiny")
}
if (library(shinyMatrix,logical.return=TRUE)==FALSE) {
  install.packages("shinyMatrix")
  library("shinyMatrix")
}
if (library(ggplot2,logical.return=TRUE)==FALSE) {
  install.packages("ggplot2")
  library("ggplot2")
}
if (library(reshape2,logical.return=TRUE)==FALSE) {
  install.packages("reshape2")
  library("reshape2")
}
if (library(DiagrammeR,logical.return=TRUE)==FALSE) {
  install.packages("DiagrammeR")
  library("DiagrammeR")
}
if (library(Rfast,logical.return=TRUE)==FALSE) {
  install.packages("Rfast")
  library("Rfast")
}
if (library(rsvg,logical.return=TRUE)==FALSE) {
  install.packages("rsvg")
  library("rsvg")
}
if (library(DiagrammeRsvg,logical.return=TRUE)==FALSE) {
  install.packages("DiagrammeRsvg")
  library("DiagrammeRsvg")
}
if (library(magrittr,logical.return=TRUE)==FALSE) {
  install.packages("magrittr")
  library("magrittr")
}
# if (library(dplyr,logical.return=TRUE)==FALSE) {
#   install.packages("plyr")
#   library("plyr")
# }
if (library(dplyr,logical.return=TRUE)==FALSE) {
  install.packages("dplyr")
  library("dplyr")
}




# library(shiny)
# library(shinyMatrix)
# library(ggplot2)
# library(reshape2)

source("generate_transition_matrices.R", local=TRUE)#local=environment() )
source("sarsop_parse_Shiny.R", local=TRUE)#local=environment() )
source("alpha_min_fast.R", local=TRUE)
source("draw_polgraph.R", local=TRUE)

######### SET UP THE DATA COLLECTION MATRICES ###################
#clean/empty matrix
specMatInit <- matrix(data=0, nrow= 6, ncol=2)
#prefilled for species threat (currently just made up for demonstration)
#specMatInit <- matrix(data= c(0.02, 0.3, 0.015, 0.35, 0.05, 0.25, 0.025, 0.3, 0.15, 0.1, 0.1, 0.15), nrow=6, ncol=2, byrow=TRUE)
specMatInit <- matrix(data= c(0.02, 0.8, 0.015, 0.8, 0.05, 0.25, 0.025, 0.3, 0.15, 0.02, 0.1, 0.01), nrow=6, ncol=2, byrow=TRUE)

colnames(specMatInit) <- c("P(Extinct)", "P(High)")
rownames(specMatInit) <- c("(Not Present, Low)",
                           "(Not Present, High)",
                           "(Low, Low)",
                           "(Low, High)",
                           "(High, Low)",
                           "(High, High)")

#clean/empty matrix
threatMat1 <- matrix(data=0, nrow=1, ncol=2) 
#prefilled for fox threat
threatMat1 <- matrix(data= c(0.875, 0.283333), nrow=1, ncol=2) #values obtained from experts (AM_elicitation_combined.xlsx)
colnames(threatMat1) <- c("P(High|A0)", "P(High|A3)")
rownames(threatMat1) <- c("High")

#clean/empty matrix
threatMat2 <- matrix(data=0, nrow=1, ncol=4)
#prefilled for fox threat
threatMat2 <- matrix(data= c(0.433333333,	0.666666667,	0.675, 0.866666667),
                     nrow=1, ncol=4) #values obtained from experts (AM_elicitation_combined.xlsx)
colnames(threatMat2) <- c("P(Low|A0)", "P(Low|A1)","P(Low|A2)","P(Low|A3)")
rownames(threatMat2) <- c("Low")

#add some variables-- we can make these reactive later

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

#shorten the initial belief values for input to the UI (allows reactive input without getting sum >1)
initSpeciesBel_short <- matrix(initSpeciesBel[,-length(initSpeciesBel)], nrow=1, ncol= (n.Speciesmodels-1))
colnames(initSpeciesBel_short) <- head(speciesModel.names,-1)

initThreatBel_short <- matrix(initThreatBel[,-length(initThreatBel)], nrow=1, ncol= (n.Foxmodels-1))
colnames(initThreatBel_short) <- head(foxModel.names,-1)

#############################################################
#Build UI#
ui <- fluidPage(
  
  navbarPage("Threat-Species Adaptive Management",
  
    tabPanel("Simulate",
      sidebarLayout(
        sidebarPanel( width= 6,
          div(style="display: inline-block;vertical-align:top; width: 150px;",numericInput("maxT", "Length of simulation",value=20,min=1, max=NA, step=1)),
          div(style="display: inline-block;vertical-align:top; width: 180px;",numericInput("nSims", "Number of simulations",value=30,min=1, max=NA, step=1)),
          br(),            #line break
          div(style="display: inline-block;vertical-align:top; width: 150px;",numericInput("recoverProb", "Recovery Prob",value=0,min=0, max=1, step=0.1)),
          br(),            #line break
          div(style="display: inline-block;vertical-align:top; width: 150px;",numericInput("costExt", "Benefit of nonextinction",value=20,min=0, max=NA, step=1)),
          br(),            #line break
          #numericInput("maxT", "Length of simulation",value=20,min=1, max=NA, step=1, width= '100px'),
          #numericInput("nSims", "Number of simulations",value=30,min=1, max=NA, step=1, width= '100px'),
          matrixInput("actionCost",class="numeric",
                      value= CostRatio,
                      rows= list(names = TRUE),
                      cols= list(names= TRUE),
                      copy = TRUE,
                      paste = TRUE),
          br(),
          
          strong("Threat Elicitation"),
          #elicit threat information
          matrixInput("threatMat1a",class="numeric",
                      value= threatMat1,
                      rows= list(names = TRUE),
                      cols= list(names= TRUE),
                      copy = TRUE,
                      paste = TRUE),
          
          matrixInput("threatMat2a",class="numeric",
                      value= threatMat2,
                      rows= list(names = TRUE),
                      cols= list(names= TRUE),
                      copy = TRUE,
                      paste = TRUE),
          #elicit species information
          strong("Species Elicitation"),
          matrixInput("SpeciesMat",class="numeric",
                      value= specMatInit,
                      rows= list(names = TRUE),
                      cols= list(names= TRUE),
                      copy = TRUE,
                      paste = TRUE),
          
    
          radioButtons("foxModLabel", "Threat Model", foxModel.names, selected = "F5", inline = TRUE),
          radioButtons("spModLabel", "Species Model", speciesModel.names, selected = "S2", inline=TRUE),
          
          checkboxGroupInput("actionLabel", "Simulated Actions", choices= actions.list,
                             selected = c("do_nothing", "a3"), width = NULL),
          
          #reate action buttons for generating the pomdpx file and solving the POMDP with sarsop
          actionButton("getPOMDPX", "generate POMDPX file"),
          br(),
          div(style="display: inline-block;vertical-align:top; width: 150px;",numericInput("SARSOPtol", "MOMDP tolerance",value=0.5,min=0, max=1, step=0.05)),
          div(style="display: inline-block;vertical-align:top; width: 150px;",numericInput("SARSOPtimeout", "MOMDP timeout (sec)",value=100,min=0, step=10)),
          
          br(), 
          
          actionButton("solvePOMDP", "Solve the MOMDP"),
          
         
          ), #end sidebarpanel
        
  
  
        mainPanel(width= 6,
                  plotOutput("SimPlot2", height= "1000px")
                  
        )
      ) #close tabPanel
    ), #close Navbar page
    
    tabPanel("Belief simulation",
             strong("Simulated belief plots"),
             br(), #add a new line
             textOutput("TrueModelReport"),
             plotOutput("SimPlot3", height= "300px"),
             br(), #add a new line
             br(),
             textOutput("threatBelTerminal"),
             plotOutput("SimPlot4", height= "300px"),
             br(), #add a new line
             br(),
             textOutput("speciesBelTerminal"),
             br(),
             strong("Action Selection Frequency during simulation"),
             plotOutput("SimPlot_action", height= "300px")
             ),
    
    tabPanel("Explore POMDP Solution",
             strong("Current state"),
             br(), #add a new line
             div(style="display: inline-block;vertical-align:top; width: 150px;",
                 radioButtons("threatState", "Threat State", choiceNames= list("Low", "High"), 
                              choiceValues= list("LowF", "HighF"), selected = "LowF", inline = FALSE)),
            
             div(style="display: inline-block;vertical-align:top; width: 180px;",
                  radioButtons("speciesState", "Species State", choiceNames= list("Loc. Extinct", "Low", "High"), 
                               choiceValues= list("LocExtSp", "LowSp", "HighSp"), selected = "LowSp", inline = FALSE)),
             br(),
             
             strong("Initial Belief: Threat models"),
             #elicit current belief information-- threat
             matrixInput("initThreatBel_shorta",class="numeric",
                         value= initThreatBel_short,
                         rows= list(names = TRUE),
                         cols= list(names= TRUE),
                         copy = TRUE,
                         paste = TRUE),
             
             textOutput("showBel1"),
             br(),
             
             strong("Initial Belief: Species models"),
             #elicit current belief information--species
             matrixInput("initSpeciesBel_shorta",class="numeric",
                         value= initSpeciesBel_short, #remove last value and compute reactively to ensure sum to 1
                         rows= list(names = TRUE),
                         cols= list(names= TRUE),
                         copy = TRUE,
                         paste = TRUE),
             
             textOutput("showBel2"),
             
             br(),
          actionButton("getOptimalAct", "Get Optimal Action"),
          textOutput("OptActionVal") ,

          
          strong("Next state"),
          br(), #add a new line
          div(style="display: inline-block;vertical-align:top; width: 150px;",
              radioButtons("threatState2", "Threat State", choiceNames= list("Low", "High"), 
                           choiceValues= list("LowF", "HighF"), inline = FALSE)),
          
          div(style="display: inline-block;vertical-align:top; width: 180px;",
              radioButtons("speciesState2", "Species State", choiceNames= list("Loc. Extinct", "Low", "High"),
                           choiceValues= list("LocExtSp","LowSp", "HighSp"), inline = FALSE)),
          br(),
          br(),
          textOutput("action.print"),
          textOutput("newBelief.th"),
          textOutput("newBelief.sp"),
          
          
          HTML(
            paste(
              h6("(Note that if you select a state where the species goes from Locally Extinct to extant (and have zero recovery probability), the belief will return NaN)")
            ) )
            
    ), #close tabpanel
    
    
     tabPanel("Policy graph",
              h4(strong("Policy graph plot")),
              br(),
              div(style="display: inline-block;vertical-align:top; width: 150px;",numericInput("precision_alphamin", "Desired tolerance for alphamin",value=0.05,min=0, max=NA, step=0.01)),
              br(),  
              div(style="display: inline-block;vertical-align:top; width: 150px;",numericInput("numvect_alphamin", "Max number of alphavectors",value=15,min=0, max=NA, step=1)),
              br(),  
              actionButton("alphaMinSolve", "Compress Policy"),
              br(),
              h6("Note that Compress Policy takes some time to run."),
              h6("You can observe progress via the print messages in the RStudio console"),
              h6("The button compresses the policy using the alpha-min-fast algorithm and plots a policy graph below."),
              h6("When completed, output .dot and .svg files are saved to the ./pomdp_solved folder."),
              grVizOutput('polgraphImg', width = "100%", height = "760px") #render the policy graph
             #imageOutput("myImage")
     )          
    
      
    
  ) #close UI
)

##########################################
#write server#
server <- function(input, output, session) {
  benefitRatio <- reactive({ c(0,input$costExt,input$costExt) })  #set benefitRatio for all functions based on input value
  
  
  df_all <- reactive({
    #benefitRatio= c(0,input$costExt,input$costExt)
    
    true.model <- c(input$foxModLabel, input$spModLabel)
    initialState <- c("HighF", "LowSp", true.model[1], true.model[2])
    
    Transition.matrices <- get.transition(input$SpeciesMat, input$threatMat1a, input$threatMat2a, input$recoverProb)
    #create the df for ggplot
    
    beliefSpecies <- c(input$initSpeciesBel_shorta, 1-sum(input$initSpeciesBel_shorta))
    beliefThreat <- c(input$initThreatBel_shorta, 1-sum(input$initThreatBel_shorta))
    longBel <- getLongFormatBelief(beliefThreat, beliefSpecies) #convert to long format belief
    
    
    outfileName1 <- paste("./pomdp_solved/", "ShinySolution","_", paste(benefitRatio(), collapse="_"),".policy", sep="")
    prepare.plot(benefitRatio(), input$actionCost, input$nSims, input$maxT, true.model, initialState,Transition.matrices,input$actionLabel, longBel, outfileName1)
    })
  
  df_belief <- reactive({
    true.model <- c(input$foxModLabel, input$spModLabel)
    initialState <- c("HighF", "LowSp", true.model[1], true.model[2])
    Transition.matrices <- get.transition(input$SpeciesMat, input$threatMat1a, input$threatMat2a, input$recoverProb)
    #benefitRatio= c(0,input$costExt,input$costExt)
    
    beliefSpecies <- c(input$initSpeciesBel_shorta, 1-sum(input$initSpeciesBel_shorta))
    beliefThreat <- c(input$initThreatBel_shorta, 1-sum(input$initThreatBel_shorta))
    longBel <- getLongFormatBelief(beliefThreat, beliefSpecies) #convert to long format belief
    
    policy.filename <- paste("./pomdp_solved/ShinySolution_", paste(benefitRatio(), collapse="_"),".policy", sep="")
    policy <- read.policy(policy.filename)
    
    simulate.MOMDP.belief(input$nSims, input$maxT, initialState, longBel, policy, Transition.matrices, benefitRatio())
      
  })
  
  df_simMOMDPactions <- reactive({
    #benefitRatio= c(0,input$costExt,input$costExt)
    
    true.model <- c(input$foxModLabel, input$spModLabel)
    initialState <- c("HighF", "LowSp", true.model[1], true.model[2])
    
    Transition.matrices <- get.transition(input$SpeciesMat, input$threatMat1a, input$threatMat2a, input$recoverProb)
  
    policy.filename <- paste("./pomdp_solved/ShinySolution_", paste(benefitRatio(), collapse="_"),".policy", sep="")
    policy <- read.policy(policy.filename)
    
    beliefSpecies <- c(input$initSpeciesBel_shorta, 1-sum(input$initSpeciesBel_shorta))
    beliefThreat <- c(input$initThreatBel_shorta, 1-sum(input$initThreatBel_shorta))
    longBel <- getLongFormatBelief(beliefThreat, beliefSpecies) #convert to long format belief
    
    simulate.MOMDP(input$nSims, input$maxT, initialState, longBel, policy, Transition.matrices, benefitRatio())
    
  })
  
  #beliefSpeciesA <- reactive({input$initSpeciesBel_shorta})#, 1-sum(input$initSpeciesBel_shorta))})
  #beliefThreatA <- reactive({input$initThreatBel_shorta})#, 1-sum(input$initThreatBel_shorta))})
  
  #generate the POMDPX file reactively
  observeEvent(input$getPOMDPX, {
    print("starting writing POMDPX file")
    runName <- "ShinyGrab"
    outputFileName <- paste("pomdpx_files/sarsop_input_", runName,".pomdpx", sep="")
    sarsop_parse(input$SpeciesMat, input$threatMat1a, input$threatMat2a, input$recoverProb, benefitRatio(), input$actionCost, outputFileName)
    print("finished writing to \"./pomdpx_files/filename\"")
  })
  
  #solve the POMDPX after creating the POMDPX
  observeEvent(input$solvePOMDP, {
    print("starting solving POMDP file: please wait")
    
    #input pomdpx always has the same name (output from sarsop_parse, called by POMDPX button)
    datfileName <- "./pomdpx_files/sarsop_input_ShinyGrab.pomdpx"
    
    #output the solution into the /pomdp_solved directory; name according to benefitRatio
    #benefitRatio=c(0,input$costExt,input$costExt)
    outfileName <- paste("./pomdp_solved/ShinySolution_", paste(benefitRatio(), collapse="_"),".policy", sep="")
    
    #write an external command for sarsop to run and call it using system(cmd)
    precision <- input$SARSOPtol#1e-1  #set precision for sarsop
    cmd <- paste("./sarsop/src/pomdpsol.exe \"", datfileName, "\" --precision ", precision, " --timeout ", input$SARSOPtimeout," --output ", outfileName, sep="")
    system(cmd)
    
    print(paste("finished solving. Writing to:", outfileName, collapse=""))
  })
  
  optAct.reactVals <- reactiveValues(optAct2= "do_nothing")#,
                                     #  #make the optimal action a reactive value and initialise it
  
  output$showBel1 <- renderText({
    beliefThreat <- c(input$initThreatBel_shorta, 1-sum(input$initThreatBel_shorta))
    paste(c("The current threat belief is:", beliefThreat, collapse=""))})
  
  output$showBel2 <- renderText({
    beliefSpecies <- c(input$initSpeciesBel_shorta, 1-sum(input$initSpeciesBel_shorta))
    paste(c("The current species belief is:", beliefSpecies, collapse=""))})
  
  
 
  #get the optimal action when the button is pressed
  observeEvent(input$getOptimalAct, {
    print("retrieving optimal action...")
    #print(beliefThreat)
    beliefSpecies <- c(input$initSpeciesBel_shorta, 1-sum(input$initSpeciesBel_shorta))
    beliefThreat <- c(input$initThreatBel_shorta, 1-sum(input$initThreatBel_shorta))
    longBel <- getLongFormatBelief(beliefThreat, beliefSpecies) #convert to long format belief
    #benefitRatio <- c(0,input$costExt,input$costExt)
    policy.filename <- paste("./pomdp_solved/ShinySolution_", paste(benefitRatio(), collapse="_"),".policy", sep="")
    policy <- read.policy(policy.filename)
    OptAct <- getOptAction(policy, longBel, n.actions)
    #set value of reactive optact.reactVals$optAct2 so we can access this later
    optAct.reactVals$optAct2 <- OptAct
    #OptBel <- updateBelief(longBel, prevState, nextState, OptAct,Transition.matrices)
    output$OptActionVal <- renderText({paste("The optimal action is ", OptAct)})
    print("found optimal action")
  })  
  
  toListen <- reactive({
    list(input$threatState2,input$speciesState2)
  })  #listen for both states to be input before updating
  
  observeEvent(toListen(), {  #react when both buttons pushed
     beliefSpecies <- c(input$initSpeciesBel_shorta, 1-sum(input$initSpeciesBel_shorta))
     beliefThreat <- c(input$initThreatBel_shorta, 1-sum(input$initThreatBel_shorta))
     longBel <- getLongFormatBelief(beliefThreat, beliefSpecies) #convert to long format belief
     print(paste(c("Initial Threat Belief: ",round(beliefThreat,3)), collapse = " "))  #output to console for error checking
     print(paste(c("Initial Species Belief: ", round(beliefSpecies,3)),collapse=" "))
     
     
     initState <- c(input$threatState, input$speciesState)
     nextState <- c(input$threatState2, input$speciesState2)
     action <-  optAct.reactVals$optAct2  #reacts based on the value after pushing the getOptAction button, otherwise default value do_nothing
     Transition.matrices <- get.transition(input$SpeciesMat, input$threatMat1a, input$threatMat2a, input$recoverProb)
     
     newBel <- updateBelief(longBel, initState, nextState, action,Transition.matrices)
     
     #convert long form belief back to short forms
     newBel.short <- extract.modelBelief2(newBel)
     newThreatBel <- newBel.short[[1]] 
     newSpeciesBel <- newBel.short[[2]] 
     print("belief updated")
     
     
     output$action.print <- renderText({paste("The action applied was", action, collapse="")})
     output$newBelief.th <- renderText({
       paste(c("The new threat belief is", round(newThreatBel,3), collapse=""))})
     output$newBelief.sp <- renderText({
       paste(c("The new species belief is ", round(newSpeciesBel,3), collapse=""))})
     
   })
  

  
  #render the plots
  output$SimPlot2 <- renderPlot({
    true.model <- c(input$foxModLabel, input$spModLabel)
    initialState <- c("HighF", "LowSp", true.model[1], true.model[2])
    
    df_all2 <- df_all()
    df_all2$series <-  plyr::mapvalues(df_all2$series, from = c("fox_0", "species_0", "reward", "sum_reward"), to = c("(a) Threat", "(b) Species", "(c) Reward", "(d) Sum Rewards"))
    df_all2$action[which(df_all2$action== "do_nothing")] <- "Do nothing"
    
    #varnames <- c("fox_0", "species_0",  "foxModel_0", "speciesModel_0", "reward")
    varnames <- levels(df_all2$series)
    #make a dummy dataframe containing the y limits for the plots
    ddummy <-  data.frame(time=1, series=rep(varnames[1:3], each=2), 
                          value=c(rep(c(1:2,1,3), times=1), #fox_0 ranges from 1:2, species_0 from 1:3, 
                                  0,20)) #limit on the reward (arbitrary lower bound, what should this be?)

    #plot simulation variables
    modelName <- paste("Model",initialState[3],initialState[4], sep="_")
    plotdf.all <- ggplot(data= df_all2, aes(x=time)) +
      geom_line(aes(y=lower, group=action, colour= factor(action)), linetype="dashed", size=0.8) +
      geom_line(aes(y=upper,group=action, colour= factor(action)), linetype="dashed", size=0.8)+
      geom_line(aes(y=mean,group=action, colour= factor(action)), size=1.2) +
      geom_blank(data= ddummy, aes(x=time, y=value)) +
      facet_wrap(vars(series), scales="free_y", ncol=1)+
      ggtitle(modelName) +
      ylab("") +
      theme(plot.title = element_text(hjust = 0.5))  #centres the plot title
    plotdf.all <- plotdf.all+ labs(color='Action')
    print(plotdf.all)

  }, height = 1000, units="px")
  
  
  output$SimPlot_action <- renderPlot({
    actionDat.long<-melt(df_simMOMDPactions()$action.freq[,-1])
    colnames(actionDat.long)[3] <- "Frequency"
    plotAction <- ggplot(actionDat.long, aes(x = Var2, y = Var1)) + 
      geom_raster(aes(fill=Frequency)) + 
      scale_fill_gradient(low="grey90", high="red") +
      labs(x="Time", y="Action", title="MOMDP Action Selection Freq.") +
      theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                         axis.text.y=element_text(size=9),
                         plot.title=element_text(size=11))
    print(plotAction)
    
  }, height = 300, units="px")
  
  output$threatBelTerminal <- renderText({  #print out terminal simulated belief vector
    end.BelT <- df_belief()[[2]][df_belief()[[2]]$time== input$maxT+1,]$mean  #get the mean beliefs of each model at the terminal time
    paste(c("Terminal Threat Belief:", round(end.BelT,3), collapse=""))})
  
  
  
  # #render the belief plots
  output$TrueModelReport <- renderText({  #"true" model contained in initial state
    true.model <- c(input$foxModLabel, input$spModLabel)
    paste(c("True model:", true.model, collapse=""))})
  
  output$SimPlot3 <- renderPlot({
    df.Belief.plot.fox <- df_belief()[[2]]
    #plot Fox marginal belief
    # df.Belief.plot.fox <- df_belief$df.Belief.fox
    df.Belief.plot.fox$series <- factor(df.Belief.plot.fox$series)
    plotdf.Bel.fox <- ggplot(data= df.Belief.plot.fox, aes(x=time))+
      geom_line(aes(y=lower, group=series, colour= factor(series)), linetype="dashed", size=0.8) +
      geom_line(aes(y=upper,group=series, colour= factor(series)), linetype="dashed", size=0.8)+
      geom_line(aes(y=mean,group=series, colour= factor(series)), size=1.2) +
      ylim(0,1) +
       ggtitle("Threat Model Belief") +
       ylab("Belief")
     plotdf.Bel.fox <- plotdf.Bel.fox+ labs(color='Model') 
     print(plotdf.Bel.fox)
    }, height = 300, units="px")
  
  output$threatBelTerminal <- renderText({  #print out terminal simulated belief vector
    end.BelT <- df_belief()[[2]][df_belief()[[2]]$time== input$maxT+1,]$mean  #get the mean beliefs of each model at the terminal time
    paste(c("Terminal Threat Belief:", round(end.BelT,3), collapse=""))})
  
  
  output$SimPlot4 <- renderPlot({
    #plot Species marginal belief
    df.Belief.plot.species <- df_belief()[[3]]
    df.Belief.plot.species$series <- factor(df.Belief.plot.species$series)
     plotdf.Bel.species <- ggplot(data= df.Belief.plot.species, aes(x=time))+
       geom_line(aes(y=lower, group=series, colour= factor(series)), linetype="dashed", size=0.8) +
       geom_line(aes(y=upper,group=series, colour= factor(series)), linetype="dashed", size=0.8)+
       geom_line(aes(y=mean,group=series, colour= factor(series)), size=1.2) +
       ylim(0,1) +
       ggtitle("Species Model Belief") +
       ylab("Belief")
     plotdf.Bel.species <- plotdf.Bel.species+ labs(color='Model') 
     print(plotdf.Bel.species)
  }, height = 300, units="px")
  
  output$speciesBelTerminal <- renderText({  #print out terminal simulated belief vector
    end.BelS <- df_belief()[[3]][df_belief()[[3]]$time== input$maxT+1,]$mean  #get the mean beliefs of each model at the terminal time
    paste(c("Terminal Species Belief:", round(end.BelS,3), collapse=""))})
  
  observeEvent(input$alphaMinSolve, {
    maxDepthPolgraph <- 2 #depth of policy tree (recommend not greater than 4 for readability)  
    # benefitRatio= c(0,input$costExt,input$costExt)  #c(-20,0,0)
    policy.filename <- paste("./pomdp_solved/ShinySolution_", paste(benefitRatio(), collapse="_"),".policy", sep="")
    
    #policy.filename <- "./pomdp_solved/test.policy"
   # policy <- read.policy(policy.filename)
    beliefs.filename <- "beliefs.txt"
    precision_a <- input$precision_alphamin#0.05  #user-defined
    N <- input$numvect_alphamin #10 #user-define number of alphavectors
    
    #run the alpha-min-fast function
    print("Commencing alpha-min-fast algorithm. Please wait")
    print(c("precision_a=", precision_a))
    print(c("policy.filename=", policy.filename))
    print(c("beliefs.filename=", beliefs.filename))
    print(c("N=", N))
    reducedPolicy <- alpha_min_fast(policy.filename, beliefs.filename, precision_a, N)  #turn off to save comp time
     
     
     filename.out <- "pomdp_solved/reducedPolicy.policy"
     print(paste("Alpha-min-fast completed. Saving output file in .dot format to: ", filename.out, sep=""))
     policyGraphFileName <- print.reduced.policy(policy.filename, reducedPolicy,  filename.out)  #writes to 
     print("Policy file saved. Rendering plot")
     policyGraphFileName <- 'pomdp_solved/reducedPolicyGraph.dot'
     plotdf.policyGr <- grViz(policyGraphFileName, engine = "dot")
     plotdf.policyGr  #plot to Shiny output
     
     
     #save policy graph plot as a pdf-- tends not to display well if polgraph is big: use svg instead
     imgName <- paste('./pomdp_solved/polGraph_depth_', maxDepthPolgraph,'_precision_',precision_a,".pdf", sep="")
     plotdf.policyGr %>% export_svg %>% charToRaw %>% rsvg_pdf(imgName)
     
     #save policy graph plot as a svg
     imgNameSVG <- paste('./pomdp_solved/polGraph_depth_', maxDepthPolgraph,'_precision_',precision_a,".svg", sep="")
     plotdf.policyGr %>% export_svg %>% charToRaw %>% rsvg_svg(imgNameSVG)
     print(paste("Policy graph saved to", imgNameSVG, sep=""))
    
    #plot the policy graph for the current 'true' model from the input
    true.model <- c(input$foxModLabel, input$spModLabel)
    belief.stationary <- df_belief()[[1]][df_belief()[[1]]$time== input$maxT+1,]$mean  #get mean beliefs of each model at the terminal time
    Transition.matrices <- get.transition(input$SpeciesMat, input$threatMat1a, input$threatMat2a, input$recoverProb)
    
    reducedPol <- read.policy(filename.out)
    
    policyGraphFileName.cond <- draw.polgraph.conditional(belief.stationary, true.model, reducedPol, Transition.matrices, threshold= 0.05)
    #draw the policy graph
    plotdf.policyGr <- grViz(policyGraphFileName.cond, engine = "dot")
  
    # #save policy graph plot as an svg
     imgNameSVG <- paste('./pomdp_solved/polGraph_mini', true.model[1], '_', true.model[2],'.svg', sep="")
     plotdf.policyGr %>% export_svg %>% charToRaw %>% rsvg_svg(imgNameSVG)
     print(paste("Policy graph saved to", imgNameSVG, sep=""))
     
     plotYN(1)  #set the reactive plot value to 1 after this code has been run
     #TO DO: PLOT TO SHINY. ALSO UNCOMMENT ALPHA-MIN LINES ABOVE 
  })
  
  plotYN <- reactiveVal(0)  #initialise a reactive value to zero= don't plot; change after the alphamin button above has been pressed
  
   output$polgraphImg <- renderGrViz({
     if (plotYN()==1){
      policyGraphFileName.cond <- './pomdp_solved/reducedPolgraph_mini.dot'
      plotdf.policyGr <- grViz(policyGraphFileName.cond, engine = "dot")
      plotdf.policyGr  #plot to Shiny output
     }
  })
  # output$polgraphImg <- renderGrViz({
  #    #plot policy graph to specified depth using sarsop polgraph function
  #    maxDepthPolgraph <- 2 #depth of policy tree (recommend not greater than 4 for readability)  
  #   # benefitRatio= c(0,input$costExt,input$costExt)  #c(-20,0,0)
  #    policy.filename <- paste("./pomdp_solved/ShinySolution_", paste(benefitRatio(), collapse="_"),".policy", sep="")
  #    
  #    #policy.filename <- "./pomdp_solved/test.policy"
  #    policy <- read.policy(policy.filename)
  #    beliefs.filename <- "beliefs.txt"
  #    precision_a <- input$precision_alphamin#0.05  #user-defined
  #    N <- input$numvect_alphamin#10 #user-define number of alphavectors
  #    
  #    #run the alpha-min-fast function
  #    print("Commencing alpha-min-fast algorithm. Please wait")
  #    reducedPolicy <- alpha_min_fast(policy.filename, beliefs.filename, precision_a, N)
  #    
  #    
  #    filename.out <- "pomdp_solved/reducedPolicy.policy"
  #    print(paste("Alpha-min-fast completed. Saving output file in .dot format to: ", filename.out, sep=""))
  #    policyGraphFileName <- print.reduced.policy(policy.filename, reducedPolicy,  filename.out)  #writes to 
  #    print("Policy file saved. Rendering plot")
  #   # policyGraphFileName <- 'pomdp_solved/reducedPolicyGraph.dot'
  #    plotdf.policyGr <- grViz(policyGraphFileName, engine = "dot")
  #    plotdf.policyGr  #plot to Shiny output
  #    #install.packages('rsvg')
  #    #install.packages('DiagrammeRsvg')
  #    #library(rsvg)
  #    #library(DiagrammeRsvg)
  #    
  #    #save policy graph plot as a pdf-- tends not to display well if polgraph is big: use svg instead
  #    imgName <- paste('./pomdp_solved/polGraph_depth_', maxDepthPolgraph,'_precision_',precision_a,".pdf", sep="")
  #    plotdf.policyGr %>% export_svg %>% charToRaw %>% rsvg_pdf(imgName)
  #    
  #    #save policy graph plot as a pdf
  #    imgNameSVG <- paste('./pomdp_solved/polGraph_depth_', maxDepthPolgraph,'_precision_',precision_a,".svg", sep="")
  #    plotdf.policyGr %>% export_svg %>% charToRaw %>% rsvg_svg(imgNameSVG)
  #     
  #    
  #   #  ##ATTEMPT TO CLEAN POLICY GRAPH: 
  #   #  #FIRST, STRIP OUT BELIEF IN DOMINANT MODEL FROM DOT FILE; THEN MATCH NODES AND EDGES AND RENAME WITH FEWER NODES
  #   #  #read in dot as character; detect all occurrences of  "\1A" and delete the number before it (belief in dominant model)
  #   #  dat <- readChar(policyGraphFileName, file.info(policyGraphFileName)$size)  #in case you need to read the dot file into R as string
  #   #  #d <- substr(dat, 1, 100)
  #   #  
  #   #  data <- read.table(file(policyGraphFileName),sep='\n', quote= "") 
  #   #  nrowsNode <- grep("root", data[,1])[2]-1 #get second occurrence of 'root'; this is the first line of edge descriptions 
  #   #    for (i in 3:nrowsNode){
  #   #      d <- data[i,]
  #   #     startChar <- regexpr("(F.,S.)",d)[1] + attr(regexpr("(F.,S.)",d), "match.length")
  #   #     endChar <- regexpr("\\lA",d)[1]
  #   #     strReplace <- paste(substr(d,1,startChar),substr(d,endChar-1, nchar(d)), sep=" ")
  #   #     data[i,] <- strReplace
  #   #    }
  #   # 
  #   #  #separate out the node names and their labels
  #   #  nodesData <- matrix(data=NA, nrow= nrowsNode-2, ncol=5)
  #   #  nodesData[,1] <- data[3:nrowsNode,]
  #   #  for (i in 1:nrow(nodesData)){
  #   #    nodesData[i,2] <- substr(nodesData[i,1], 1, regexpr("label=",nodesData[i,1])[1]-2)
  #   #    nodesData[i,3] <- substr(nodesData[i,1], regexpr("label=",nodesData[i,1])[1]-2, nchar(nodesData[i,1]))
  #   #  }
  #   #  nodesData[,4] <- match(nodesData[,3], unique(nodesData[,3]))  #indices based on matches
  #   #  nodesData[,5] <- paste("x", nodesData[,4], sep="")  #new names for each node-- now need to replace names in the edges part of the file based on the mapping between cols 2 and 5
  #   #  nodesData[1,5] <- "root" #keep the name of the root node
  #   #  
  #   #  #replace the nodenames in the original file with their unique identifiers from nodeData[,5]
  #   #  data.edit <- data
  #   #  for (i in 1:nrow(nodesData)){
  #   #   data.edit <- data.frame(lapply(data.edit, function(x) {gsub(pattern=nodesData[i,2], replacement= nodesData[i,5], x)}))
  #   #  }  
  #   #  #delete unneccessary nodes
  #   #   data.edit2 <- unique(data.edit)
  #   #   
  #   # #now need to sum and normalize all incoming probabilities to each node (using edges definitions)  
  #   #   nrowsNode2 <- grep("root", data.edit2[,1])[2]-1 #get second occurrence of 'root'; this is the first line of edge descriptions 
  #   #   nrowsEdge2 <- nrow(data.edit2)- nrowsNode2
  #   #   #separate out the node names and their labels
  #   #   edgesData <- matrix(data=NA, nrow= nrowsEdge2, ncol=4)
  #   #   edgesData[,1] <- data.edit2[(nrowsNode2+1):nrow(data.edit2),1]
  #   #   for (i in 1:nrowsEdge2){
  #   #     d <- data.edit2[i+nrowsNode2,]
  #   #     startChar <- regexpr(")",d)[1]  
  #   #     endChar <- regexpr("\\\\",d)[1]  #escape the double backslash
  #   #     strReplace <- paste(substr(d,1,startChar),substr(d,endChar, nchar(d)), sep=" ") #extract the string for comparisons
  #   #     probD <- as.numeric(substr(d,startChar+1, endChar-1)) #extract the probabiity of the edge for computation
  #   #     edgesData[i,2] <- strReplace
  #   #     edgesData[i,3] <- probD
  #   #   }
  #   #   edgesData[,4] <- match(edgesData[,2], unique(edgesData[,2]))  #indices based on matches
  #   #   #group edges by same index and sum
  #   #   colnames(edgesData) <- c("raw", "matchString","prob","index")
  #   #   edgesData <- as.data.frame(edgesData)
  #   #   edgesData$prob <- as.numeric(edgesData$prob)
  #   #   edgesData$index <- as.numeric(edgesData$index)
  #   #   edgeDatCut <- edgesData %>% 
  #   #                 select(index, prob,matchString)  %>% 
  #   #                 group_by(index, matchString) %>% 
  #   #                 summarise(sum(prob))
  #   #   colnames(edgeDatCut)[3] <- "sumProb"
  #   #   #now need to normalise the outgoing probabilities to ensure sum =1###########
  #   #   
  #   #   #reconstruct the raw string with the new probabilities
  #   #   for (i in 1:nrow(edgeDatCut)){
  #   #     d <- edgeDatCut$matchString[i]
  #   #     startChar <- regexpr(")",d)[1] 
  #   #     endChar <- nchar(d)- nchar("\\l\"];")
  #   #     #edgeDatCut[i,4] <- paste(substr(d,1,startChar), " ", edgeDatCut$sumProb[i], substr(d,startChar+1,nchar(d)), sep="")
  #   #     edgeDatCut[i,4] <- paste(substr(d,1,startChar), " ", edgeDatCut$sumProb[i], "\\l\"];", sep="") #cull observation since always 1
  #   #   }
  #   #   edgeDatCut[nrow(edgeDatCut),4] <- "}" #remove NA created by sumprob
  #   #   nodesformatted <- as.data.frame(data.edit2[(1:nrowsNode2),1])
  #   #   edgesformatted <- as.data.frame(edgeDatCut[,4])
  #   #   names(nodesformatted) <- names(edgesformatted)
  #   #   data.edit3 <- rbind(nodesformatted, edgesformatted)
  #   #   
  #   #  
  #   #  #write the edited policy graph back to text file and plot as before
  #   #  fileout <- paste(substr(policyGraphFileName,1,nchar(policyGraphFileName)-4), "Edited.dot",sep="")
  #   #  
  #   #  write.table(data.edit3, file= fileout,row.names= FALSE, col.names= FALSE, quote=FALSE)
  #   #  plotdf.policyGr2 <- grViz(fileout, engine = "dot")
  #   #  
  #   #  imgNameSVG2 <- paste('./pomdp_solved/polGraph_depth_', maxDepthPolgraph,'_precision_',precision,"Edited.svg", sep="")
  #   #  plotdf.policyGr2 %>% export_svg %>% charToRaw %>% rsvg_svg(imgNameSVG2)
  #    
  #  })
  

}
###########################################
#call Shiny app#
shinyApp(ui = ui, server = server)
