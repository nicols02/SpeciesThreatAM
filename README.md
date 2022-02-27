# SpeciesThreatAM: A Shiny implementation of the Threat-Species MOMDP
R code for the threat-species adaptive management Shiny App. The app is designed to help users interact with an optimal adaptive management problem for the management of threatened species. 

This document serves as a user manual for the Shiny implementation of the threat-species MOMDP described in “A general optimal adaptive management approach for demonstrating effectiveness in threatened species management” (Nicol et al, 2021). The app is an interactive user interface for eliciting the necessary parameters, solving the MOMDP and exploring the solution via simulation.

Note that the app can be accessed online using any OS via: https://jonathan-ferrer.shinyapps.io/SpeciesThreatAM/. The code behind the online app are included in this repo and are optimised for Windows.
The installation instructions included in this document are also designed for Windows users.

# Licence
The code is licensed using the MIT open source licence. See licence information available on github. Basically this is free to use and modify with attribution. No warranty or liability is given for the code.

# Installation:
NOTE THAT INSTALLATION IS NOT RECOMMENDED UNLESS YOU WANT TO MODIFY THE CODE. TO SIMPLY USE THE APP, visit https://jonathan-ferrer.shinyapps.io/SpeciesThreatAM/.

The following documentation only works for Windows users. For users with other OS, we recommend using the online app or contacing the authors for help with modifying the code to run on your system. Linux users will find installation easy, but there are a couple of lines 
in the R code that will need to be modified to ensure the app runs smoothly. We have not tested the app with Mac.

The app is written in the R programming language (version 4.0.2; note that testing on earlier R versions created warnings and issues rendering the plots in the app). It has been tested using RStudio (version 1.3.1093) on Windows 10 version 20H2. We recommend using RStudio to run the app, since it has features such as progress messages that will be printed to the console. 
The app is available from github at: https://github.com/nicols02/SpeciesThreatAM.git. Source code can be obtained from this URL by cloning the repository to a directory on your local system.  
You will need to install a Linux distribution to ‘make’ the sarsop file. We recommend using WSL2, which is a Windows supported tool for running Linux distributions through your Windows 10 machine. To install WSL2, follow the instructions at: 
https://docs.microsoft.com/en-us/windows/wsl/install-win10. 
Note that if you choose a software other than WSL (e.g. Cygwin) then you will need to manually update the code in the app that calls sarsop (line 405-6). The syntax for use with Cygwin is included at these lines but is commented out in the code.

After installing WSL, open Linux (i.e. Ubuntu) and run “sudo apt update && sudo apt upgrade” to update your Ubuntu packages to the latest version.
Once you have installed WSL and a Linux distribution (we used Ubuntu), you need to navigate to the folder where you stored sarsop. To access your C:, you need to open the Ubuntu drive and type “cd /mnt/c/”. From here it should be straightforward to cd (i.e. change directory) to the folder where you stored sarsop and locate the sarsop/src folder. For example, my default location was: 

“cd /mnt/c/Users/<your_username>/Documents/SpeciesThreatAM/sarsop/src”.
Next you’ll need to ‘make’ the sarsop files so that you can run sarsop. You’ll need to add the ‘make’ command to Ubuntu, since you only installed a minimal Linux distribution. To do this, type the following into the Ubuntu console:
“sudo apt-get install build-essential” 
You’ll need to enter your password that you created when you installed and first launched Ubuntu. Build-essential gives you a suite of important functions needed by sarsop including make and gcc.
Once you’ve added the make command and are in the sarsop/src directory, you can simply type “make”.
A good troubleshooting step here is to check that sarsop is installed. From Ubuntu, try to solve an example problem by entering the following into the console:
“./pomdpsol ../examples/POMDPX/Tiger.pomdpx”
You should see sarsop solve a toy problem on the screen and a policy file will be generated to "out.policy" by default (see https://github.com/AdaCompNUS/sarsop  for more information).
If this works, then sarsop is installed correctly. 

You can also check that WSL2 is working in the Windows command window. To do this, open a windows cmd prompt (Click the windows icon and type “cmd”). Navigate to the sarsop/src directory and type:
“wsl ./pomdpsol ../examples/POMDPX/Tiger.pomdpx”
This should solve the same problem as above, but via the Windows command prompt. If this works, then sarsop should be ready to run in the Shiny app.

Note that the Shiny app will not run from a URL (e.g. shiny::runGitHub(“SpeciesThreatAM”, “nicols02”, ref= “main”)) because it has a predefined folder structure that reads and writes files as you step through the process of solving the MOMDP.
## Running from RStudio:
The Shiny app is stored in a file called “app.R”. To run the app, place the files into an app directory in your working directory. First, include the shiny library by typing “library(“shiny”) into the console. You can then launch the app in R using either runApp("app.R"), or by opening the app file in RStudio and clicking “Run App” in the top right hand corner of the scripting window.

“app.R” sources three other files that contain functions used by the app. These are called “generate_transition_matrices.R”, “sarsop_parse_Shiny.R”, and “alpha_min_fast.R”. These should be sourced automatically by the app, but you’ll need to be sure that were download from the git repository and that they’re stored in the same directory as the app. The packages used by app.R should be automatically downloaded and sourced if you don’t already have them installed—this may take a few moments the first time that the app is run and progress will be documented in the RStudio console window.

## Notes on additional software:
The app calls other software, in particular the SARSOP MOMDP solver (original version available from https://github.com/AdaCompNUS/sarsop). If installed directly from source, SARSOP requires a linux environment (WSL2 is recommended if you are on Windows—see text above). However, an unpacked version of SARSOP is packaged with the app, so there should be no need to install it from its original location. We made minor modifications to the SARSOP code (specifically, our implementation stores the sampled belief vectors in a file for use in alpha-min-fast), so we recommend using the version that comes with our app.
As well as a modified version of SARSOP the alpha-min-fast algorithm uses the lp_solve program (http://lpsolve.sourceforge.net/5.5/). GraphViz (https://graphviz.org/) may be useful for viewing policy graphs, but we used the R package DiagrammeR to visualise .dot files produced by SARSOP and alpha-min-fast. DiagrammeR will automatically install and load when the app is run.

# Using the app

##  1.	Simulation and setup page
After launching the app, you should see a two-column layout (Figure 1). The left sidebar contains information that is required to generate the POMDP solution. There are default values in each box but these can be edited as required to solve different problems. The right sidebar may take a moment to load. It shows simulations of the system given the parameters on the left. As you update the values in the sidebar, the simulations will update automatically. Users can use the simulations to see the trajectory of the species and the threat. The visualisations provide a sanity check on the values entered into the elicitation boxes by allowing the users to see the impacts of the values they enter on the long-term behaviour of the system. If an optimal solution has previously been generated then this will be plotted on the graph too.
 ![Figure 1](/readme_images/Picture1.png)
Figure 1: The Simulate tab. This is the view that you should see when you first open the app.

Below we provide information on each of the fields in the left sidebar. Acceptable values for each field are included in the brackets following the description.
