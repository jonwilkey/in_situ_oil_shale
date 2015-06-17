# Script Info -------------------------------------------------------------
# Name:      UO_main.R (Unconventional Oil and Simulation Main Script)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com

# Description -------------------------------------------------------------

# This script creates runs the economic analysis for a set of in situ oil shale
# simualtions prepared for ICSE by Michal Hradisky. Structure is as follows:
#
# 1. Sets working directory, libraries, and options
# 2. BLAH

# 1.1 Paths ---------------------------------------------------------------

# Predefine list object "path" for holding directory path listings
path <- NULL

# Path switch - uncomment and/or replace with the path directory for your local
# copy of the Git repository and Dropbox files.

pwd.drop <- "D:/"                       # Windows
pwd.git  <- "C:/Users/Jon/Documents/R/"
# pwd.drop <- "/Users/john/"              # Mac
# pwd.git  <- "/Users/john/Documents/"
# pwd.drop <- "~/"                        # Linux
# pwd.git  <- "~/Documents/R Projects/"

# Define paths.
# "raw"  is raw data (*.dbf files from DOGM, *.csv files, etc.).
# "data" is prepared data files (typically *.rda).
# "look" is lookup tables.
# "plot" is the directory for saving plot *.pdf files.
# "work" is the working directory where main.R and IO_options.R are located.
# "fun"  is the directory for all *.R functions.
path$raw   <- paste(pwd.drop, "Dropbox/Oil Shale/Raw Data", sep = "")
path$data  <- paste(pwd.drop, "Dropbox/Oil Shale/Prepared Data", sep = "")
path$plot  <- paste(pwd.drop, "Dropbox/Oil Shale/Plots", sep = "")
path$work  <- paste(pwd.git,  "oilshale/", sep = "")
path$fun   <- paste(pwd.git,  "oilshale/Functions", sep = "")
path$BCfig <- paste(pwd.drop, "Dropbox/Oil Shale/Book Chapter/Figures", sep = "")
path$Cdata <- paste(pwd.drop, "Dropbox/CLEAR/DOGM Data/Prepared Data", sep = "")

# Remove temporary path objects
remove(pwd.drop, pwd.git)

# Set working directory
setwd(path$work)


# 1.2 Functions -----------------------------------------------------------

# List of functions used in this script to be loaded here
flst <- file.path(path$fun, c("wtRadius.R",
                              "eNPV.R",
                              "fcap.R",
                              "ffoc.R",
                              "stax.R",
                              "fNPV.R",
                              "clipboard.R"))

# Load each function in list then remove temporary file list variables
for (f in flst) source(f); remove(f, flst)


# 1.3 Libraries -----------------------------------------------------------

library(zoo)
library(sqldf)
library(lhs)


# 1.4 Options -------------------------------------------------------------

# Don't want strings 'typed' as factors but as characters
options(stringsAsFactors=FALSE)

# Run script "IO_options.R" to load user defined input/output options
source("UO_options.R")


# 2.0 Read in simulation data ---------------------------------------------

# Load from dataimport.R script
load(file.path(path$data, "dataImport.rda"))

# # Sample data set
# # Use read.csv to read simulation data. Assuming that all the *.csv files have
# # the same time index, create one data.frame out of all the data
# data <- data.frame(time =  read.csv(file.path(path$raw, "sample oil.csv"))[,1],
#                    coil =  read.csv(file.path(path$raw, "sample oil.csv"))[,2],
#                    power = read.csv(file.path(path$raw, "sample power.csv"))[,2],
#                    NER =   read.csv(file.path(path$raw, "sample NER.csv"))[,2])
#
# # Unit conversions: time from seconds to days
# data$time <- data$time/3600/24

# Concatonate parameter space with nwell vector
temp1 <- data.frame(index = rep(1, times = length(nwell)), nwell, NER)
temp2 <- data.frame(index = rep(1, times = nrow(uopt$parR)), uopt$parR)
parR <- merge(x = temp1, y = temp2, all = T); remove(temp1, temp2)
parR <- parR[,-1]


# Loop --------------------------------------------------------------------

# Predefine results space
oilSP <- rep(0, times = nrow(parR))
CPFB <-  oilSP
TCI <-   oilSP
Toil <-  oilSP

# For each set of input parameter picks j
for (j in 1:nrow(parR)) {

  # x.x Drilling ------------------------------------------------------------

  # Calculate well segment lengths
  wellL <- data.frame(turn = wtRadius(t = uopt$wellDesign$turnrate,
                                      p = uopt$wellDesign$pipelength,
                                      a = uopt$wellDesign$angle),
                      total = parR$totalL[j])
  wellL$stem <- uopt$wellDesign$TVD-wellL$turn
  wellL$prod <- wellL$total-(wellL$stem+wellL$turn)

  # Calculate well drilling schedule
  drillsched <- rep(c(rep(0, parR$tDrill[j]-1), uopt$nrig), ceiling(parR$nwell[j]/uopt$nrig))

  # If too many wells were drilled in last time step
  if (sum(drillsched) > parR$nwell[j]) {

    # Replace last entry with correct number of wells
    drillsched[length(drillsched)] <- parR$nwell[j]-sum(drillsched[1:(length(drillsched)-1)])
  }

  # Design and construction time
  tconstr <- ifelse(length(drillsched) >= uopt$tconstr.min, length(drillsched), uopt$tconstr.min)
  tdesign <- round(tconstr/3)

  # Calculate capital cost for wells
  capwell <- drillsched*parR$well.cap[j]


  # Scale and Fit Base Data ------------------------------------

  # Scale data
  coil <-  dcoil[,(ceiling(j/nrow(uopt$parR))+1)]*(wellL$prod/uopt$base.prod)
  power <- denergy[,(ceiling(j/nrow(uopt$parR))+1)]*(wellL$prod/uopt$base.prod)

  # Fit each oil/power data with approximation functions
  fcoil <-  approxfun(x = dcoil$time, y = coil, rule = 2)
  fpower <- approxfun(x = denergy$time, y = power)


  # Heating -----------------------------------------------------------------

  # Heater capital cost
  capheat <- parR$nwell[j]*uopt$heatcost*(wellL$prod/uopt$heatBlength)^1 # scale linearly?

  # Heating operating cost calculation
  # Step 1: Calculate energy demand history
  E <- NULL
  for (k in 1:(max(denergy$time)-1)) {

    # Itegrate heating demand
    E <- c(E, integrate(fpower, k, k+1)$value)
  }

  # Step 2: Multiply E by electricity price to get operating cost (also
  # concatonate in an additional zero to fix length of E)
  opheat <- c(0,E)*uopt$ep


  # Oil Production ----------------------------------------------------------

  # Calculate maximum potential oil production (xg = 0) on daily basis
  moil <- c(0, diff(fcoil(1:max(dcoil$time))))

  # Calculate actual oil production
  oil <- moil*(1-parR$xg[j])


  # Production, separation, and storage -------------------------------------

  # Capital cost formula:
  #
  # capPSS = base(func. of length)*(new max oil/base max oil)^0.6*nwell
  #
  capPSS <- uopt$fcapPSS(wellL$total)*(max(oil)/parR$nwell[j]/uopt$capPSSbase)^0.6*parR$nwell[j]

  # Operating cost formula:
  #
  # opPSS = (base cost/day/well as f(length))*(oil prod as f(time)/base oil prod)
  #
  # Since oil is total oil production from all wells, don't have to multiply by
  # number of wells in simulation.
  opPSS <- (uopt$fopPSS(wellL$total)/365)*(oil/uopt$opPSSbase)


  # Utility Lines -----------------------------------------------------------

  # Capital cost of all utilities
  capU <- with(uopt, hubL*(eline+eswitch))


  # Capital Costing ---------------------------------------------------------

  ccs <- fcap(capheat, capPSS, capU, oil, capwell)


  # Fixed Costs -------------------------------------------------------------

  opF <- ffoc(Nopers = uopt$Nopers, CTDC = ccs$TDC, CTPI = ccs$TPI)


  # DCF Analysis ------------------------------------------------------------

  # Make model data.frame
  model <- data.frame(CTDC = c(rep(x = -ccs$TDC/(tdesign+tconstr), times = tdesign+tconstr),
                               rep(x = 0,                          times = length(oil))))

  # Design Capital
  model$CD <- c(rep(x = -(ccs$Land+ccs$Permit)/tdesign, times = tdesign),
                rep(x = 0,                              times = tconstr+length(oil)))

  # Well Drilling and Completion Capital
  model$CWD <- c(rep(x = 0, times = tdesign),
                 -capwell,
                 rep(0,     times = nrow(model)-(tdesign+length(capwell))))

  # Startup Capital
  model$CSt <- c(rep(x = 0, times = tdesign+tconstr),
                 -(ccs$RIP+ccs$Start),
                 rep(x = 0, times = length(oil)-1))

  # Working Capital
  model$CWC <- c(rep(x = 0, times = tdesign+tconstr),
                 -ccs$WC,
                 rep(x = 0, times = length(oil)-2),
                 ccs$WC)

  # Gas production
  model$gasp <- c(rep(x = 0, times = tdesign+tconstr),
                  moil*uopt$convert.otg*parR$xg[j])

  # Gas sales
  model$gsale <- model$gasp*parR$gp[j]

  # Gas royalties
  model$rg <- -uopt$royalr*model$gsale

  # Gas severance taxes
  stg <- -stax(prod = model$gasp, ep = parR$gp[j], uopt$royalr, uopt$st.low, uopt$st.high, uopt$st.con, uopt$st.cut.o)

  # PSS operating costs
  model$opPSS <- c(rep(x = 0, times = tdesign+tconstr),
                   -opPSS)

  # Electricity/heating costs
  model$opheat <- c(rep(x = 0, times = tdesign+tconstr),
                    -opheat)

  # Fixed costs (labor, maintenance, property taxes, insurance)
  model$fixed <- c(rep(x = 0,        times = tdesign+tconstr),
                   rep(x = -opF/365, times = length(oil)))

  # Oil production
  model$oilp <- c(rep(x = 0, times = tdesign+tconstr),
                  oil)

  # Depreciation
  model$D <- c(rep(x = 0, times = tdesign+tconstr),
               -ccs$TDC/365*
                 uopt$fD[1:length(oil)]/
                 ((1+uopt$inf)^
                    floor(((tdesign+tconstr+1):(tdesign+tconstr+length(oil)))/365)))

  # Discount factor
  model$df <- 1/((1+parR$IRR[j])^floor((1:(tdesign+tconstr+length(oil)))/365))


  # Solve for oil price -----------------------------------------------------

  # Oil Supply Price
  oilSP[j] <- uniroot(NPV, lower = 0, upper = 1e7)$root


  # Save results ------------------------------------------------------------

  # Total capital cost
  Toil[j] <- sum(oil)
  TCI[j] <-  ccs$TCI
  CPFB[j] <- ccs$TCI/(Toil[j]/length(oil))
}

# ... and really save results
results <- data.frame(parR, oilSP, Toil, TCI, CPFB)
save(results, file = file.path(path$data, "UO_main Results v1.rda"))
