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

# Windows
pwd.drop <- "D:/"
pwd.git  <- "C:/Users/Jon/Documents/R/"

# # Mac
# pwd.drop <- "/Users/john/"
# pwd.git  <- "/Users/john/Documents/"

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

# Remove temporary path objects
remove(pwd.drop, pwd.git)

# Set working directory
setwd(path$work)


# 1.2 Functions -----------------------------------------------------------

# List of functions used in this script to be loaded here
flst <- file.path(path$fun, c("wtRadius.R",
                              "eNPV.R",
                              "fcap.R",
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

# Use read.csv to read simulation data. Assuming that all the *.csv files have
# the same time index, create one data.frame out of all the data
data <- data.frame(time =  read.csv(file.path(path$raw, "sample oil.csv"))[,1],
                   coil =  read.csv(file.path(path$raw, "sample oil.csv"))[,2],
                   power = read.csv(file.path(path$raw, "sample power.csv"))[,2],
                   NER =   read.csv(file.path(path$raw, "sample NER.csv"))[,2])

# Unit conversions: time from seconds to days
data$time <- data$time/3600/24



# Loop --------------------------------------------------------------------

# Just take first row for now, code in loop for all LHS rows later
j <- 1


# x.x Drilling ------------------------------------------------------------

# Calculate well segment lengths
wellL <- data.frame(turn = wtRadius(t = uopt$wellDesign$turnrate,
                                         p = uopt$wellDesign$pipelength,
                                         a = uopt$wellDesign$angle),
                         total = uopt$parR$totalL[j])
wellL$stem <- uopt$wellDesign$TVD-wellL$turn
wellL$prod <- uopt$wellDesign$totalL-(wellL$stem+wellL$turn)

# Calculate well drilling schedule
drillsched <- rep(c(rep(0, uopt$parR$tDrill[j]-1), uopt$nrig), ceiling(uopt$nwell/uopt$nrig))

# If too many wells were drilled in last time step
if (sum(drillsched) > uopt$nwell) {

  # Replace last entry with correct number of wells
  drillsched[length(drillsched)] <- uopt$nwell-sum(drillsched[1:(length(drillsched)-1)])
}

# Design and construction time
tconstr <- ifelse(length(drillsched) >= uopt$tconstr.min, length(drillsched), uopt$tconstr.min)
tdesign <- round(tconstr/3)

# Calculate capital cost for wells
capwell <- drillsched*uopt$parR$well.cap[j]


# Scale and Fit Base Data ------------------------------------

# Scale data
data$coil <-  data$coil*(wellL$prod/uopt$base.prod)
data$power <- data$power*(wellL$prod/uopt$base.prod)

# Fit each oil/power data with approximation functions
fcoil <-  approxfun(x = data$time, y = data$coil)
fpower <- approxfun(x = data$time, y = data$power)


# Heating -----------------------------------------------------------------

# Heater capital cost
capheat <- uopt$nwell*uopt$heatcost*(wellL$prod/uopt$heatBlength)^1 # scale linearly?

# Heating operating cost calculation
# Step 1: Calculate energy demand history
E <- NULL
for (i in 1:(nrow(data)-1)) {

  # Itegrate heating demand
  E <- c(E, integrate(fpower, data$time[i], data$time[i+1])$value)
}

# Step 2: Multiply E by electricity price to get operating cost (also
# concatonate in an additional zero to fix length of E)
opheat <- c(0,E)*uopt$ep


# Oil Production ----------------------------------------------------------

# Calculate oil production on: (1) daily basis and (2) adjust to bbl from m^3
oil <- c(0, diff(fcoil(1:max(data$time))))*6.2898


# Production, separation, and storage -------------------------------------

# Capital cost formula:
#
# capPSS = base(func. of length)*(new max oil/base max oil)^0.6*nwell
#
capPSS <- uopt$fcapPSS(wellL$total)*(max(oil)/uopt$nwell/uopt$capPSSbase)^0.6*uopt$nwell

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


# DCF Analysis ------------------------------------------------------------

# Build timing data.frame with all terms in CF equation
model <- data.frame(time = 1:(tdesign+tconstr+length(oil)))

# Add capital costs column
model$CTDC <- c(rep(ccs$TDC/(tdesign+tconstr), ((tdesign+tconstr))), rep(0, length())

# Build discount factor vector
df <- NULL
n <- 0
while(length(df) < nrow(model)) {

  df <- c(df, rep(1/((1+uopt$parR$IRR[1])^n), 365)) # replace 1 with i in for-loop
  n <- n+1
}
model$df <- df[1:nrow(model)]

model$capwell <- c(capwell, rep(0, nrow(model)-length(capwell)))
model$capheat <- c(rep(0, nrow(model)-length(oil)), capheat)
model$oil <-     c(rep(0, nrow(model)-length(oil)), oil)


NPV <- function(op) {
  NPV <- with(model, sum(df*(oil*op-capwell)))
  return(NPV)
}

# Oil Supply Price
oilSP <- uniroot(NPV, lower = 0, upper = 10e3)$root

