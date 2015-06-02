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


# X.X Drilling Cost Fitting -----------------------------------------------

# Update drilling and completion costs?
if (uopt$update.hdrill == T) {source(file.path(path$fun, "hdrill cost.R"))}

# Update drilling time?
if (uopt$update.tDrill == T) {source(file.path(path$fun, "tDrill.R"))}

# Load results
load(file.path(path$data, "hdrill.rda"))
load(file.path(path$data, "tDrill.rda"))


# 2.0 Read in simulation data ---------------------------------------------

# Use read.csv to read simulation data. Assuming that all the *.csv files have
# the same time index, create one data.frame out of all the data
data <- data.frame(time =  read.csv(file.path(path$raw, "sample oil.csv"))[,1],
                   coil =  read.csv(file.path(path$raw, "sample oil.csv"))[,2],
                   power = read.csv(file.path(path$raw, "sample power.csv"))[,2],
                   NER =   read.csv(file.path(path$raw, "sample NER.csv"))[,2])

# Unit conversions: time from seconds to days
data$time <- data$time/3600/24

# Fit each oil/power data with approximation functions
fcoil <-  approxfun(x = data$time, y = data$coil)
fpower <- approxfun(x = data$time, y = data$power)


# x.x Drilling ------------------------------------------------------------

# Calculate well segment lengths
wellLength <- data.frame(turn = wtRadius(t = uopt$wellDesign$turnrate,
                                         p = uopt$wellDesign$pipelength,
                                         a = uopt$wellDesign$angle),
                         total = uopt$wellDesign$totalL)
wellLength$stem <- uopt$wellDesign$TVD-wellLength$turn
wellLength$prod <- uopt$wellDesign$totalL-(wellLength$stem+wellLength$turn)

# Calculate well drilling schedule
drillsched <- rep(c(rep(0, tDrill-1), uopt$nrig), ceiling(uopt$nwell/uopt$nrig))

# If too many wells were drilled in last time step
if (sum(drillsched) > uopt$nwell) {

  # Replace last entry with correct number of wells
  drillsched[length(drillsched)] <- uopt$nwell-sum(drillsched[1:(length(drillsched)-1)])
}

# Calculate capital cost for wells
capwell <- drillsched*uopt$wcost


# Heating -----------------------------------------------------------------

# Heater capital cost
capheater <- uopt$heatcost*(wellLength$total/uopt$heatBlength)*uopt$nwell


# Oil Production ----------------------------------------------------------

# Calculate oil production, adjust (1) to bbl from m^3, and (2) from simulated
# length to production length
oil <- c(0, diff(fcoil(1:max(data$time))))*6.2898*wellLength$prod/(5*3.28084)


# Production, separation, and storage -------------------------------------

capPSS <- uopt$fcapPSS(wellLength$total)*uopt$nwell
opPSS <- uopt$fopPSS(wellLength$total)*uopt$nwell


# DCF Analysis ------------------------------------------------------------

# Build timing data.frame with all terms in CF equation
model <-         data.frame(time = seq(1:(length(capwell)+length(oil))))

# Build discount factor vector
df <- NULL
n <- 0
while(length(df) < nrow(model)) {

  df <- c(df, rep(1/((1+uopt$IRR)^n), 365))
  n <- n+1
}
model$df <- df[1:nrow(model)]

model$capwell <- c(capwell, rep(0, nrow(model)-length(capwell)))
model$oil <-     c(rep(0, nrow(model)-length(oil)), oil)

NPV <- function(op) {
  NPV <- with(model, sum(df*(oil*op-capwell)))
  return(NPV)
}

# Oil Supply Price
oilSP <- uniroot(NPV, lower = 0, upper = 10e3)$root

