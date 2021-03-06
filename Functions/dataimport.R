# Function Info -----------------------------------------------------------
# Name:       dataimport.R (Data Import Script)
# Author(s):  Jon Wilkey
# Contact:    jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# NA since runs as a script


# Outputs -----------------------------------------------------------------

# dataImport.rda - saved data file with denergy, dcoil, and nwell objects


# Description -------------------------------------------------------------

# This function reads in the energy and oil production information contained in
# Michal's simulation results export from a series of *.csv files, as well as
# the number of wells for each simulation (recorded in an Excel spreadsheet).
# After reading in all of the results, the data is exported by saving to disk.


# Script ------------------------------------------------------------------

# Define scenario number vector
nscen <- c(2:242)

# Get initial energy/oil objects
denergy <- read.csv(file.path(path$raw, paste("energy-", 1, ".csv", sep = "")))[,c(1,3)]
dcoil <-   read.csv(file.path(path$raw, paste("prod-",   1, ".csv", sep = "")))[,c(1,5)]

# Start name vectors
ename <- c("time", "e1")
oname <- c("time", "o1")

# For each scenario number
for (i in nscen) {

  # Read *.csv i
  etemp <- read.csv(file.path(path$raw, paste("energy-", i, ".csv", sep = "")))[,3]
  otemp <- read.csv(file.path(path$raw, paste("prod-",   i, ".csv", sep = "")))[,5]

  # Add entry to name vector
  ename <- c(ename, paste("e", i, sep = ""))
  oname <- c(oname, paste("o", i, sep = ""))

  # Add column to data result
  denergy <- cbind(denergy, etemp)
  dcoil <- cbind(dcoil, otemp)
}

# Name change
names(denergy) <- ename
names(dcoil) <-   oname

# Unit conversions: time from seconds to days
denergy$time <- denergy$time/3600/24
dcoil$time <-   dcoil$time/3600/24

# Get design table
DT <- read.csv(file.path(path$raw, "242FinalDesignTable.csv"))

# Rename
names(DT) <- c("design", "TE", "mKerogen", "hspace", "vspace", "angle", "loc", "radius", "nrow", "nwell", "normNER")

# Export result
save(denergy, dcoil, DT, file = file.path(path$data, "dataImport.rda"))


# Well Data ---------------------------------------------------------------

# Read in wellData.csv
welldata <- read.csv(file.path(path$raw, "wellData.csv"))

# Change names
names(welldata) <- c("api",
                     "dSpud",
                     "dDepth",
                     "dCased",
                     "dCompl",
                     "tdSpudCompl",
                     "depth",
                     "vertLength",
                     "latLength",
                     "field",
                     "county",
                     "wellType",
                     "operator",
                     "nomDrillC",
                     "nomCaseC",
                     "nomComplC",
                     "cpi",
                     "adDrillC",
                     "adComplC")

# Change variable types
welldata$dSpud <-  as.Date(welldata$dSpud)
welldata$dDepth <- as.Date(welldata$dDepth)
welldata$dCased <- as.Date(welldata$dCased)
welldata$dCompl <- as.Date(welldata$dCompl)

# Plotting function
tempPlot <- function(a, lab.x, lab.plot) {

  # PDF plot
  pdf(file.path(path$plot, paste(lab.plot,".pdf", sep = "")))
  # cdfcomp(temp, horizontals = F, addlegend = F, datapch = 1, xlab = lab.x, ylab = "Cumulative Probability", main = "")
  # legend("bottomright", c("Data", "Fit"), pch = c(1, NA), lty = c(NA, 1), col = c("black","red"))
  denscomp(temp, probability = T, addlegend = F, xlab = lab.x, ylab = "Probability Density", main = "")
  rug(jitter(temp$data))
  dev.off()

  # EPS plot
  setEPS()
  postscript(file.path(path$plot, paste(lab.plot,".eps", sep = "")))
  # cdfcomp(temp, horizontals = F, addlegend = F, datapch = 1, xlab = lab.x, ylab = "Cumulative Probability", main = "")
  # legend("bottomright", c("Data", "Fit"), pch = c(1, NA), lty = c(NA, 1), col = c("black","red"))
  denscomp(temp, probability = T, addlegend = F, xlab = lab.x, ylab = "Probability Density", main = "")
  rug(jitter(temp$data))
  dev.off()
}

# Calculate drilling time
welldata$tdSpudTD <- as.numeric(difftime(welldata$dDepth, welldata$dSpud, units = "days"))

# Fit drilling time data parameters
temp <- fitdist(welldata$tdSpudTD[which(!is.na(welldata$tdSpudTD))], "lnorm")
input.par <- data.frame(par1 = coef(temp)[1],
                        par2 = coef(temp)[2])
tempPlot(temp, "Well Drilling Time (days)", "Figure 11-3 Well drilling time fitted CDF")

# Get drilling costs parameters
temp <- fitdist(welldata$adDrillC[which(!is.na(welldata$adDrillC))]/1e6, "norm")
input.par <- rbind(input.par,
                   data.frame(par1 = coef(temp)[1],
                              par2 = coef(temp)[2]))
tempPlot(temp, "Drilling Cost ($1e6)", "Figure 11-4 Drilling cost fitted CDF")

# Get completion costs parameters
temp <- fitdist(welldata$adComplC[which(!is.na(welldata$adComplC))], "lnorm")
input.par <- rbind(input.par,
                   data.frame(par1 = coef(temp)[1],
                              par2 = coef(temp)[2]))
tempPlot(temp, "Completion Cost ($1e6)", "Figure 11-5 Completion cost fitted CDF")

# Get well length parameters
temp <- fitdist(welldata$depth[welldata$depth > 0]/1e3, "lnorm")
input.par <- rbind(input.par,
                   data.frame(par1 = coef(temp)[1],
                              par2 = coef(temp)[2]))
tempPlot(temp, "Total Well Length (1e3 ft)", "Figure 11-2 Total well length fitted CDF")


# Get gas price parameters

# Load data (eia.hp)
load("C:/Users/jonwi/Dropbox/CLEAR/DOGM Data/Prepared Data/EIAprices_v8.rda")

# Select only the last five years of data
gp <- eia.hp$GP[(nrow(eia.hp)-59):nrow(eia.hp)]

# Fit data
temp <- fitdist(gp, "norm")
input.par <- rbind(input.par,
                   data.frame(par1 = coef(temp)[1],
                              par2 = coef(temp)[2]))
tempPlot(temp, "Natural Gas Price ($ / MCF)", "Figure 11-7 Natural gas prices fitted CDF")

# Recovery fraction
temp <- c(0.8, 0.9)
input.par <- rbind(input.par,
                   data.frame(par1 = mean(temp),
                              par2 = sd(temp)))

# Gas fraction
temp <- c(0.25, 0.33)
input.par <- rbind(input.par,
                   data.frame(par1 = mean(temp),
                              par2 = sd(temp)))

# Change row.names
row.names(input.par) <- c("tDrill", "drillC", "complC", "depth", "gp", "rec", "xg")

# Save out result
save(input.par, file = file.path(path$data, "inputParamValues.rda"))

