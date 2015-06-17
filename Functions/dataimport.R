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

# Load XLConnect library (for reading in # of wells from Excel spreadsheet)
library(XLConnect)

# Define scenario number vector
nscen <- c(2:51, 53:88)

# Get initial energy/oil objects
denergy <- read.csv(file.path(paste(path$raw, "/Processing", sep = ""),
                             paste("instPower-", 1, ".csv", sep = "")))
dcoil <- read.csv(file.path(paste(path$raw, "/Processing", sep = ""),
                           paste("oil-", 1, ".csv", sep = "")))

# Start name vectors
ename <- c("time", "e1")
oname <- c("time", "o1")

# For each scenario number
for (i in nscen) {

  # Read *.csv i
  etemp <- read.csv(file.path(paste(path$raw, "/Processing", sep = ""),
                              paste("instPower-", i, ".csv", sep = "")))
  otemp <- read.csv(file.path(paste(path$raw, "/Processing", sep = ""),
                              paste("oil-", i, ".csv", sep = "")))

  # Add entry to name vector
  ename <- c(ename, paste("e", i, sep = ""))
  oname <- c(oname, paste("o", i, sep = ""))

  # Add column to data result
  denergy <- cbind(denergy, etemp[,2])
  dcoil <- cbind(dcoil, otemp[,2])
}

# Name change
names(denergy) <- ename
names(dcoil) <-   oname

# Unit conversions: time from seconds to days
denergy$time <- denergy$time/3600/24
dcoil$time <-   dcoil$time/3600/24

# Unit conversion: m^3 oil to bbl oil
dcoil[,2:ncol(dcoil)] <- dcoil[,2:ncol(dcoil)]*6.2898

# Get well counts from "shale87designs.xlsx"
nwell <- readWorksheetFromFile(file     = paste(path$raw, "/shale87designs.xlsx", sep = ""),
                               sheet    = "Sheet1",
                               startRow = 4,
                               startCol = 0,
                               endRow   = 0,
                               endCol   = 0,
                               header   = TRUE)$Counted.Number.of.Wells

# Get NER values from same spreadsheet
NER <- readWorksheetFromFile(file     = paste(path$raw, "/shale87designs.xlsx", sep = ""),
                             sheet    = "Sheet1",
                             startRow = 4,
                             startCol = 0,
                             endRow   = 0,
                             endCol   = 0,
                             header   = TRUE)$NER..t.boe


# Export result
save(denergy, dcoil, nwell, NER, file = file.path(path$data, "dataImport.rda"))
