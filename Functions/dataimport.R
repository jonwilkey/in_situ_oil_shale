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
names(DT) <- c("design", "TE", "mKerogen", "nwell", "normNER")

# Export result
save(denergy, dcoil, DT, file = file.path(path$data, "dataImport.rda"))
