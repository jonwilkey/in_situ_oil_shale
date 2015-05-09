# Script Info -------------------------------------------------------------
# Name:      UO_options.R (Unconventional Oil and Simulation Options Script)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com

# Description -------------------------------------------------------------

# This script creates a list object named "uopt" that contains the options for
# all the inputs/outputs that control the execution of the main.R script. Review
# each input/output below and change as desired from their base values.


# 1.0 Global Options ------------------------------------------------------

# Define "uopt" list object - this must exist in order to set any other options
uopt <- NULL


# Parameter space ---------------------------------------------------------

uopt$IRR <- 0.4
uopt$cpi <- 236.736

# Well Options ------------------------------------------------------------

# Number of wells
uopt$nwell <- 133

# Well Design
uopt$wellDesign <- data.frame(turnrate = 3,    # Turning rate (deg./pipe)
                              pipelength = 30, # Pipe segment length (ft)
                              angle = 90,      # Total turn angle (deg.)
                              TVD = 2500,      # True vertical depth (ft)
                              totalL = 19553)  # Total length (ft) - NOTE cannot exceed 5 mi (length of deposit)

# Drill Time (in days)
uopt$drillTime <- as.numeric(difftime(as.Date("2013-05-30"), as.Date("2013-02-23"), units = "days"))

# Number of rigs
uopt$nrig <- 15

# Cost per well
uopt$wcost <- 11e6


# Heater Options ----------------------------------------------------------

# Base design
uopt$heatcost <-    90e3*(uopt$cpi/218.056) # Base cost, inflation adjusted from 2010
uopt$heatBlength <- 1936                    # Base length (ft)
uopt$geneff <-      0.46                    # Generator efficiency
uopt$gencost <-     1100*(uopt$cpi/232.957) # Base cost per kW capacity, inflation adjusted from 2013

