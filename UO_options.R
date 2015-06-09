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


# Parameter Space Generation ----------------------------------------------

# Generate latin hypercube sample
parLHS <- randomLHS(n = 10, k = 6)

# Calcualte parameter values from LHS
uopt$parR <- data.frame(tDrill =   c(112,   qunif(parLHS[,1], min = 72,  max = 143)),
                        well.cap = c(3.5e6, qunif(parLHS[,2], min = 1.9e6,  max = 6.6e6)),
                        totalL =   c(10450, qunif(parLHS[,3], min = 7930, max = 11360)),
                        xg =       c(0.2,   qunif(parLHS[,4], min = 0,    max = 0.4)),
                        gp =       c(3,     qunif(parLHS[,5], min = 2, max = 6)),
                        IRR =      c(0.15,  qunif(parLHS[,6], min = 0.1,  max = 0.4)))

#fuel.pc =    qunif(parLHS[,6],  min = 0.5,  max = 1.5),
#heat.cap =   qunif(parLHS[,3],  min = 0.5,  max = 1.5),
# heat.eff =   qunif(parLHS[,3],  min = 0.48, max = 1.00),
#elec.gf =    qunif(parLHS[,4],  min = 0,    max = 1),
#elec.pc =    qunif(parLHS[,5],  min = 0.5,  max = 1.5),
#fuel.hhv =   qunif(parLHS[,7],  min = 912,  max = 2012),
#xc =         qunif(parLHS[,10], min = 0,    max = 0.4),
# pss.cap =    qunif(parLHS[,11], min = 0.5,  max = 1.5),
# pss.op =     qunif(parLHS[,8], min = 0.5,  max = 1.5),
#hub.length = qunif(parLHS[,13], min = 0.5,  max = 1.5),

# Remove LHS
remove(parLHS)


# Update Functions --------------------------------------------------------

uopt$update.hdrill <- F # Well drilling & completion costs
uopt$update.tDrill <- F # Well drilling time


# Index Values ------------------------------------------------------------

# CPI value for inflation adjustment
uopt$cpi <- 236.736


# Well Options ------------------------------------------------------------

# Number of wells
uopt$nwell <- 133

# Base well horizontal production length for simulation (in ft)
uopt$base.prod <- 5*3.28084

# Well Design
uopt$wellDesign <- data.frame(turnrate =   3,    # Turning rate (deg./pipe)
                              pipelength = 30,   # Pipe segment length (ft)
                              angle =      90,   # Total turn angle (deg.)
                              TVD =        2500, # True vertical depth (ft)
                              totalL =     11e3) # Total length (ft) - NOTE cannot exceed 5 mi (length of deposit)

# Number of rigs
uopt$nrig <- 14


# Heater Options ----------------------------------------------------------

# Base design
uopt$heatcost <-    90e3*(uopt$cpi/218.056) # Base cost, inflation adjusted from 2010
uopt$heatBlength <- 1936                    # Base length (ft)
uopt$gencost <-     1100*(uopt$cpi/232.957) # Base cost per kW capacity, inflation adjusted from 2013


# Production, separation, and storage Options -----------------------------

# Capital and operating cost approximation functions - input cost data here from
# here:
# http://www.eia.gov/pub/oil_gas/natural_gas/data_publications/cost_indices_equipment_production/current/coststudy.html
uopt$fcapPSS <- approxfun(x = c(2e3, 4e3, 8e3, 12e3),
                          y = c(1178400, 1633700, 2584400, 3033100)*(uopt$cpi/214.537)/10)
uopt$fopPSS <- approxfun(x = c(2e3, 4e3, 8e3, 12e3),
                          y = c(255700, 285400, 394200, 555300)*(uopt$cpi/214.537)/10)

# Base capacity of PSS (in BOPD)
uopt$capPSSbase <- 200
uopt$opPSSbase <-  100


# Utilities ---------------------------------------------------------------

# Utah 2014 annual average industrial electricity price ($/kWh)
# Data from EIA Average retail price of electricity to ultimate customers: http://www.eia.gov/electricity/data.cfm#sales
uopt$ep <- 0.0607

# Gas Price (2014 USD per MCF)
uopt$gp <- mean(5.62,6.57,6.35,5.78,5.67,5.39,5.35,4.88,4.95,4.96,4.93,5.53)

# Electrity infrastructure
uopt$eline <-   425e3*(uopt$cpi/232.957) # Line cost ($/mi)
uopt$eswitch <- 10e3*(uopt$cpi/232.957)  # Switching gear and tap ($/mi)

# Distance to nearest utility hub (mi)
uopt$hubL <- 50



# Finance and Econ Terms --------------------------------------------------

# Minimum construction time (in days)
uopt$tconstr.min <- 365*9/12




