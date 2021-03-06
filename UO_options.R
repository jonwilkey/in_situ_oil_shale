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


# Version History ---------------------------------------------------------

# Version #
uopt$ver <- "v14"

# v11
# - What went into book chapter v6

# v12
# - Changed salary to $82,510/yr
# - Changed from uniform to fitted distributions for all terms
# - Increased LHS sample points from n = 512 to n = 2000
# - Used geneticLHS instead of optimumLHS to pick sample points

# v13
# - Adjusted parameter range, specifically removed casing costs from drilling
# costs, changed mean/sd for gas fraction

# v14
# - Caught error in how the length of the well turn segment was being calcualted


# One-time analysis functions ---------------------------------------------

# Uncomment and rerun if desired
# source(file.path(path$fun, "parDataAnalysis.R")) # Determines tDrill, well cost, well length, and gas price info
# source(file.path(path$fun, "dataimport.R"))      # Reads in simulation energy, oil production, and nwell data

# Parameter Space Generation ----------------------------------------------

# Generate latin hypercube sample - Uncomment first line for new sample,
# otherwise load saved one. Note - generating a new LHS set will take a long time.
# parLHS <- geneticLHS(n = 2e3, k = 7, pop = 10e3, gen = 10, pMut = 0.1)
# save(parLHS, file = file.path(path$data, "parLHS_genv1.rda"))
load(file.path(path$data, "parLHS_genv1.rda"))

# Load parameter shape values for calculated distributions
load(file.path(path$data, "inputParamValues.rda"))

# Calcualte parameter values from LHS. Refer to values in
# load(file.path(path$data, hdrill.rda)) as generated by "hdrill cost.R" for
# reference.
uopt$parR <- data.frame(LHS =       1:nrow(parLHS),                                                             # LHS scenario #
                        tDrill =    qlnorm(parLHS[,1], meanlog = input.par$par1[1], sdlog = input.par$par2[1]), # Time to drill and complete well (days)
                        well.cap =  qnorm(parLHS[,2],  mean =    input.par$par1[2], sd =    input.par$par2[2]), # Drilling capital cost
                        compl.cap = qlnorm(parLHS[,2], meanlog = input.par$par1[3], sdlog = input.par$par2[3]), # Completion cost, using the same paramter pick as drilling costs
                        totalL =    qlnorm(parLHS[,3], meanlog = input.par$par1[4], sdlog = input.par$par2[4]), # Total well length (ft)
                        xg =        qnorm(parLHS[,4],  mean =    input.par$par1[7], sd =    input.par$par2[7]), # Mass fraction gas
                        gp =        qnorm(parLHS[,5],  mean =    input.par$par1[5], sd =    input.par$par2[5]), # Gas price ($/MCF)
                        IRR =       qnorm(parLHS[,6],  mean =    0.15,              sd =    0.025),             # Internal rate of return
                        rec =       qnorm(parLHS[,7],  mean =    input.par$par1[6], sd =    input.par$par2[6])) # Recovery fraction

# Remove LHS
remove(parLHS, input.par)

# Round drilling time
uopt$parR$tDrill <- as.integer(round(uopt$parR$tDrill))

# Get points that are outside of bounds
ind <- with(uopt$parR, which(well.cap < 0 |    # Can't be zero cost
                               totalL < 2836 | # Can't be shorter than vertical and turn segments of well
                               xg < 0 |        # Gas fraction must be 0 <= xg <= 1
                               xg > 1 |
                               gp < 0 |        # No negative gas prices
                               IRR < 0 |       # Positive IRR
                               rec < 0 |       # Recovery fraction must be 0 <= rg <= 1
                               rec > 1))

# Drop points that are outside of bounds.
uopt$parR <- uopt$parR[-ind,]; remove(ind)


# Index Values ------------------------------------------------------------

# CPI value for inflation adjustment (average 2014 USD)
uopt$cpi <- 236.736


# Well Options ------------------------------------------------------------

# Base well horizontal production length for simulation (in ft)
uopt$base.prod <- 5*3.28084

# Well Design
uopt$wellDesign <- data.frame(turnrate =   3,    # Turning rate (deg./pipe)
                              pipelength = 30,   # Pipe segment length (ft)
                              angle =      90,   # Total turn angle (deg.)
                              TVD =        2500) # True vertical depth (ft)

# Number of rigs
uopt$nrig <- 14

# Well reclamation cost ($/well) inflation adjusted from 2009 USD
uopt$wellrec <- 27555*(uopt$cpi/214.537)

# Heater (x) to producer (y) ratio where x is number below
uopt$HPratio <- 12


# Product properties ------------------------------------------------------

# Conversion factor for mass equilvalency of oil to gas
# Forumula:         (1 / density gas) * (conversion factor m^3 to MCF)
uopt$convert.otg <- 1/(1.24)*(35.3147/1e3)

# Density of oil
# Formula :         (density of water @ 60F)*(141.5/(API gravity + 131.5))
uopt$rho.oil <- 998.2071*(141.5/(36+131.5))

# Energy densities (converted from MJ/kg to kWh/kg)
uopt$eoil <- 42.55/3.6 # Oil
uopt$egas <- 22.78/3.6 # Gas


# Heater Options ----------------------------------------------------------

# Base design
uopt$heatcost <-    90e3*(uopt$cpi/218.056) # Base cost, inflation adjusted from 2010
uopt$heatBlength <- 1936                    # Base length (ft)


# Production, separation, and storage Options -----------------------------

# Capital and operating cost approximation functions - input cost data here from
# here:
# http://www.eia.gov/pub/oil_gas/natural_gas/data_publications/cost_indices_equipment_production/current/coststudy.html
uopt$fcapPSS <- approxfun(x = c(2e3, 4e3, 8e3, 12e3),
                          y = c(1178400, 1633700, 2584400, 3033100)*(uopt$cpi/214.537)/10,
                          rule = 2)
uopt$fopPSS <- approxfun(x = c(2e3, 4e3, 8e3, 12e3),
                          y = c(255700, 285400, 394200, 555300)*(uopt$cpi/214.537)/10,
                         rule = 2)

# Base capacity of PSS (in BOPD)
uopt$capPSSbase <- 200
uopt$opPSSbase <-  100


# Labor -------------------------------------------------------------------

# Number of operators per shift
uopt$Nopers <- 3

# Salary
uopt$salary <- 82510 # Source: http://www.bls.gov/oes/current/oes_ut.htm#17-0000 (ChemE)


# Utilities ---------------------------------------------------------------

# Utah 2014 annual average industrial electricity price ($/kWh)
# Data from EIA Average retail price of electricity to ultimate customers: http://www.eia.gov/electricity/data.cfm#sales
uopt$ep <- 0.0607

# Electrity infrastructure, assuming 500 kV single circuit line (1500 MW capacity)
# (https://www.wecc.biz/Reliability/2014_TEPPC_Transmission_CapCost_Report_B+V.pdf).
uopt$eline <-   (959700+   # Base Line cost ($/mi)
                   (24.23* # Right of way width (acres/mile)
                      85)) # BLM land capital cost ($/acre)
uopt$eswitch <- 0.1        # Substation cost as fraction of line cost

# Distance to nearest utility hub (mi)
uopt$hubL <- 50


# Finance and Econ Terms --------------------------------------------------

# Minimum construction time (in days)
uopt$tconstr.min <- round(365*9/12)

# ACRS 10-yr Depreciation Schedule
uopt$fD <- c(rep(0.1000, 365),
             rep(0.1800, 365),
             rep(0.1440, 365),
             rep(0.1152, 365),
             rep(0.0922, 365),
             rep(0.0737, 365),
             rep(0.0655, 365),
             rep(0.0655, 365),
             rep(0.0656, 365),
             rep(0.0655, 365),
             rep(0.0328, 365))

# Royalties
uopt$royalr <- 0.125

# Severance taxes
uopt$st.low <- 0.03
uopt$st.high <- 0.05
uopt$st.con <- 0.002
uopt$st.cut.o <- 13
uopt$st.cut.g <- 1.5

# Income tax rates
uopt$rTS <- 0.05
uopt$rTF <- 0.35

# Labor
uopt$radmin.comp <- 0.0125
