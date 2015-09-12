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
pwd.drop <- "C:/Users/jonwi/"                 # Desktop
pwd.git  <- "C:/Users/jonwi/Documents/R/"
# pwd.drop <- "C:/Users/Jon Wilkey/"              # Laptop
# pwd.git  <- "C:/Users/Jon Wilkey/Documents/R/"

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
                              "fCFterms.R",
                              "asYear.R",
                              "multiplot.R",
                              "clipboard.R"))

# Load each function in list then remove temporary file list variables
for (f in flst) source(f); remove(f, flst)


# 1.3 Libraries -----------------------------------------------------------

library(zoo)
library(sqldf)
library(lhs)
library(beepr)
library(ggplot2)
library(scales)


# 1.4 Options -------------------------------------------------------------

# Don't want strings 'typed' as factors but as characters
options(stringsAsFactors=FALSE)

# Run script "IO_options.R" to load user defined input/output options
source("UO_options.R")


# 2.0 Read in simulation data ---------------------------------------------

# Load from dataimport.R script
load(file.path(path$data, "dataImport.rda"))

# Concatonate parameter space with nwell vector
temp1 <- data.frame(index = rep(1, times = nrow(DT)), DT)
temp2 <- data.frame(index = rep(1, times = nrow(uopt$parR)), uopt$parR)
parR <- merge(x = temp1, y = temp2, all = T); remove(temp1, temp2)
parR <- parR[,-1]


# Loop --------------------------------------------------------------------

# Predefine results space
oilSP <-     rep(0, times = nrow(parR))
CPFB <-      oilSP
TCI <-       oilSP
Toil <-      oilSP
sTE <-       oilSP
prodL <-     oilSP
Tgas <-      oilSP
NER <-       oilSP
fc.heat <-   oilSP
fc.PSS <-    oilSP
fc.site <-   oilSP
fc.serv <-   oilSP
fc.util <-   oilSP
fc.cont <-   oilSP
fc.land <-   oilSP
fc.permit <- oilSP
fc.RIP <-    oilSP
fc.start <-  oilSP
fc.wells <-  oilSP
fc.WC <-     oilSP
pb.cap <-    oilSP
pb.loai <-   oilSP
pb.rstp <-   oilSP
pb.royl <-   oilSP
pb.tfts <-   oilSP
pb.heat <-   oilSP
pb.opPS <-   oilSP
pb.prof <-   oilSP
maxE <-      oilSP
meanE <-     oilSP

# Progress Bar (since this next for-loop takes a while)
pb <- txtProgressBar(min = 0, max = nrow(parR), width = 75, style = 3)

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
  coil <-  dcoil[,(ceiling(j/nrow(uopt$parR))+1)]*(wellL$prod/uopt$base.prod)*parR$rec[j]
  power <- denergy[,(ceiling(j/nrow(uopt$parR))+1)]*(wellL$prod/uopt$base.prod)

  # Fit each oil/power data with approximation functions
  fcoil <-  approxfun(x = dcoil$time, y = coil, rule = 2)
  fpower <- approxfun(x = denergy$time, y = power)


  # Heating -----------------------------------------------------------------

  # Heater capital cost
  capheat <- parR$nwell[j]*uopt$heatcost*(wellL$prod/uopt$heatBlength)^1

  # Heating operating cost calculation
  # Step 1: Calculate energy demand history
  E <- NULL
  for (k in 1:(max(denergy$time)-1)) {

    # Itegrate heating demand
    E <- c(E, integrate(fpower, k, k+1)$value)
  }

  # Convert from daily basis to hourly basis
  E <- E*24

  # Step 2: Multiply E by electricity price to get operating cost (also
  # concatonate in an additional zero to fix length of E)
  opheat <- c(0,E)*uopt$ep


  # Oil Production ----------------------------------------------------------

  # Calculate maximum potential oil production (xg = 0) on daily basis (note -
  # units are in kg)
  moil <- c(0, diff(fcoil(1:max(dcoil$time))))

  # Calculate actual oil (in bbl) = moil*(1-xg)/rho.oil*(6.2898 bbl/m^3)
  oil <- moil*(1-parR$xg[j])/uopt$rho.oil*6.2898


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
  capU <- with(uopt, hubL*(eline)*(1+eswitch))


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
                  moil*parR$xg[j]*uopt$convert.otg)

  # Gas sales
  model$gsale <- model$gasp*parR$gp[j]

  # Gas royalties
  model$rg <- -uopt$royalr*model$gsale

  # Gas severance taxes
  model$stg <- -stax(prod =    model$gasp,
                     ep =      parR$gp[j],
                     royalr =  uopt$royalr,
                     st.low =  uopt$st.low,
                     st.high = uopt$st.high,
                     st.con =  uopt$st.con,
                     st.cut =  uopt$st.cut.g)

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
               -ccs$TDC/365*uopt$fD[1:length(oil)])

  # Discount factor
  model$df <- 1/((1+parR$IRR[j])^floor((1:(tdesign+tconstr+length(oil)))/365))


  # Solve for oil price -----------------------------------------------------

  # Oil Supply Price
  oilSP[j] <- uniroot(NPV, lower = 0, upper = 1e7)$root


  # Calculate $/bbl cash flows ----------------------------------------------

  # Capital cost fractions
  fc.heat[j] <-   capheat/ccs$TCI       # Heating
  fc.PSS[j] <-    capPSS/ccs$TCI        # Product separation and storage
  fc.site[j] <-   with(ccs, Site/TCI)   # Site
  fc.serv[j] <-   with(ccs, Serv/TCI)   # Service facilities
  fc.util[j] <-   with(ccs, capU/TCI)   # Utilities (i.e. electrical grid connection)
  fc.cont[j] <-   with(ccs, Cont/TCI)   # Contingency
  fc.land[j] <-   with(ccs, Land/TCI)   # Land
  fc.permit[j] <- with(ccs, Permit/TCI) # Permitting
  fc.RIP[j] <-    with(ccs, RIP/TCI)    # Royalties for intellectual property
  fc.start[j] <-  with(ccs, Start/TCI)  # Startup
  fc.wells[j] <-  with(ccs, Wells/TCI)  # Wells
  fc.WC[j] <-     with(ccs, WC/TCI)     # Working capital

  # Run fCFterms function to get terms in cash flow equation that depend on oil
  CF <- fCFterms(oilSP[j])

  # Per barrel
  pb.cap[j] <-  with(model, sum(CTDC+CD+CWD+CSt+CWC))/sum(oil)                                    # Capital
  pb.loai[j] <- (sum(model$fixed)+sum(0.01*ccs$TPI)+sum(CF$admin.comp))/sum(oil)                  # Labor, maint., overhead, admin salary + comp, insurance
  pb.rstp[j] <- (sum(with(CF, ro+sto+TS+TF))+sum(with(model, rg+stg))-sum(0.01*ccs$TPI))/sum(oil) # Royalties, serverance, income tax, and property taxes
  pb.royl[j] <- (sum(CF$ro)+sum(model$rg))/sum(oil)                                               # Royalties only
  pb.tfts[j] <- (sum(with(CF, TS+TF)))/sum(oil)                                                   # Taxes only
  pb.heat[j] <- sum(model$opheat)/sum(oil)                                                        # Electrical heating cost
  pb.opPS[j] <- sum(model$opPSS)/sum(oil)                                                         # Product separation and storage cost
  pb.prof[j] <- sum(CF$osale)/sum(oil)+pb.cap[j]+pb.loai[j]+pb.rstp[j]+pb.heat[j]+pb.opPS[j]      # Net profit


  # Save results ------------------------------------------------------------

  Toil[j] <-  sum(oil)
  Tgas[j] <-  sum(model$gasp)
  TCI[j] <-   ccs$TCI
  CPFB[j] <-  ccs$TCI/(Toil[j]/length(oil))
  sTE[j] <-   sum(E)
  prodL[j] <- wellL$prod
  maxE[j] <-  max(E)
  meanE[j] <- mean(E)
  NER[j] <-   (sum(moil)*(1-parR$xg[j])*uopt$eoil+sum(moil)*parR$xg[j]*uopt$egas)/(sTE[j])

  # Update progress bar
  Sys.sleep(1e-3)
  setTxtProgressBar(pb, j)
}

# Close progress bar
close(pb)

# Sound off when loop is complete
beep(3, message("Script Finished"))

# ... and really save results
results <- data.frame(parR,
                      prodL,
                      oilSP,
                      Toil,
                      Tgas,
                      TCI,
                      CPFB,
                      sTE,
                      NER,
                      maxE,
                      meanE,
                      fc.heat,
                      fc.PSS,
                      fc.site,
                      fc.serv,
                      fc.util,
                      fc.cont,
                      fc.land,
                      fc.permit,
                      fc.RIP,
                      fc.start,
                      fc.wells,
                      fc.WC,
                      pb.cap,
                      pb.loai,
                      pb.rstp,
                      pb.royl,
                      pb.tfts,
                      pb.heat,
                      pb.opPS,
                      pb.prof)

save(results, file = file.path(path$data, paste("UO_main Results ", uopt$ver, ".rda", sep = "")))

# ... and write to csv
write.csv(x =    results[,1:29], # change me if more columns are add/shifted around
          file = file.path(path$data, paste("UO_main Results ", uopt$ver, ".csv", sep = "")),
          row.names = F)
