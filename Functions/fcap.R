# Function Info -----------------------------------------------------------
# Name:       fcap.R (Capital costing function)
# Author(s):  Jon Wilkey
# Contact:    jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# capheat - capital cost of heating system

# capPSS - capital cost of production, separation, and storage system

# capU - capital cost of utilities

# oil - oil production history

# capwell - capital cost for wells

# capwellRec - capital cost of well reclamation


# Outputs -----------------------------------------------------------------

# ccs - capital cost schedule


# Description -------------------------------------------------------------

# blah


# Function ----------------------------------------------------------------
fcap <- function(capheat, capPSS, capU, oil, capwell, capwellRec) {

  # Calculate following terms according to Seider et al. (2009)
  ccs <-         data.frame(TBM = sum(capheat)+sum(capPSS))         # Total bare module investment
  ccs$Site <-    0.1*ccs$TBM                                        # Site preparation
  ccs$Serv <-    0.1*ccs$TBM                                        # Service facilities
  ccs$capU <-    capU                                               # Utility plants/connections
  ccs$DPI <-     with(ccs, TBM+Site+Serv+capU)                      # Direct permanent investment
  ccs$Cont <-    0.18*ccs$DPI                                       # Contigency and contractor fees
  ccs$TDC <-     with(ccs, DPI+Cont)                                # Total depreciable capital
  ccs$Land <-    0.02*ccs$TDC                                       # Land
  ccs$Permit <-  0.1*sum(oil)                                       # Permitting
  ccs$RIP <-     0.02*ccs$TDC                                       # Royalties for intellectual property
  ccs$Start <-   0.1*ccs$TDC                                        # Startup
  ccs$Wells <-   sum(capwell)                                       # Wells
  ccs$wellRec <- capwellRec                                         # Well reclamation
  ccs$TPI <-     with(ccs, TDC+Land+Permit+RIP+Start+Wells+wellRec) # Total permanent investment
  ccs$WC <-      0.05*ccs$TPI                                       # Working Capital
  ccs$TCI <-     with(ccs, TPI+WC)                                  # Total capital investment

  # Return result
  return(ccs)
}
