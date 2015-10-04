# Function Info -----------------------------------------------------------
# Name:       ffoc.R (Fixed operating costs function)
# Author(s):  Jon Wilkey
# Contact:    jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# Nopers - number of operators per shift

# CTDC - total depreciable capital cost

# CTPI - total permanent investment


# Outputs -----------------------------------------------------------------

# All fixed costs (minus management incentive compensation)


# Description -------------------------------------------------------------

# Calcualtes fixed costs based on number of operators per shift and capital
# costing terms, returning total fixed expense result.


# Function ----------------------------------------------------------------
ffoc <- function(Nopers, CTDC, CTPI, salary) {

  # Labor for operations
  LW <-   Nopers*5*2080*30 # Labor wages
  LS <-   0.15*LW          # Labor salaries
  opSS <- 0.06*LW          # Operating supplies and services
  TA <-   salary*5           # Technical assistance
  CL <-   salary*5           # Control laboratory
  L <-    LW+LS+opSS+TA+CL # Labor total

  # Maintenance
  M <-   0.05*CTDC # Maintenance
  MW <-  0.4348*M  # Maint. wages
  MS <-  0.1087*M  # Maint. salaries
  MMS <- 0.4348*M  # Maint. materials
  MO <-  0.0217*M  # Maint. overhead

  # Operating Overhead
  GPO <- 0.071*(LS+LW+MW+MS) # General plant overhead
  MDS <- 0.024*(LS+LW+MW+MS) # Mechanical dept. services
  ERD <- 0.059*(LS+LW+MW+MS) # Employee relations dept.
  BS <-  0.074*(LS+LW+MW+MS) # Business services
  Ov <- GPO+MDS+ERD+BS       # Overhead total

  # General Expenses
  admin <- 200e3 # Administrative expense (1 admin), incentive compensation is separate

  # Property Taxes
  pt <- 0.01*CTPI

  # Insurance
  ins <- 0.004*CTPI

  # Final fixed cost (per year)
  result <- L+M+Ov+admin+pt+ins

  # Return result
  return(result)
}
