
# Function Info -----------------------------------------------------------
# Name:       eNPV.R (NPV comparison calculation for costs of meeting energy demand)
# Author(s):  Jon Wilkey
# Contact:    jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# data - data.frame containing heating energy demand as a function of time

# ep - electricity price (in $/kWh)

# ir - interest rate

# fpower - approximation function for power demand as f(time)


# Outputs -----------------------------------------------------------------

# NPV.ep - net present value of the energy purchase option


# Description -------------------------------------------------------------

# blah


# Function ----------------------------------------------------------------
eNPV <- function(data, ep, ir, fpower) {

  # Get energy demand as f(time) on annual basis ----------------------------

  # Get sequence of time steps for integration limits in days
  steps <- c(min(data$time), seq(from = 365, to = max(data$time), by = 365))

  # Check - is last data point contained within steps sequence?
  if(max(steps) < max(data$time)) {

    # If not, add maximum time value as final integration limit
    steps <- c(steps, max(data$time))
  }

  # Predefine values to be integrated
  E <- NULL

  # For each year
  for (i in 1:(length(steps)-1)) {

    # Integrate between lower/upper limits of that year using approximation
    # functions
    E <- c(E, integrate(fpower, steps[i], steps[i+1])$value)
  }


  # Calculate discount factor -----------------------------------------------

  # Use annual discount factor formula
  df <- 1/((1+ir)^(0:(length(E)-1)))


  # NPV Purchased Electricity -----------------------------------------------

  # NPV of purchased electricity cost is just the electrical heating demand as a
  # function of time multiplied by the cost of the electricity (assumed costant),
  # and then multiplied by the discount factor

  NPV.pe <- sum(E*ep*df)

  # Return result
  return(NPV.pe)
}
