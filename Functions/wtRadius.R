# Function Info -----------------------------------------------------------
# Name:      wtRadius.R (Well Turn Radius Function)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# t - turnrate (deg. per pipe segment)

# p - length per pipe segment

# a - total angle of rotation


# Outputs -----------------------------------------------------------------

# r - radius of turn in wellbore in units of length equivalent to units on p


# Description -------------------------------------------------------------

# This function calculates the radius required to turn a pipe a degrees from
# vertical given a turning rate t (degrees / pipe segment length) for a pipe
# with segments of length p


# Function ----------------------------------------------------------------
wtRadius <- function(t, p, a) {

  # Set r's intial value to 0
  r <- 0

  # For each pipe segment, calculate the length of it's opposite side using
  # trigonometry, and add results together to get total turn radius
  for (i in seq(0, a, t)) {

    # Calculate r for pipe segment i and add to current value of r
    r <- r+p*sin(i*(pi/180))
  }

  # Return result
  return(r)
}
