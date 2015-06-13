# Function Info -----------------------------------------------------------
# Name:      stax.R (Severance tax calculation)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# prod - matrix of production volumes (bbl oil or MCF gas)

# ep - vector of inflation-adjusted energy prices ($/bbl or $/MCF)

# royalr - royalty rate (fraction)

# st.low - Low ST rate for oil/gas for values <= cutoff value threshold

# st.high - High ST rate for oil/gas for values > cutoff value threshold

# st.con - Conservation fee rate

#st.cut - Oil or gas cutoff value threshold ($/bbl or $/MCF) for switch from low
#to high ST rate


# Outputs -----------------------------------------------------------------

# ST - severance tax payments


# Description -------------------------------------------------------------

# This function determines the severance tax payments for each well (rows)
# during each timestep (columns) according to the following set of equations:

# [1] ST = rST * volume
# [2] f_st = (TV - st.cut) / st.cut
# [3] rst = (st.low * (1 - f_st) + st.high * f_st) * TV + st.con * TV
# [4] TV = price - royalr

# where ST is the severance tax payment, rST is the severance tax rate ($/bbl or
# MCF), st.low is the severance tax rate on values below st.cut (severance tax
# cutoff), st.high is the severance tax rate on values above st.cut, f_st is the
# fraction of product value that is > st.cut, TV is the taxable value, st.con is
# the conservation fee, price is the wellhead value of the product, and royalr
# is the fraction paid in royalties.


# Function ----------------------------------------------------------------
stax <- function (prod, ep, royalr, st.low, st.high, st.con, st.cut) {

  # Deduct royalty payments from price to determine taxable value (TV)
  TV <- ep-royalr*ep

  # Determine fraction of TV above split tax rate (f_st) of cutoff threshold
  f_st <- ifelse(test = (TV - st.cut)/TV < 0,
                 yes = 0,
                 no = (TV - st.cut)/TV)

  # ST per unit volume
  rST <- (st.low*(1-f_st)+st.high*f_st)*TV+st.con*TV

  # ST total
  ST <- rST*prod
  return(ST)
}
