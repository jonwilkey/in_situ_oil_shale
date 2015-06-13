# Function Info -----------------------------------------------------------
# Name:       fNPV.R (Net present value function)
# Author(s):  Jon Wilkey
# Contact:    jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# op - oil price in $/bbl


# Outputs -----------------------------------------------------------------

# Net present value (NPV)


# Description -------------------------------------------------------------

# Calcualtes NPV as function of oil price


# Function ----------------------------------------------------------------
NPV <- function(op) {

  # Oil Sales
  osale <- model$oilp*op

  # Royalties
  ro <- -uopt$royalr*osale

  # Severance taxes
  sto <- -stax(prod = model$oilp, ep = op, uopt$royalr, uopt$st.low, uopt$st.high, uopt$st.con, uopt$st.cut.o)

  # Depletion
  d <- -(ccs$Land/sum(moil))*model$oilp

  # Income taxes
  TI <- osale+ro+sto+d+with(model, gsale+opPSS+opheat+fixed+D+rg+stg) # Taxable Income
  TI <- ifelse(TI < 0, 0, TI)                                         # Only keep positive values of TI
  TS <- -uopt$rTS*TI                                                   # State income taxes
  TF <- -uopt$rTF*(TI+TS)                                              # Federal income taxes

  # Administrative compensation
  NP <- osale+ro+sto+TS+TF+with(model, gsale+rg+stg+opPSS+opheat+fixed+CTDC+CD+CWD+CSt+CWC)
  admin.comp <- -uopt$radmin.comp*ifelse(NP > 0, NP, 0)

  # Final cash flow
  CF <- osale+ro+sto+TS+TF+admin.comp+with(model, gsale+rg+stg+opPSS+opheat+fixed+CTDC+CD+CWD+CSt+CWC)

  # Final NPV
  NPV <- sum(model$df*CF)

  return(NPV)
}
