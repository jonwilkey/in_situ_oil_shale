
# Predefine results matrix
fitc <- matrix(0, nrow = length(unique(results$design)), ncol = 7)
rsquared <- nrow(fitc)

for (i in 1:nrow(fitc)) {

  # Select only results from ISD i
  r <- results[results$design == i,]

  temp <- lm(oilSP~tDrill+well.cap+prodL+rec+xg+IRR+normNER-1, results)

  # Extract coefficients
  fitc[i,] <- coefficients(temp)

  # Extract R^2 value
  rsquared[i] <- summary(temp)$r.squared
}

# Combine
fitc <- data.frame(fitc, rsquared)
names(fitc) <- c("tDrill", "well.cap", "prodL", "rec", "xg", "IRR", "normNER", "rsquared")
