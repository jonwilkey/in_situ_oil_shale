# Predefine results matrix
epr <- matrix(0, nrow = (max(denergy$time)-1), ncol = (ncol(denergy)-1))

# For each scenario i
for (i in 2:243) {

  # Get base data
  power <- denergy[,i]

  # Adjust for well length assuming median producer length of 8500 ft
  power <- power*(8500/uopt$base.prod)

  # Approximation function
  fpower <- approxfun(x = denergy$time, y = power)

  # Heating operating cost calculation
  # Step 1: Calculate energy demand history
  E <- NULL
  for (k in 1:(max(denergy$time)-1)) {

    # Itegrate heating demand
    E <- c(E, integrate(fpower, k, k+1)$value)
  }

  # Convert from daily basis to hourly basis and save
  epr[,i-1] <- E*24
}

# Get median curve
qepr <- apply(epr, 1, median)

# Set line colors for quantiles used in quant
linecolor <- "#00000033"

# PDF copy
pdf(file.path(path$plot, "Figure 11-6 Daily electricity demand curves.pdf"))

# Main plot with largest quantile result
plot(1:2554, epr[,1]/1e3,
     ylim = c(100, 100e3),
     yaxt = "n",
     log = "y",
     type = "l",
     col = "#00000010",
     xlab = "Time (days)",
     ylab = "Electricity Demand (MWh / day)")
for (i in 2:ncol(epr)) {lines(1:2554, epr[,i]/1e3, col = linecolor)}
lines(1:2554, qepr/1e3, col = "black", lwd = 2, lty = 2)

axis(side = 2, at = c(1e2, 1e3, 1e4, 1e5),
     labels = c(expression(10^2),
                expression(10^3),
                expression(10^4),
                expression(10^5)),
     las = 2)
legend("topright",
       c("Scenario", "Median"),
       lty = c(1, 2),
       col = c(linecolor, "black"),
       lwd = c(1, 2))

dev.off()

# EPS copy
setEPS()
postscript(file.path(path$plot, "Figure 11-6 Daily electricity demand curves.eps"))

# Main plot with largest quantile result
plot(1:2554, epr[,1]/1e3,
     ylim = c(100, 100e3),
     yaxt = "n",
     log = "y",
     type = "l",
     col = "#00000010",
     xlab = "Time (days)",
     ylab = "Electricity Demand (MWh / day)")
for (i in 2:ncol(epr)) {lines(1:2554, epr[,i]/1e3, col = linecolor)}
lines(1:2554, qepr/1e3, col = "black", lwd = 2, lty = 2)

axis(side = 2, at = c(1e2, 1e3, 1e4, 1e5),
     labels = c(expression(10^2),
                expression(10^3),
                expression(10^4),
                expression(10^5)),
     las = 2)
legend("topright",
       c("Scenario", "Median"),
       lty = c(1, 2),
       col = c(linecolor, "black"),
       lwd = c(1, 2))

dev.off()
