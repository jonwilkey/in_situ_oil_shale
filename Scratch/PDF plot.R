

# Set quantile sequence
qseq <- seq(from = 0.05, to = 0.95, by = 0.1)

# Add names
qseq.names <- c("p10", "p20", "p30", "p40", "p50", "p60", "p70", "p80", "p90")

# Get quantiles for each parameter
q.par.range <- data.frame(tDrill   =  quantile(results$tDrill,   qseq), # Time to drill and complete well (days)
                          well.cap =  quantile(results$well.cap, qseq), # Drilling capital cost
                          prodL    =  quantile(results$prodL,    qseq), # Total well length (ft)
                          xg       =  quantile(results$xg,       qseq), # Mass fraction gas
                          gp       =  quantile(results$gp,       qseq), # Gas price ($/MCF)
                          IRR      =  quantile(results$IRR,      qseq), # Internal rate of return
                          rec      =  quantile(results$rec,      qseq)) # Recovery fraction

# Start plot and set plotting options
line.color <- rainbow((length(qseq)-1))
pdf(file.path(path$plot, "OSP PDF by parameter v13.pdf"))

# For each parameter
for (j in 1:ncol(q.par.range)) {

  # Calculate probability density of first quantile range
  temp <- density(results$oilSP[which(results[names(q.par.range)[j]] >= q.par.range[1, j] &
                                      results[names(q.par.range)[j]] <= q.par.range[2, j])],
                  from = 0,
                  to =   100e3,
                  n =    2 ^ 15)

  # Switch from list to data.frame
  temp <- data.frame(x = temp$x, y = temp$y)

  # For all other quantile ranges
  for (i in 2:(length(qseq)-1)) {

    # Get just the probability density values in that range
    temp <- cbind(temp,
                  density(results$oilSP[which(results[names(q.par.range)[j]] >= q.par.range[i,     j] &
                                              results[names(q.par.range)[j]] <= q.par.range[i + 1, j])],
                            from = 0,
                            to =   100e3,
                            n =    2 ^ 15)$y)
  }

  # Main plot
  plot(temp[, 1], temp[, 2],
       type = "l",
       col  = line.color[1],
       xlim = c(0, 3e3),
       ylim = c(0, 1.05 * max(temp[, 2:ncol(temp)])),
       xlab = "OSP ($ / bbl)",
       ylab = "Probability Density",
       main = paste("PDF for Quantiles of", names(q.par.range)[j]))

  # Grid on
  grid(lty = 1, col = "lightgrey")

  # Redraw all quantile lines
  for (i in 1:(length(qseq)-1)) {

    # Draw each line
    lines(temp[, 1], temp[, i + 1], col = line.color[i])
  }

  # Add legend
  legend("topright",
         legend = qseq.names,
         lty    = 1,
         ncol   = 2,
         col    = line.color,
         bg     = "white")
}

# Close plot
dev.off()
