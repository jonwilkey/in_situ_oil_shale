pdf(file.path(path$plot, "OSP CDF plot.pdf"))
plot(test,
     log = "x",
     xlim = c(10, 100e3),
     xlab = "OSP ($/bbl)",
     ylab = "Cumulative probability",
     main = "")
logGrid(1,5,"v", col = "lightgrey")
abline(h = c(0, 1), col = "lightgrey")
lines(test)
points(x = 175, y = length(which(results$oilSP <= 175))/nrow(results), col = "red", cex = 1.25)
dev.off()
