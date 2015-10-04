# Load best econ results
load(file.path(path$data, "UO_main Results v10.rda"))

pdf(file.path(path$plot, "best econ vs NER v10.pdf"))

# Create base plot
plot(oilSP~NER, results,
     log = "y",
     ylim = c(10, 10e3),
     xlim = c(0,11),
     yaxt = "n",
     xlab = "NER",
     ylab = "Oil Supply Price ($/bbl) - Log Scale")

# Draw gridlines
logGrid(min =1, max = 4, dir = "h")
abline(v = seq(0, 11, 1), lty =1 , col = "lightgrey")

# Re-plot points
points(oilSP~NER, results)

# Label y-axis
axis(side = 2, at = c(1e1, 1e2, 1e3, 1e4),
     labels = c(expression(10^1),
                expression(10^2),
                expression(10^3),
                expression(10^4)),
     las = 2)

dev.off()
