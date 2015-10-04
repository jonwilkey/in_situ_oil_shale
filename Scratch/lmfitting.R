
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

# 1st order interactions only
test <- lm(oilSP ~ (hspace+vspace+angle+loc+radius+nrow+nwell+tDrill+well.cap+prodL+rec+xg+gp+IRR-1),
           results)

# Get median values of all inputs
mtv <- with(results, c(median(hspace),
                       median(vspace),
                       median(angle),
                       median(loc),
                       median(radius),
                       median(nrow),
                       median(nwell),
                       median(tDrill),
                       median(well.cap),
                       median(prodL),
                       median(rec),
                       median(xg),
                       median(gp),
                       median(IRR)))

# Barplot
# setEPS()
# postscript(file.path(path$plot, "test.eps"))
# tiff(file.path(path$plot, "lm barplot v10.tif"), res=600, compression = "lzw", height=7, width=10, units="in")
pdf(file.path(path$plot, "lm barplot v10.pdf"), width = 10, height = 7)
barplot(mtv*coefficients(test)/max(mtv*coefficients(test)),
        ylim = c(-1,1),
        ylab = "Relative OSP Impact",
        #xlab = "Input Parameter",
        names.arg = c(expression(H[space]),
                      expression(V[space]),
                      expression(V[angle]),
                      expression(V[loc]),
                      "r",
                      expression(n[row]),
                      expression(n[well]),
                      expression(t[Drill]),
                      expression(C[DC]),
                      "L",
                      expression(x[r]),
                      expression(x[g]),
                      "gp",
                      "IRR"))
dev.off()
