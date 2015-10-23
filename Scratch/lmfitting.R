# Modify well geometry values
r <- results
r$hspace <- r$hspace * 3.28084
r$vspace <- r$vspace * 3.28084
r$loc    <- r$loc    * 3.28084
r$radius <- r$radius * 3.28084 * 12

# 1st order interactions only
test <- lm(oilSP ~ (hspace+vspace+angle+loc+radius+nrow+nwell+tDrill+well.cap+compl.cap+totalL+rec+xg+gp+IRR-1),
           r)

# Get median values of all inputs
mtv <- with(r, c(median(hspace),
                 median(vspace),
                 median(angle),
                 median(loc),
                 median(radius),
                 median(nrow),
                 median(nwell),
                 median(tDrill),
                 median(well.cap),
                 median(compl.cap),
                 median(totalL),
                 median(rec),
                 median(xg),
                 median(gp),
                 median(IRR)))



# Barplot
pdf(file.path(path$plot, "Figure 11-12 Regression relative impact v14.pdf"), width = 10, height = 7)

barplot(mtv*coefficients(test)/max(mtv*coefficients(test)),
        ylim = c(-1,1),
        ylab = "Relative OSP Impact",
        xlab = "Input Parameter",
        names.arg = c(expression(H[space]),
                      expression(V[space]),
                      expression(V[angle]),
                      expression(V[location]),
                      "r",
                      expression(n[row]),
                      expression(n[well]),
                      expression(t[Drill]),
                      expression(C[Drill]),
                      expression(C[Compl]),
                      "L",
                      expression(x[r]),
                      expression(x[g]),
                      "gp",
                      "IRR"))

dev.off()

setEPS(width = 10, height = 7)
postscript(file.path(path$plot, "Figure 11-12 Regression relative impact v14.eps"))

barplot(mtv*coefficients(test)/max(mtv*coefficients(test)),
        ylim = c(-1,1),
        ylab = "Relative OSP Impact",
        xlab = "Input Parameter",
        names.arg = c(expression(H[space]),
                      expression(V[space]),
                      expression(V[angle]),
                      expression(V[location]),
                      "r",
                      expression(n[row]),
                      expression(n[well]),
                      expression(t[Drill]),
                      expression(C[Drill]),
                      expression(C[Compl]),
                      "L",
                      expression(x[r]),
                      expression(x[g]),
                      "gp",
                      "IRR"))

dev.off()
