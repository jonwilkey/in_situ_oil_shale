pdf(file.path(path$plot, "ISv2 Results.pdf"))

# pairs(~nwell+NER+tDrill+well.cap+totalL+xg+gp+IRR+log(oilSP)+Toil+TCI+CPFB,
#       data = results,
#       main = "Scatterplot Matrix of All In Situ v2 Results",
#       col = "#00000025")

plot(oilSP~nwell, results,
     log = "y",
     col = "#00000025",
     ylab = "Wellhead Oil Supply Price ($/bbl) - Log Scale",
     xlab = "Number of Wells")

plot(oilSP~NER, results,
     log = "y",
     col = "#00000025",
     ylab = "Wellhead Oil Supply Price ($/bbl) - Log Scale",
     xlab = "NER")

plot(oilSP~tDrill, results,
     log = "y",
     col = "#00000025",
     ylab = "Wellhead Oil Supply Price ($/bbl) - Log Scale",
     xlab = "tDrill (days)")

plot(oilSP~well.cap, results,
     log = "y",
     col = "#00000025",
     ylab = "Wellhead Oil Supply Price ($/bbl) - Log Scale",
     xlab = "Well Capital Cost (2014 USD)")

plot(oilSP~totalL, results,
     log = "y",
     col = "#00000025",
     ylab = "Wellhead Oil Supply Price ($/bbl) - Log Scale",
     xlab = "Total Well Length (ft)")

plot(oilSP~xg, results,
     log = "y",
     col = "#00000025",
     ylab = "Wellhead Oil Supply Price ($/bbl) - Log Scale",
     xlab = "Gas Mass Fraction")

plot(oilSP~gp, results,
     log = "y",
     col = "#00000025",
     ylab = "Wellhead Oil Supply Price ($/bbl) - Log Scale",
     xlab = "Wellhead Gas Price ($/MCF)")

plot(oilSP~IRR, results,
     log = "y",
     col = "#00000025",
     ylab = "Wellhead Oil Supply Price ($/bbl) - Log Scale",
     xlab = "IRR")

plot(oilSP~Toil, results,
     log = "y",
     col = "#00000025",
     ylab = "Wellhead Oil Supply Price ($/bbl) - Log Scale",
     xlab = "Total Oil Production (bbl)")

plot(oilSP~TCI, results,
     log = "y",
     col = "#00000025",
     ylab = "Wellhead Oil Supply Price ($/bbl) - Log Scale",
     xlab = "Total Capital Investment (2014 USD)")

plot(oilSP~CPFB, results,
     log = "xy",
     col = "#00000025",
     ylab = "Wellhead Oil Supply Price ($/bbl) - Log Scale",
     xlab = "Capital per Flowing Barrel (2014 USD per bbl) - LOG SCALE")

dev.off()
