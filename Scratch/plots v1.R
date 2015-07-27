load(file.path(path$data, "UO_main Results v3.rda"))

pdf(file.path(path$plot, "ISv4 Results.pdf"))

plot(oilSP~nwell, results,
     log = "y",
     col = "#00000025",
     ylab = "Wellhead Oil Supply Price ($/bbl) - Log Scale",
     xlab = "Number of Wells")
points(rbind(aggregate(r$oilSP, list(r$nwell), max),
             aggregate(r$oilSP, list(r$nwell), min)),
       col = "red", pch = 19)

plot(oilSP~NER, results,
     log = "y",
     col = "#00000025",
     ylab = "Wellhead Oil Supply Price ($/bbl) - Log Scale",
     xlab = "NER")
points(rbind(aggregate(r$oilSP, list(r$NER), max),
             aggregate(r$oilSP, list(r$NER), min)),
       col = "red", pch = 19)

plot(oilSP~tDrill, results,
     log = "y",
     col = "#00000025",
     ylab = "Wellhead Oil Supply Price ($/bbl) - Log Scale",
     xlab = "tDrill (days)")
points(rbind(aggregate(r$oilSP, list(r$tDrill), max),
             aggregate(r$oilSP, list(r$tDrill), min)),
       col = "red", pch = 19)

plot(oilSP~well.cap, results,
     log = "y",
     col = "#00000025",
     ylab = "Wellhead Oil Supply Price ($/bbl) - Log Scale",
     xlab = "Well Capital Cost (2014 USD)")
points(rbind(aggregate(r$oilSP, list(r$well.cap), max),
             aggregate(r$oilSP, list(r$well.cap), min)),
       col = "red", pch = 19)

plot(oilSP~totalL, results,
     log = "y",
     col = "#00000025",
     ylab = "Wellhead Oil Supply Price ($/bbl) - Log Scale",
     xlab = "Total Well Length (ft)")
points(rbind(aggregate(r$oilSP, list(r$totalL), max),
             aggregate(r$oilSP, list(r$totalL), min)),
       col = "red", pch = 19)

plot(oilSP~xg, results,
     log = "y",
     col = "#00000025",
     ylab = "Wellhead Oil Supply Price ($/bbl) - Log Scale",
     xlab = "Gas Mass Fraction")
points(rbind(aggregate(r$oilSP, list(r$xg), max),
             aggregate(r$oilSP, list(r$xg), min)),
       col = "red", pch = 19)

plot(oilSP~gp, results,
     log = "y",
     col = "#00000025",
     ylab = "Wellhead Oil Supply Price ($/bbl) - Log Scale",
     xlab = "Wellhead Gas Price ($/MCF)")
points(rbind(aggregate(r$oilSP, list(r$gp), max),
             aggregate(r$oilSP, list(r$gp), min)),
       col = "red", pch = 19)

plot(oilSP~IRR, results,
     log = "y",
     col = "#00000025",
     ylab = "Wellhead Oil Supply Price ($/bbl) - Log Scale",
     xlab = "IRR")
points(rbind(aggregate(r$oilSP, list(r$IRR), max),
             aggregate(r$oilSP, list(r$IRR), min)),
       col = "red", pch = 19)

plot(oilSP~Toil, results,
     log = "y",
     col = "#00000025",
     ylab = "Wellhead Oil Supply Price ($/bbl) - Log Scale",
     xlab = "Total Oil Production (bbl)")
points(rbind(aggregate(r$oilSP, list(r$Toil), max),
             aggregate(r$oilSP, list(r$Toil), min)),
       col = "red", pch = 19)

plot(oilSP~TCI, results,
     log = "y",
     col = "#00000025",
     ylab = "Wellhead Oil Supply Price ($/bbl) - Log Scale",
     xlab = "Total Capital Investment (2014 USD)")
points(rbind(aggregate(r$oilSP, list(r$TCI), max),
             aggregate(r$oilSP, list(r$TCI), min)),
       col = "red", pch = 19)

plot(oilSP~CPFB, results,
     log = "xy",
     col = "#00000025",
     ylab = "Wellhead Oil Supply Price ($/bbl) - Log Scale",
     xlab = "Capital per Flowing Barrel (2014 USD per bbl) - LOG SCALE")
points(rbind(aggregate(r$oilSP, list(r$CPFB), max),
             aggregate(r$oilSP, list(r$CPFB), min)),
       col = "red", pch = 19)

plot(oilSP~rec, results,
     log = "y",
     col = "#00000025",
     ylab = "Wellhead Oil Supply Price ($/bbl) - Log Scale",
     xlab = "Recovery Fraction")
points(rbind(aggregate(r$oilSP, list(r$rec), max),
             aggregate(r$oilSP, list(r$rec), min)),
       col = "red", pch = 19)

dev.off()
