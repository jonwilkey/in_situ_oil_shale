load(file.path(path$data, paste("UO_main Results ", uopt$ver, ".rda", sep = "")))


# Base graphics - Scatterplots --------------------------------------------

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


# ggplot2 Graphics --------------------------------------------------------

# Histogram
pdf(file.path(path$plot, "ISS v8 hist.pdf"))

ggplot(r, aes(x = nwell)) +    geom_histogram(aes(y=..density..),binwidth = 1, colour = "black", fill = "white") + theme_bw() + geom_density(alpha = 0.2, fill = "lightgrey")
ggplot(r, aes(x = NER)) +      geom_histogram(aes(y=..density..), colour = "black", fill = "white") + theme_bw() + geom_density(alpha = 0.2, fill = "lightgrey")
ggplot(r, aes(x = tDrill)) +   geom_histogram(aes(y=..density..), colour = "black", fill = "white") + theme_bw() + geom_density(alpha = 0.2, fill = "lightgrey")
ggplot(r, aes(x = well.cap)) + geom_histogram(aes(y=..density..), colour = "black", fill = "white") + theme_bw() + geom_density(alpha = 0.2, fill = "lightgrey")
ggplot(r, aes(x = totalL)) +   geom_histogram(aes(y=..density..), colour = "black", fill = "white") + theme_bw() + geom_density(alpha = 0.2, fill = "lightgrey")
ggplot(r, aes(x = xg)) +       geom_histogram(aes(y=..density..), colour = "black", fill = "white") + theme_bw() + geom_density(alpha = 0.2, fill = "lightgrey")
ggplot(r, aes(x = rec)) +      geom_histogram(aes(y=..density..), colour = "black", fill = "white") + theme_bw() + geom_density(alpha = 0.2, fill = "lightgrey")
ggplot(r, aes(x = gp)) +       geom_histogram(aes(y=..density..), colour = "black", fill = "white") + theme_bw() + geom_density(alpha = 0.2, fill = "lightgrey")
ggplot(r, aes(x = IRR)) +      geom_histogram(aes(y=..density..), colour = "black", fill = "white") + theme_bw() + geom_density(alpha = 0.2, fill = "lightgrey")

dev.off()

# Black and white theme with no y axis label
theme_bw_noy <- function (base_size = 12, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(axis.text = element_text(size = rel(0.8)),
          axis.ticks = element_line(colour = "black"),
          axis.title.y = element_blank(),
          legend.key = element_rect(colour = "grey80"),
          panel.background = element_rect(fill = "white", colour = NA),
          panel.border = element_rect(fill = NA, colour = "grey50"),
          panel.grid.major = element_line(colour = "grey90", size = 0.2),
          panel.grid.minor = element_line(colour = "grey98", size = 0.5),
          strip.background = element_rect(fill = "grey80", colour = "grey50", size = 0.2))
}

# Multiplot hexplot function
multi.xyhex <- function(r, logflag) {

  fnwell <- ggplot(r, aes(x = nwell, y = oilSP)) +
    stat_binhex(bins = 20) +
    scale_fill_gradientn(colours = c("lightgrey","black")) +
    theme_bw() +
    xlab("Number of Wells") +
    ylab("OSP ($/bbl)") +
    guides(fill = FALSE)
  if (logflag == T) {
    fnwell <- fnwell+scale_y_continuous(trans = log10_trans(),
                                        breaks = c(10^2, 10^3, 10^4, 10^5),
                                        labels = trans_format("log10", math_format(10^.x)))
  }

  ftDrill <- ggplot(r, aes(x = tDrill, y = oilSP)) +
    stat_binhex(bins = 20) +
    scale_fill_gradientn(colours = c("lightgrey","black")) +
    theme_bw() +
    xlab("tDrill (days)") +
    ylab("OSP ($/bbl)") +
    guides(fill = FALSE)
  if (logflag == T) {
    ftDrill <- ftDrill+scale_y_continuous(trans = log10_trans(),
                                        breaks = c(10^2, 10^3, 10^4, 10^5),
                                        labels = trans_format("log10", math_format(10^.x)))
  }

  fcwell <- ggplot(r, aes(x = well.cap/1e6, y = oilSP)) +
    stat_binhex(bins = 20) +
    scale_fill_gradientn(colours = c("lightgrey","black")) +
    theme_bw() +
    xlab("Well Cost($1e6)") +
    ylab("OSP ($/bbl)") +
    guides(fill = FALSE)
  if (logflag == T) {
    fcwell <- fcwell+scale_y_continuous(trans = log10_trans(),
                                        breaks = c(10^2, 10^3, 10^4, 10^5),
                                        labels = trans_format("log10", math_format(10^.x)))
  }

  fprodL <- ggplot(r, aes(x = prodL/1e3, y = oilSP)) +
    stat_binhex(bins = 20) +
    scale_fill_gradientn(colours = c("lightgrey","black")) +
    theme_bw_noy() +
    xlab("Heater Length (1e3 ft)") +
    ylab("OSP ($/bbl)") +
    guides(fill = FALSE)
  if (logflag == T) {
    fprodL <- fprodL+scale_y_continuous(trans = log10_trans(),
                                        breaks = c(10^2, 10^3, 10^4, 10^5),
                                        labels = trans_format("log10", math_format(10^.x)))
  }

  frec <- ggplot(r, aes(x = rec, y = oilSP)) +
    stat_binhex(bins = 20) +
    scale_fill_gradientn(colours = c("lightgrey","black")) +
    theme_bw_noy() +
    xlab("Product Recovery") +
    ylab("OSP ($/bbl)") +
    guides(fill = FALSE)
  if (logflag == T) {
    frec <- frec+scale_y_continuous(trans = log10_trans(),
                                        breaks = c(10^2, 10^3, 10^4, 10^5),
                                        labels = trans_format("log10", math_format(10^.x)))
  }

  fxg <- ggplot(r, aes(x = xg, y = oilSP)) +
    stat_binhex(bins = 20) +
    scale_fill_gradientn(colours = c("lightgrey","black")) +
    theme_bw_noy() +
    xlab("Gas Fraction") +
    ylab("OSP ($/bbl)") +
    guides(fill = FALSE)
  if (logflag == T) {
    fxg <- fxg+scale_y_continuous(trans = log10_trans(),
                                        breaks = c(10^2, 10^3, 10^4, 10^5),
                                        labels = trans_format("log10", math_format(10^.x)))
  }

  fgp <- ggplot(r, aes(x = gp, y = oilSP)) +
    stat_binhex(bins = 20) +
    scale_fill_gradientn(colours = c("lightgrey","black")) +
    theme_bw_noy() +
    xlab("Gas Price ($/MCF)") +
    ylab("OSP ($/bbl)") +
    guides(fill = FALSE)
  if (logflag == T) {
    fgp <- fgp+scale_y_continuous(trans = log10_trans(),
                                        breaks = c(10^2, 10^3, 10^4, 10^5),
                                        labels = trans_format("log10", math_format(10^.x)))
  }

  fIRR <- ggplot(r, aes(x = IRR, y = oilSP)) +
    stat_binhex(bins = 20) +
    scale_fill_gradientn(colours = c("lightgrey","black")) +
    theme_bw_noy() +
    xlab("IRR") +
    ylab("OSP ($/bbl)") +
    guides(fill = FALSE)
  if (logflag == T) {
    fIRR <- fIRR+scale_y_continuous(trans = log10_trans(),
                                        breaks = c(10^2, 10^3, 10^4, 10^5),
                                        labels = trans_format("log10", math_format(10^.x)))
  }

  fNER <- ggplot(r, aes(x = NER, y = oilSP)) +
    stat_binhex(bins = 20) +
    scale_fill_gradientn(colours = c("lightgrey","black")) +
    theme_bw_noy() +
    xlab("NER") +
    ylab("OSP ($/bbl)") +
    guides(fill = FALSE)
  if (logflag == T) {
    fNER <- fNER+scale_y_continuous(trans = log10_trans(),
                                        breaks = c(10^2, 10^3, 10^4, 10^5),
                                        labels = trans_format("log10", math_format(10^.x)))
  }

  multiplot(fnwell, ftDrill, fcwell, fprodL, frec, fxg, fgp, fIRR, fNER, cols = 3)
}

# Plot full dataset
pdf(file.path(path$plot, "xy full multi v9.pdf"))
multi.xyhex(r = results, logflag = TRUE)
dev.off()

# Plot reduced results
pdf(file.path(path$plot, "xyhex reduced v9.pdf"))
multi.xyhex(r = results[results$oilSP <= 175,], logflag = FALSE)
dev.off()

# Boxplots for economically viable set

# Reshape
bdr <- rbind(data.frame(type = as.factor("CTPI"),   cost = (-r$pb.cap)),
             data.frame(type = as.factor("CV"),     cost = (-(r$pb.heat+r$pb.opPS))),
             data.frame(type = as.factor("CF"),     cost = (-r$pb.loai)),
             data.frame(type = as.factor("R"),      cost = (-r$pb.royl)),
             data.frame(type = as.factor("TFTS"),   cost = (-r$pb.tfts)),
             data.frame(type = as.factor("STP"),    cost = (-(r$pb.rstp-r$pb.royl-r$pb.tfts))),
             data.frame(type = as.factor("Profit"), cost = r$pb.prof))

# Drop any negatives
bdr <- bdr[bdr$cost >= 0,]

pdf(file.path(path$plot, "costs per bbl.pdf"))

boxplot(cost~type, bdr,
        range = 0,
        names = c(expression(C[TPI]),
                  expression(C[V]),
                  expression(C[F]),
                  expression(R),
                  expression(T[F] + T[S]),
                  expression(ST),
                  "Profit"),
        xlab = "Cost Category",
        ylab = "Cost ($/bbl)")

dev.off()

r <- results[results$oilSP <= 175,]

# Repeat for capital
cdr <- rbind(data.frame(type = as.factor("heat"),   frac = r$fc.heat),
             data.frame(type = as.factor("PSS"),    frac = r$fc.PSS),
             data.frame(type = as.factor("SS"),     frac = (r$fc.site+r$fc.serv)),
             data.frame(type = as.factor("util"),   frac = r$fc.util),
             data.frame(type = as.factor("cont"),   frac = r$fc.cont),
             data.frame(type = as.factor("land"),   frac = r$fc.land),
             data.frame(type = as.factor("permit"), frac = r$fc.permit),
             data.frame(type = as.factor("RIP"),    frac = r$fc.RIP),
             data.frame(type = as.factor("start"),  frac = r$fc.start),
             data.frame(type = as.factor("wells"),  frac = r$fc.wells),
             data.frame(type = as.factor("WC"),     frac = r$fc.WC))

cdrc <- rbind(data.frame(type = as.factor("heat"),   frac = r$fc.heat*r$TCI),
              data.frame(type = as.factor("PSS"),    frac = r$fc.PSS*r$TCI),
              data.frame(type = as.factor("SS"),     frac = (r$fc.site+r$fc.serv)*r$TCI),
              data.frame(type = as.factor("util"),   frac = r$fc.util*r$TCI),
              data.frame(type = as.factor("cont"),   frac = r$fc.cont*r$TCI),
              data.frame(type = as.factor("land"),   frac = r$fc.land*r$TCI),
              data.frame(type = as.factor("permit"), frac = r$fc.permit*r$TCI),
              data.frame(type = as.factor("RIP"),    frac = r$fc.RIP*r$TCI),
              data.frame(type = as.factor("start"),  frac = r$fc.start*r$TCI),
              data.frame(type = as.factor("wells"),  frac = r$fc.wells*r$TCI),
              data.frame(type = as.factor("WC"),     frac = r$fc.WC*r$TCI))

pdf(file.path(path$plot, "capital cost boxplot.pdf"))

boxplot(frac~type, cdrc,
        range = 0,
        log = "y",
        ylim = c(5e5, 5e9),
        yaxt = "n",
        names = c("Heat",
                  "PSS",
                  expression(C[SS]),
                  expression(C[alloc]),
                  expression(C[cont]),
                  expression(C[L]),
                  expression(C[P]),
                  expression(C[RIP]),
                  expression(C[S]),
                  expression(C[DC]),
                  expression(C[WC])),
        xlab = "Capital Cost Category",
        ylab = expression(paste("Capital Cost ($)")))
axis(side = 2, at = c(1e5, 1e6, 1e7, 1e8, 1e9, 1e10),
     labels = c(expression(10^5),
                expression(10^6),
                expression(10^7),
                expression(10^8),
                expression(10^9),
                expression(10^10)),
     las = 2)

dev.off()



# Tables ------------------------------------------------------------------

# Best scenarios by OSP
r <- with(results, data.frame(design, LHS, oilSP, nwell, tDrill, well.cap, prodL, rec, xg, gp, IRR, NER))

# Predefine results object
bso <- NULL

# For each of the top i results
for (i in 1:5) {

  # Find the LHS # of the lowest oilSP
  lhsn <- r$LHS[which.min(r$oilSP)]

  # Get row indices of lhsn in r
  ind <- which(r$LHS == lhsn)

  # Get a temporary subset of r
  temp <- r[ind,]

  # Reorder according to OSP
  temp <- temp[order(temp$oilSP),]

  # create table
  dft <- as.data.frame(matrix(c(lhsn, temp$design[1:5], lhsn, round(temp$oilSP[1:5],2)), nrow = 2, byrow = T))

  # Change variable types and round
  dft[c(1, 3, 5, 7, 9),] <- as.integer(dft[c(1, 3, 5, 7, 9),])
  dft[c(2, 4, 6, 8, 10),] <- round(dft[c(2, 4, 6, 8, 10),], 2)

  # Save out results
  bso <- rbind(bso, dft)

  # Drop all lhsn rows from r
  r <- r[-ind,]
}

# Worst scenarios by OSP
r <- with(results, data.frame(design, LHS, oilSP, nwell, tDrill, well.cap, prodL, rec, xg, gp, IRR, NER))

# Predefine results object
wso <- NULL

# For each of the top i results
for (i in 1:5) {

  # Find the LHS # of the lowest oilSP
  lhsn <- r$LHS[which.max(r$oilSP)]

  # Get row indices of lhsn in r
  ind <- which(r$LHS == lhsn)

  # Get a temporary subset of r
  temp <- r[ind,]

  # Reorder according to OSP
  temp <- temp[order(temp$oilSP, decreasing = T),]

  # create table
  dft <- round(data.frame(matrix(c(lhsn, temp$design[1:5], lhsn, round(temp$oilSP[1:5],2)), nrow = 2, byrow = T)))

  # Save out results
  wso <- rbind(wso, dft)

  # Drop all lhsn rows from r
  r <- r[-ind,]
}

# Top 5 best/worst LHS scenario parameters table
lhsn <- unique(c(bso[,1],wso[,1]))

# Predefine results object
bwso <- NULL

for (i in 1:length(lhsn)) {

  # Subset
  temp <- results[results$LHS == lhsn[i],c("tDrill", "well.cap", "prodL", "rec", "xg", "gp", "IRR")]

  # Change data types and round
  temp$tDrill <- round(temp$tDrill)
  temp$well.cap <- round(temp$well.cap/1e6,2)
  temp$prodL <- round(temp$prodL/1e3,2)
  temp[,c("rec", "xg", "gp", "IRR")] <- round(temp[,c("rec", "xg", "gp", "IRR")],2)

  bwso <- rbind(bwso,
                c(lhsn[i],unique(temp)))
}

# Predefine results
cr <- matrix(0, nrow = 9, ncol = 242)
pr <- cr

# Get correlation coefficients and p-values for each ISD
for (i in 1:242) {

  # Get subset
  temp <- corr.test(results[results$design == i,
                            c("oilSP", "tDrill", "well.cap", "prodL", "rec", "xg", "gp", "IRR", "NER")])

  # Extract values
  cr[,i] <- temp$r[,1]
  pr[,i] <- temp$p[,1]
}

# Make table
cpr <- data.frame(rmean = rowMeans(cr), pmean = rowMeans(pr))

# Drop first row and round r value
cpr <- cpr[-1,]
cpr$rmean <- round(cpr$rmean,2)


# Energy demand curve -----------------------------------------------------

test <-        read.csv(file.path(path$raw, "energy-164.csv"))
test[,2:3] <-  test[,2:3]*(8500/uopt$base.prod)
names(test) <- c("time","ce","ie")
test$time <-   test$time/3600/24
