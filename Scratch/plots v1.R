load(file.path(path$data, paste("UO_main Results ", uopt$ver, ".rda", sep = "")))

# Price cutoff for economic viability
eia.op <- read.csv(file.path(path$raw, "EIA2015AEO highoilOP.csv"), skip = 4)
names(eia.op) <- c("year", "op")

# Prices are in 2013 dollars (CPI = 232.957), adjust to 2014 USD
eia.op$op <- eia.op$op * (uopt$cpi / 232.957)

price.cut <- mean(eia.op$op[eia.op$year >= 2015])

# # Repeat for reference price forecast
# eia.op <- read.csv(file.path(path$raw, "EIA2015AEO refoilOP.csv"), skip = 4)
# names(eia.op) <- c("year", "op")
#
# # Prices are in 2013 dollars (CPI = 232.957), adjust to 2014 USD
# eia.op$op <- eia.op$op * (uopt$cpi / 232.957)
#
# price.cut.ref <- mean(eia.op$op[eia.op$year >= 2015])


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

  fhspace <- ggplot(r, aes(x = hspace*3.28084, y = oilSP)) +
    stat_binhex(bins = 20) +
    scale_fill_gradientn(colours = c("lightgrey","black")) +
    theme_bw() +
    xlab(expression(paste(H[space], " (ft)"))) +
    ylab("OSP ($ / bbl)") +
    guides(fill = FALSE)
  if (logflag == T) {
    fhspace <- fhspace+scale_y_continuous(trans = log10_trans(),
                                          breaks = c(10^2, 10^3, 10^4, 10^5),
                                          labels = trans_format("log10", math_format(10^.x)))
  }

  fvspace <- ggplot(r, aes(x = vspace*3.28084, y = oilSP)) +
    stat_binhex(bins = 20) +
    scale_fill_gradientn(colours = c("lightgrey","black")) +
    theme_bw() +
    xlab(expression(paste(V[space], " (ft)"))) +
    ylab("OSP ($ / bbl)") +
    guides(fill = FALSE)
  if (logflag == T) {
    fvspace <- fvspace+scale_y_continuous(trans = log10_trans(),
                                          breaks = c(10^2, 10^3, 10^4, 10^5),
                                          labels = trans_format("log10", math_format(10^.x)))
  }

  fangle <- ggplot(r, aes(x = angle, y = oilSP)) +
    stat_binhex(bins = 20) +
    scale_fill_gradientn(colours = c("lightgrey","black")) +
    theme_bw() +
    xlab(expression(paste(V[angle], " (deg)"))) +
    ylab("OSP ($ / bbl)") +
    guides(fill = FALSE)
  if (logflag == T) {
    fangle <- fangle+scale_y_continuous(trans = log10_trans(),
                                        breaks = c(10^2, 10^3, 10^4, 10^5),
                                        labels = trans_format("log10", math_format(10^.x)))
  }

  floc <- ggplot(r, aes(x = loc*3.28084, y = oilSP)) +
    stat_binhex(bins = 20) +
    scale_fill_gradientn(colours = c("lightgrey","black")) +
    theme_bw() +
    xlab(expression(paste(V[location], " (ft)"))) +
    ylab("OSP ($ / bbl)") +
    guides(fill = FALSE)
  if (logflag == T) {
    floc <- floc+scale_y_continuous(trans = log10_trans(),
                                    breaks = c(10^2, 10^3, 10^4, 10^5),
                                    labels = trans_format("log10", math_format(10^.x)))
  }

  fradius <- ggplot(r, aes(x = as.factor(round(radius*3.28084,2)), y = oilSP)) +
    geom_violin(fill = "grey") +
    theme_bw_noy() +
    xlab("r (ft)") +
    ylab("OSP ($ / bbl)") +
    guides(fill = FALSE)
  if (logflag == T) {
    fradius <- fradius+scale_y_continuous(trans = log10_trans(),
                                          breaks = c(10^2, 10^3, 10^4, 10^5),
                                          labels = trans_format("log10", math_format(10^.x)))
  }

  fnrow <- ggplot(r, aes(x = as.factor(nrow), y = oilSP)) +
    geom_violin(fill = "grey") +
    theme_bw_noy() +
    xlab(expression(n[row])) +
    ylab("OSP ($ / bbl)") +
    guides(fill = FALSE)
  if (logflag == T) {
    fnrow <- fnrow+scale_y_continuous(trans = log10_trans(),
                                      breaks = c(10^2, 10^3, 10^4, 10^5),
                                      labels = trans_format("log10", math_format(10^.x)))
  }

  fnwell <- ggplot(r, aes(x = nwell, y = oilSP)) +
    stat_binhex(bins = 20) +
    scale_fill_gradientn(colours = c("lightgrey","black")) +
    theme_bw_noy() +
    xlab(expression(n[well])) +
    ylab("OSP ($ / bbl)") +
    guides(fill = FALSE)
  if (logflag == T) {
    fnwell <- fnwell+scale_y_continuous(trans = log10_trans(),
                                        breaks = c(10^2, 10^3, 10^4, 10^5),
                                        labels = trans_format("log10", math_format(10^.x)))
  }

  ftDrill <- ggplot(r, aes(x = tDrill, y = oilSP)) +
    stat_binhex(bins = 20) +
    scale_fill_gradientn(colours = c("lightgrey","black")) +
    theme_bw_noy() +
    xlab(expression(paste(t[Drill], " (days)"))) +
    ylab("OSP ($ / bbl)") +
    guides(fill = FALSE)
  if (logflag == T) {
    ftDrill <- ftDrill+scale_y_continuous(trans = log10_trans(),
                                        breaks = c(10^2, 10^3, 10^4, 10^5),
                                        labels = trans_format("log10", math_format(10^.x)))
  }

  fcdrill <- ggplot(r, aes(x = well.cap/1e6, y = oilSP)) +
    stat_binhex(bins = 20) +
    scale_fill_gradientn(colours = c("lightgrey","black")) +
    theme_bw_noy() +
    xlab(expression(paste(C[Drill], " ($1e6)"))) +
    ylab("OSP ($ / bbl)") +
    guides(fill = FALSE)
  if (logflag == T) {
    fcdrill <- fcdrill+scale_y_continuous(trans = log10_trans(),
                                        breaks = c(10^2, 10^3, 10^4, 10^5),
                                        labels = trans_format("log10", math_format(10^.x)))
  }

  fccompl <- ggplot(r, aes(x = compl.cap/1e6, y = oilSP)) +
    stat_binhex(bins = 20) +
    scale_fill_gradientn(colours = c("lightgrey","black")) +
    theme_bw_noy() +
    xlab(expression(paste(C[Compl], " ($1e6)"))) +
    ylab("OSP ($ / bbl)") +
    guides(fill = FALSE)
  if (logflag == T) {
    fccompl <- fccompl+scale_y_continuous(trans = log10_trans(),
                                        breaks = c(10^2, 10^3, 10^4, 10^5),
                                        labels = trans_format("log10", math_format(10^.x)))
  }

  fprodL <- ggplot(r, aes(x = prodL/1e3, y = oilSP)) +
    stat_binhex(bins = 20) +
    scale_fill_gradientn(colours = c("lightgrey","black")) +
    theme_bw_noy() +
    xlab("L (1e3 ft)") +
    ylab("OSP ($ / bbl)") +
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
    xlab(expression(x[r])) +
    ylab("OSP ($ / bbl)") +
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
    xlab(expression(x[g])) +
    ylab("OSP ($ / bbl)") +
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
    xlab(expression(paste(gp," ($ / MCF)"))) +
    ylab("OSP ($ / bbl)") +
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
    ylab("OSP ($ / bbl)") +
    guides(fill = FALSE)
  if (logflag == T) {
    fIRR <- fIRR+scale_y_continuous(trans = log10_trans(),
                                        breaks = c(10^2, 10^3, 10^4, 10^5),
                                        labels = trans_format("log10", math_format(10^.x)))
  }

  multiplot(fhspace, fvspace, fangle, floc, fradius, fnrow, fnwell, ftDrill, fcdrill,
            fccompl, fprodL, frec, fxg, fgp, fIRR, cols = 4)
}

# Plot full dataset
pdf(file.path(path$plot, "Figure 11-8 Full OSP hexbin v14.pdf"), width = 11, height = 11)
multi.xyhex(r = results, logflag = TRUE)
dev.off()
setEPS(width = 11, height = 11)
postscript(file.path(path$plot, "Figure 11-8 Full OSP hexbin v14.eps"))
multi.xyhex(r = results, logflag = TRUE)
dev.off()

# Plot reduced results
pdf(file.path(path$plot, "Figure 11-9 Reduced OSP hexbin v14.pdf"), width = 11, height = 11)
multi.xyhex(r = results[results$oilSP <= price.cut,], logflag = FALSE)
dev.off()
setEPS(width = 11, height = 11)
postscript(file.path(path$plot, "Figure 11-9 Reduced OSP hexbin v14.eps"))
multi.xyhex(r = results, logflag = TRUE)
dev.off()


# Full OSP NER set
NERfull <- ggplot(results, aes(x = NER, y = oilSP)) +
  stat_binhex(bins = 20) +
  scale_fill_gradientn(colours = c("lightgrey","black")) +
  theme_bw() +
  xlab("NER") +
  ylab("OSP ($/bbl)") +
  guides(fill = FALSE) +
  scale_y_continuous(trans = log10_trans(),
                     breaks = c(10^2, 10^3, 10^4, 10^5),
                     labels = trans_format("log10", math_format(10^.x)))

pdf(file.path(path$plot, "Figure 11-10 OSP vs NER v14.pdf"))
NERfull
dev.off()
setEPS()
postscript(file.path(path$plot, "Figure 11-10 OSP vs NER v14.eps"))
NERfull
dev.off()

# Reduced OSP NER set
NERred <- ggplot(results[results$oilSP <= price.cut,], aes(x = NER, y = oilSP)) +
  stat_binhex(bins = 20) +
  scale_fill_gradientn(colours = c("lightgrey","black")) +
  theme_bw() +
  xlab("NER") +
  ylab("OSP ($/bbl)") +
  guides(fill = FALSE)

pdf(file.path(path$plot, "Figure 11-11 Reduced OSP vs NER v14.pdf"))
NERred
dev.off()
setEPS()
postscript(file.path(path$plot, "Figure 11-11 Reduced OSP vs NER v14.eps"))
NERred
dev.off()


# Boxplots for economically viable set
r <- results[results$oilSP <= price.cut,]

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

# Plot
pdf(file.path(path$plot, "Figure 11-14 Boxplot costs per bbl v14.pdf"))

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

setEPS()
postscript(file.path(path$plot, "Figure 11-14 Boxplot costs per bbl v14.eps"))

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

pdf(file.path(path$plot, "Figure 11-13 Boxplot capital costs v14.pdf"))

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

setEPS()
postscript(file.path(path$plot, "Figure 11-13 Boxplot capital costs v14.eps"))

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


# # Tables ------------------------------------------------------------------
#
# # Best scenarios by OSP - top 5 of econ parameter set
# r <- with(results, data.frame(design, LHS, oilSP, hspace, vspace, angle, loc, radius, nrow, nwell,
#                               tDrill, well.cap, prodL, rec, xg, gp, IRR))
#
# bso <- NULL
# for (i in 1:5) {
#
#   # Get economic set with lowest oilSP
#   lhsn <- r$LHS[which.min(r$oilSP)]
#
#   # Select only rows with that LHS
#   ind <- which(r$LHS == lhsn)
#   temp <- r[ind,]
#
#   # Reorder according to OSP
#   temp <- temp[order(temp$oilSP),]
#
#   bso <- rbind(bso, c(temp[1,c("LHS", "tDrill", "well.cap", "prodL", "rec", "xg", "gp", "IRR")], min(temp$oilSP), max(temp$oilSP)))
#
#   # Drop current lhsn from r
#   r <- r[-ind,]
# }
#
# # Get top 5 of geometry parameter set
# r <- with(results, data.frame(design, LHS, oilSP, hspace, vspace, angle, loc, radius, nrow, nwell,
#                               tDrill, well.cap, prodL, rec, xg, gp, IRR))
#
# bso <- NULL
# for (i in 1:5) {
#
#   # Get economic set with lowest oilSP
#   lhsn <- r$design[which.min(r$oilSP)]
#
#   # Select only rows with that LHS
#   ind <- which(r$design == lhsn)
#   temp <- r[ind,]
#
#   # Reorder according to OSP
#   temp <- temp[order(temp$oilSP),]
#
#   bso <- rbind(bso, c(temp[1,c("design", "hspace", "vspace", "angle", "loc", "radius", "nrow", "nwell")], min(temp$oilSP), max(temp$oilSP)))
#
#   # Drop current lhsn from r
#   r <- r[-ind,]
# }
