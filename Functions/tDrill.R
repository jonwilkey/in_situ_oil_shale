# Load production data from CLEAR folder
load("D:/Dropbox/CLEAR/DOGM Data/Prepared Data/production_v5.rda")

# Use SQL query to get subset of horizontal wells
p <- sqldf("select distinct p_api, h_spud_dry, h_compl_date, h_td_md
           from production
           where w_dir_horiz = 'H'")

# Only wells within last 5 years
p <- p[p$h_spud_dry >= as.Date("2010-01-01"),]

# Calculate time difference
p$dt <- as.numeric(difftime(p$h_compl_date, p$h_spud_dry, units = "days"))

# Drop rows with NA or negative values on dt
p <- p[p$dt >= 0,]
p <- p[!is.na(p$dt),]

# Calculate median full data set
tDrill <- round(median(p$dt, na.rm = T))

# Plot to PDF
pdf(file.path(path$BCfig, "Figure 11-2 DCT vs Well Length.pdf"))
plot(dt~h_td_md, p[p$h_td_md >0,],
     xlim = c(6e3, 20e3),
     ylim = c(0, 700),
     xlab = "Well Length (feet)",
     ylab = "Drilling and Completion Time (days)")
dev.off()

# Save result
save(file = file.path(path$data, "tDrill.rda"), tDrill)
