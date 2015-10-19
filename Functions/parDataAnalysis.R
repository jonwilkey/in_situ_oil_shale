# Function Info -----------------------------------------------------------
# Name:       parDataAnalysis.R (Input parameter data analysis script)
# Author(s):  Jon Wilkey
# Contact:    jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# NA - runs as script


# Outputs -----------------------------------------------------------------

# cdrill - fit for capital cost of drilling

# ccompl - fit for capital cost of completion

# Plots - of drilling and capital cost fits


# Description -------------------------------------------------------------

# This script loads the DOGM production database and *.csv file exported from
# well reports at:

# https://docs.google.com/spreadsheets/d/1mZ0RNMPnydX1Ql4Pv7K_DyTRI276AOG333bssy_svB8/edit?usp=sharing

# The function then adjusts costs for inflation, builds a set of costs for which
# both completion and drilling costs are available, determines the amount of
# time that has elapsed between spudding and first completion for all
# horizontally drilled wells in Utah since 2010, finds the minimum and maximum
# wellhead gas prices, and then plots and exports the statistics related to
# those results.


# Drilling and Completion Costs -------------------------------------------

# Load required *.csv and *.rda data files
data <- read.csv(file.path(path$raw, "hD&C costs.csv"))
load(file.path(path$data, "production_v5.rda"))
load(file.path(path$data, "cpi.rda"))

# Change names
names(data) <- c("api", "field", "county", "welltype", "depth", "operator",
                 "dcost", "ccost", "top")

# Get completion dates for each well in Basin and merge with data
compl <- sqldf("select distinct p_api, h_compl_date
               from production
               where w_dir_horiz = 'H'")

# Rename
names(compl) <- c("p_api", "rdate")

# Round to nearest month
compl$rdate <- as.Date(as.yearmon(compl$rdate))

# Merge with CPI data
data <- merge(x = data, y = compl, by.x = "api", by.y = "p_api", all.x = T)
temp <- data.frame(date = as.Date(index(cpi.z)), cpi = coredata(cpi.z))
data <- merge(x = data, y = temp, by.x = "rdate", by.y = "date")

# Adjust for inflation
data$adcost <- data$dcost*(uopt$cpi/data$cpi)
data$accost <- data$ccost*(uopt$cpi/data$cpi)

# Calculate horizonal length as "ll"
data$ll <- with(data, depth-top)

# Get subset of wells that have both drilling and completion costs
data <- subset(data, subset = (adcost >=0 & accost >= 0))


# tDrill ------------------------------------------------------------------

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


# Gas wellhead prices -----------------------------------------------------

# Load EIA price history file
load(file.path(path$data, "EIAprices_v5.rda"))

# Subset price range
eia.hp <- subset(eia.hp, subset = (as.Date(month) >= as.Date("2010-01-01")))


# Plots -------------------------------------------------------------------

# Drilling Time
pdf(file.path(path$BCfig, "Figure 11-2 DCT vs Well Length.pdf"))
plot(dt~h_td_md, p[p$h_td_md >0,],
     xlim = c(6e3, 20e3),
     ylim = c(0, 700),
     xlab = "Well Length (feet)",
     ylab = "Drilling and Completion Time (days)")
grid(lty = 1)
dev.off()

# Drilling & Completion Costs
pdf(file.path(path$BCfig, "Figure 11-3 Well Cost.pdf"))
plot(data$depth, (data$adcost+data$accost)/1e6,
     xlim = c(7e3, 12e3),
     ylim = c(1, 7),
     xlab = "Well Length (feet)",
     ylab = "Well Drilling and Completion Cost (millions of 2014 USD)")
grid(lty = 1)
dev.off()

# Wellhead gas price history
pdf(file.path(path$BCfig, "Figure 11-4 Gas wellhead prices.pdf"))
plot(GP~month, eia.hp,
     type = "l",
     #xlim = c(7e3, 12e3),
     #ylim = c(0, 6),
     xlab = "Date (months)",
     ylab = "Utah Wellhead Gas Price (2014 USD/MCF)")
grid(lty = 1)
dev.off()


# Export min/max results for LHS ------------------------------------------

statR <- data.frame(depth.min =  min(p$h_td_md[p$h_td_md > 0], na.rm = T),
                    depth.max =  max(p$h_td_md[p$h_td_md > 0], na.rm = T),
                    cost.min =   min(with(data, accost+adcost)),
                    cost.max =   max(with(data, accost+adcost)),
                    tdrill.min = min(p$dt[p$h_td_md >0]),
                    tdrill.max = max(p$dt[p$h_td_md >0]),
                    gp.min =     min(eia.hp$GP),
                    gp.max =     max(eia.hp$GP))

# Save fit
save(file = file.path(path$data, "hdrill.rda"), list = c("statR"))
