# Load required *.csv and *.rda data files
data <- read.csv(file.path(path$raw, "hD&C costs.csv"))
load("D:/Dropbox/CLEAR/DOGM Data/Prepared Data/production_v5.rda")
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

# Subset to just drilling and completion
drill <- subset(data, subset = (adcost >=0))
compl <- subset(data, subset = (accost >=0))
compl$ll <- compl$depth - compl$top

# Fit results
cdrill <- lm(log(adcost)~depth, drill)
ccompl <- lm(accost~ll, compl)

# Plot results
x <- seq(1e3, 13e3, 100)
yd <- exp(cdrill$coefficients[2]*x+cdrill$coefficients[1])
yc <- ccompl$coefficients[2]*x+ccompl$coefficients[1]

# Drilling
pdf(file.path(path$BCfig, "Figure 11-3 Drill cost.pdf"))
plot(drill$depth, drill$adcost/1e6,
     #xlim = c(7e3, 12e3),
     #ylim = c(0, 4e3),
     xlab = "Well Length (feet)",
     ylab = "Drilling Cost (millions of 2014 USD)")
lines(x, yd/1e6, col = "red")
dev.off()

# Completion
pdf(file.path(path$BCfig, "Figure 11-4 Completion cost.pdf"))
plot(compl$depth-compl$top, compl$accost/1e6,
     #xlim = c(3e3, 12e3),
     #ylim = c(0, 8),
     xlab = "Lateral Length (feet)",
     ylab = "Completion Cost (millions of 2014 USD)")
lines(x, yc/1e6, col = "red")
dev.off()

# Save fit
save(file = file.path(path$data, "hdrill.rda"), list = c("cdrill", "ccompl"))
