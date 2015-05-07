# Load drilling cost data
data <- read.csv(file.path(path$raw, "hdrill costs.csv"))

# Change variable types
data <- with(data, data.frame(api =       API,
                              field =     Field,
                              county =    County,
                              welltype =  Well.Type,
                              depth =     Measured.Depth..ft.,
                              firstprod = as.Date(First.Production),
                              operator =  Operator,
                              tstart =    as.Date(Start.Date),
                              tstop =     as.Date(Finish.Date),
                              cost =      Cost))

# Load CPI data (cpi.z)
load(file.path(path$data, "cpi.rda"))

# Calculate time difference start to finish for drilling
data$dt <- as.numeric(difftime(data$tstop, data$tstart, units = "days"))

# Merge with CPI data
data$rdate <- as.Date(as.yearmon(data$tstop))
temp <- data.frame(date = index(cpi.z), cpi = coredata(cpi.z))
data <- merge(x = data, y = temp, by.x = "rdate", by.y = "date")

# Adjust for inflation
data$acost <- data$cost*(opt$cpi/data$value)

# Plot a fit
test <- lm(log(acost)~depth, data)
x <- seq(7e3, 12e3, 100)
y <- exp(x*test$coefficients[2]+test$coefficients[1])

pdf(file.path(path$plot, "HDrill Cost Fit.pdf"))
plot(acost~depth, data,
     xlab = "Measured Depth (ft)",
     ylab = "Drilling Cost (2014 USD)",
     main = "Drilling Costs as f(depth)")
lines(x, y, col = "red")
mtext(paste("Fit: y = a*exp(depth)+b, a = ",
            round(test$coefficients[2], 6),
            ", b = ",
            round(test$coefficients[1], 2),
            ", R^2 = ",
            round(summary(test)$r.squared, 4), sep = ""))
dev.off()