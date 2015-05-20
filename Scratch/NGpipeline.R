# Input cost data
data <- data.frame(cpi =  c(232.957, 207.342, 207.342, 207.342, 195.3, 188.9, 184, 177.1),
                   cost = c(14.8, 108, 61, 60, 54.6, 16, 13, 81),
                   dist = c(14.7, 59,  52, 72, 18.7, 13, 17, 76),
                   dia =  c(16,   24,  24, 24, 24,   20, 24, 24))

# Inflation adjust
data$acost <- data$cost*(uopt$cpi/data$cpi)

# Get per mile cost
data$pm <- with(data, acost/dist)

# ... and per mile per inch diameter cost
data$pmd <- with(data, pm/dia)
