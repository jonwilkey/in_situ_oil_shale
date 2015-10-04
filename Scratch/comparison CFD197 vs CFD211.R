# Comparison of cases 197 and 211
pdf(file.path(path$plot, "Cumulative oil comparison CFD 197 vs 211.pdf"))

# Make base plot for case 197
plot(dcoil$time, dcoil[,198]/uopt$rho.oil*6.2898/5/1e3,
     type = "l",
     col = "blue",
     xlab = "Time (days)",
     ylab = "Cumulative Oil Production (1e3 bbl / m)")

# Draw line for case 211
lines(dcoil$time, dcoil[,212]/uopt$rho.oil*6.2898/5/1e3, type = "l", col = "red")

# Add legend
legend("topleft", c("CFD# 197", "CFD# 211"), lty = 1, col = c("blue", "red"))

dev.off()
