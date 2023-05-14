library(dplyr)
library(calibrate)
library(TTR)

df1     <- read.csv("auction.csv", header = T, sep=',', stringsAsFactors = FALSE)

str(df1)

c_tmp   <- c("l_close", "Cut.off", "kitta")
df_tmp  <- df1[c_tmp]
plot(df_tmp)


fit1 <- lm(Cut.off ~ l_close, data = df_tmp)
fit2 <- lm(Cut.off ~ I(l_close - mean(l_close)), data = df_tmp)
fit3 <- lm(Cut.off ~ 1 - l_close, data = df_tmp)
fit4 <- lm(Cut.off ~ I(l_close^2), data = df_tmp)
fit5 <- lm(Cut.off ~ I(l_close - 2*log10(kitta)), data = df_tmp)
fit6 <- lm(Cut.off ~ l_close:kitta, data = df_tmp)
fit7 <- lm(Cut.off ~ l_close*kitta, data = df_tmp)

plot(df1$l_close, df1$Cut.off, xlab = "Closing", ylab = "Cut Off", pch = 21, bg = 'green')
abline(fit1, col = "blue", lwd = 2)
abline(confint(fit1)[,1], col = "blue", lty = 2, lwd = 2)
abline(confint(fit1)[,2], col = "blue", lty = 2, lwd = 2)

plot(df1$l_close, df1$Cut.off, xlab = "Closing", ylab = "Cut Off", pch = 21, bg = 'green')
abline(fit5, col = "blue", lwd = 2)
abline(confint(fit5)[,1], col = "blue", lty = 2, lwd = 2)
abline(confint(fit5)[,2], col = "blue", lty = 2, lwd = 2)

res <- signif(residuals(fit1), 5)
pre <- predict(fit1) 

res <- signif(residuals(fit5), 5)
pre <- predict(fit5) 

segments(df1$l_close, df1$Cut.off, df1$l_close, pre, col="blue")
textxy(df1$l_close, df1$Cut.off, res, cex=1)


segments(df1$l_close, df1$Cut.off, df1$l_close, pre, col="blue")
textxy(df1$l_close, df1$Cut.off, res, cex=1)
