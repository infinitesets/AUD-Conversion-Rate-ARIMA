library(tidyverse)
library(lubridate)
library(tseries)
library(lmtest)
library(forecast)

df <- read.csv("C:\\Users\\seanc\\Downloads\\forEXrate data.csv")
new_df <- df[1:4774,] 
new_df$Date <- ymd(new_df$Date)
View(new_df)

australian <- select(new_df, Date, Australia.US)
australian$Australia.US <- as.numeric(as.character(australian$Australia.US))

b <- australian$Australia.US

qplot(x = Date, y = Australia.US, data = australian, geom = "line") + labs(title = "Time Series Plot of AUD to USD \nConversion Rate", y = "AUD to USD Conversion Rate") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
qplot(x = Date, y = log(Australia.US), data = australian, geom = "line")

acf(australian$Australia.US, main = "ACF Plot of AUD to USD Conversion Rate")
aus_dif <- diff(australian$Australia.US)
acf(aus_dif)
adf.test(aus_dif)
pacf(aus_dif)

model <- arima(b, order = c(0, 1, 5), fix = c(NA, NA, 0, 0, NA))

coeftest(model)
acf(model$residuals)

forecast(model, h = 5, level = 95)
