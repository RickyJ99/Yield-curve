library(ecb)
library(xts)
filter1 = list(startPeriod = "2020-06", endPeriod = "2021-06")
sp          <-  matrix(NA, nrow = 277, ncol = 10)
keys        <-  "YC.B.U2.EUR.4F.G_N_A.SV_C_YM.SR_"
colnamess   <- c()
for (count in 1:10) {
    keys_count          <- paste(keys, count, sep = "")
    keys_count          <- paste(keys_count, "Y", sep = "")
    data                <- get_data(key = keys_count,filter = filter1)
    data$obstime        <- convert_dates(data$obstime)
    colname             <- paste(count, "Y")
    sp[,count]          <- data$obsvalue
    colnamess           <- cbind(colnamess, colname)
}
colnames(sp)            <- colnamess
#sp      <- xts(sp,order.by = data$obstime)

fw          <-  matrix(NA, nrow = 277, ncol = 10)

keys        <-  "YC.B.U2.EUR.4F.G_N_A.SV_C_YM.IF_"
colnamess   <- c()
for (count in 1:10) {
    keys_count          <- paste(keys, count, sep = "")
    keys_count          <- paste(keys_count, "Y", sep = "")
    data                <- get_data(key = keys_count,filter = filter1)
    data$obstime        <- convert_dates(data$obstime)
    colname             <- paste(count, "Y")
    fw[,count]          <- data$obsvalue
    colnamess           <- cbind(colnamess, colname)
}
colnames(fw)            <- colnamess
#fw      <- xts(fw,order.by = data$obstime)
plot(fw[1,], type= "l")
lines(sp[1, ])
hicp$obstime <- convert_dates(hicp$obstime)
str(hicp)