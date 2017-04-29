Как здесь сделать импут?
library(readr)
SpeedDating <- read_csv("~/pr3/project1/SpeedDating.csv")

library(dplyr)
SD2 = select(SpeedDating, iid, age, gender, career_c, sports, tvsports, exercise, dining, museums, art, hiking, gaming, clubbing, reading, tv, theater, movies, concerts, music, shopping, yoga, attr3_1, sinc3_1, intel3_1, fun3_1, amb3_1)
SD2un = unique(SD2)
SD2NA = na.omit(SD2un)
SD22 = select(SpeedDating, iid, gender, age, field_cd, race, imprace, imprelig, zipcode, goal, date, go_out, career_c, sports, tvsports, exercise, dining, museums, art, hiking, gaming, clubbing, reading, tv, theater, movies, concerts, music, shopping, yoga, exphappy, attr3_1, sinc3_1, intel3_1, fun3_1, amb3_1 )

SD22un = unique(SD22)
SD22NA = na.omit(SD22un)

SDFdec = select (SpeedDating, iid, pid, dec)
SDFdec = na.omit(SDFdec)
SDFdec$dec=as.numeric(SDFdec$dec)

SDFlike = select (SpeedDating, iid, pid, like)
SDFlike = na.omit(SDFlike)
SDFlike$like = as.numeric(SDFlike$like)

SDFlike7= filter(SDFlike, like>7)

SDFdec1 = filter(SDFdec, dec>0)

Я не знаю как они у вас называются в системе инпута!
m= 0
n= 22

SD2NANA1 = filter(SD2NA, gender == m , age >= n-3,  age <= n+3)

SD2NANA2 = select(SD2NANA1, -age, -gender, -career_c)
SD2NANA2 <- as.data.frame(SD2NANA2)
rownames(SD2NANA2) = SD2NANA2$iid 
SD2NANA2_0 = select(SD2NANA2, -iid) 
new <- tail(SD2NANA2_0, n=1) 
new <- as.matrix(new)
big <- as.matrix(SD2NANA2_0)
  t() 
new <-t(new)[,1] 
new1 <-as.data.frame(new)
big <-t(abs(big-new))
big <- as.data.frame(big)
big$sum<-rowSums(big)
big$iid = SD2NANA2$iid

antitop = head (arrange(big, sum), 10)
antitop$iid

SDFlike7_1 = filter(SDFlike7, iid %in% antitop$iid )
SDFlike7_1$pid

SDFdec_1 = filter(SDFdec1, iid %in% antitop$iid )
SDFdec_1$pid

SD22NAlp = filter(SD22NA, iid %in% SDFlike7_1$pid)

SD22NA1dp = filter(SD22NA, iid %in% SDFdec_1$pid)
