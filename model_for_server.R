# Speed Dating - это встреча незнакомых людей, которые в течение некоторого времени (обычно не больше пары минут), общаются с людьми 
# противоположного пола, по результатам встречи решая, хотят ли они встретиться еще раз с этим человекаом или нет. 
# Цель нашего проекта - создание такой рекомендательной системы, которая выдает список возможных партнеров, которые должны понравится пользователю, который ввел свои данные.
# Для этой цели среди всей базы участников, после введения пользователем своих данных (пол, возраст), находятся "двойники", т.е. люди такого же пола и в возрасте +- 3 года.
# После этого, отбираются те, кому эти двойники сказали "да" (решение о дальнейшей встрече), выводятся полный их список и список тех участников, кому двойники сказали бы "да" (в случае, если все 4 модели подтвердили этот прогноз).
# потом - пользователь может отфильтровать полученные результаты, задав важность ряда характеристик у будущего партнера. Программа смотрит отклонения по заданным значениям характеристик у рекомендуемых партнеров, и выводит тех, кто наиболее подходит по всем характеристикам (имеет меньше отклонений).
library(readr)
library(dplyr)

SpeedDating <- read_csv("SpeedDating.csv")
# Выбираем из списка нужные характеристики (включаем), убираем пропуски. Данные для наших двойников
SD2 <- select(SpeedDating, iid, age, gender, career_c, sports, tvsports, exercise, dining, museums, art, hiking, gaming, clubbing, reading, tv, theater, movies, concerts, music, shopping, yoga, attr3_1, sinc3_1, intel3_1, fun3_1, amb3_1)
SD2NA <- na.omit(unique(SD2))
# Создаем базу данных с расширенным характеристиков противоположного пола.

SD22 <- select(SpeedDating, iid, gender, age, field_cd, race, imprace, imprelig, zipcode, goal, date, go_out, career_c, sports, tvsports, exercise, dining, museums, art, hiking, gaming, clubbing, reading, tv, theater, movies, concerts, music, shopping, yoga, exphappy, attr3_1, sinc3_1, intel3_1, fun3_1, amb3_1 )
SD22NA <- na.omit(unique(SD22))

# решение людей и их лайки
SDFdec = select(SpeedDating, iid, pid, dec)
SDFdec = na.omit(SDFdec)
SDFdec$dec=as.numeric(SDFdec$dec)
SDFlike = select (SpeedDating, iid, pid, like)
SDFlike = na.omit(SDFlike)
SDFlike$like = as.numeric(SDFlike$like)
# фильтр положительных решений
SDFdec1 = filter(SDFdec, dec>0)
SDFlike7= filter(SDFlike, like>7)

#представим, что мы девушка 22 лет. Выведем 10 тех, кто понравился моим двойникам.
m= 0
n= 22
SD2NANA2 = SD2NA %>% filter(gender == m , age >= n-3,  age <= n+3) %>% select( -age, -gender, -career_c)
SD2NANA2 <- as.data.frame(SD2NANA2)
rownames(SD2NANA2) = SD2NANA2$iid 
SD2NANA2_0 = select(SD2NANA2, -iid) 
new <- tail(SD2NANA2_0, n=1) 
new <- as.matrix(new)
big <- as.matrix(SD2NANA2_0)
new <-t(new)[,1]
new1 <-as.data.frame(new)
big <- abs(big-new)
big <- as.data.frame(big)
big$sum<-rowSums(big)
big$iid <- SD2NANA2$iid
antitop <- head(arrange(big, sum), 10)
# список похожих на меня 
people_like_me <- antitop$iid

# кто понравился таким же как я (по решению, 1 - нравится)
SDFlike7_1 = filter(SDFlike7, iid %in% people_like_me )
SDFlike7_1$pid
SDFdec_1 = filter(SDFdec1, iid %in% people_like_me )
SDFdec_1$pid

# кандинаты по like
SD22NAlp = filter(SD22NA, iid %in% SDFlike7_1$pid)

# кандидаты по decision
SD22NA1dp = filter(SD22NA, iid %in% SDFdec_1$pid)
