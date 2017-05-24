library(readr)
library(dplyr)
load("dataframes.RData")
# ввод значений
m= 1
n= 22
SD2NANA1 = filter(SD2NA, gender == m , age >= n-3,  age <= n+3)
SD2NANA2 = select(SD2NANA1, -age, -gender, -career_c)
SD2NANA2 <- as.data.frame(SD2NANA2)
rownames(SD2NANA2) = SD2NANA2$iid # дублирую iid в порядковый столбец (который не используется при расчетах)
SD2NANA2_0 = select(SD2NANA2, -iid) 
# ввод характеристик
new <- tail(SD2NANA2_0, n=1)
new <- as.matrix(new)
new <-t(new)[,1] #  транспонирую матрицу new --> получаю из строчки вектор

big <- as.matrix(SD2NANA2_0) %>% t() # транспонирую матрицу big
#new1 <-as.data.frame(new)
big <-t(abs(big-new))
big <- as.data.frame(big)
big$sum<-rowSums(big)
big$iid = SD2NANA2$iid
antitop = head (arrange(big, sum), 10)
SDFdec_all = filter(SDFdec, iid %in% antitop$iid )
SDFdec_1 = filter(SDFdec1, iid %in% antitop$iid )
SD22NA1dp = filter(SD22NA, iid %in% SDFdec_1$pid)
SD22NAall = filter(SD22NA, iid %in% SDFdec_all$pid)
SD22NAall1 = mutate(SD22NAall, deci = ifelse(iid %in% SDFdec_1$pid, 1, 0))
SD22NAall1$deci= as.factor(SD22NAall1$deci)
# предсказание значений
test = if (m==0) {
  filter(SD22NA,gender == 1)
} else {
  filter(SD22NA, gender == 0)
}
main = SD22NAall1
library(rpart)
treeall1=rpart(deci~.-deci-iid-gender,main)
library(caret)
rpartPred <- predict(treeall1, main, type = "class")
rpartPred <- predict(treeall1, test, type = "class")
rpartPred = as.data.frame(rpartPred)
rownames(rpartPred) = test$iid
rpartPred$iid = test$iid
library("e1071")
svm_model <- svm(deci~.-deci-iid-gender, data=main, kernel="linear")
svm.Pred<-predict(svm_model, main, probability=FALSE)
svm.Pred<-predict(svm_model, test, probability=FALSE)
svm.Pred = as.data.frame(svm.Pred)
rownames(svm.Pred) = test$iid
svm.Pred$iid = test$iid
library("e1071")
svm_modelpoly <- svm(deci~.-deci-iid-gender, data=main, kernel="polynomial")
svmpoly.Pred<-predict(svm_modelpoly, main, probability=FALSE)
svmpoly.Pred<-predict(svm_modelpoly, test, probability=FALSE)
svmpoly.Pred = as.data.frame(svmpoly.Pred)
rownames(svmpoly.Pred) = test$iid
svmpoly.Pred$iid = test$iid
library(randomForest)
rfModel <-randomForest(deci~.-deci-iid-gender, data=main)
rfPred<-predict(rfModel, main, probability=FALSE)
rfPred<-predict(rfModel, test, probability=FALSE)
rfPred = as.data.frame(rfPred)
rownames(rfPred) = test$iid
rfPred$iid = test$iid
predictall = inner_join(rpartPred,svm.Pred, by = "iid")
predictall = inner_join(predictall,svmpoly.Pred, by = "iid")
predictall = inner_join(predictall,rfPred, by = "iid")
predictall = predictall[c(2,1,3,4,5)]
predictall$rpartPred = as.numeric(predictall$rpartPred)
predictall$svm.Pred = as.numeric(predictall$svm.Pred)
predictall$svmpoly.Pred = as.numeric(predictall$svmpoly.Pred)
predictall$rfPred = as.numeric(predictall$rfPred)
pred = c("rpartPred", "svm.Pred", "svmpoly.Pred", "rfPred")
predictall1 = mutate(predictall, mean = rowMeans(predictall[pred]))
predictall2 = filter(predictall1, mean==2)
done = filter(SD22NA, iid %in% predictall2$iid)
