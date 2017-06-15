library(shiny)
library(readr)
library(dplyr)

ui <- fluidPage(
  
  headerPanel("Speed Dating"),
  
  tabsetPanel(
    tabPanel("Ваши характеристики",
             
             titlePanel("Заполните информацию с себе"),
             
             numericInput('age', 'Age', value=22,
                          min = 1, max = 100, step=1),
             radioButtons('gender', "Gender", 
                          c("Male" = 1,"Female" = 0)),
             
             titlePanel("Как вы относитесь к указаным видам деятельности. Оцените по шкале от 1 до 10."),
             tags$h3("1- абсолютно не важно, 10 - очень важно"),
             sliderInput("sports", "Ваше отношение к спорту:" , min= 1, max =10, value = 5),
             sliderInput("tvsports", "Ваше отношение к просмотру спортивных событий по ТВ:" , min= 1, max =10, value = 5),
             sliderInput("exercise", "Ваше отношение к физическим упражнениям:" , min= 1, max =10, value = 5),
             sliderInput("dining", "Ваше отношение к ужину вне дома:" , min= 1, max =10, value = 5),
             sliderInput("museums", "Ваше отношение к музеям:" , min= 1, max =10, value = 5),
             sliderInput("art", "Ваше отношение к искусству:" , min= 1, max =10, value = 5),
             sliderInput("hiking", "Ваше отношение к пешиму туризму/альпинизму:" , min= 1, max =10, value = 5),
             sliderInput("gaming", "Ваше отношение к компьютерным играм:" , min= 1, max =10, value = 5),
             sliderInput("clubbing", "Ваше отношение к посещению ночных клубов:" , min= 1, max =10, value = 5),
             sliderInput("reading", "Ваше отношение к чтению книг:" , min= 1, max =10, value = 5),
             sliderInput("tv", "Ваше отношение к просмотру телевизора:" , min= 1, max =10, value = 5),
             sliderInput("theater", "Ваше отношение к театру:" , min= 1, max =10, value = 5),
             sliderInput("movies", "Ваше отношение к кинотеатру:" , min= 1, max =10, value = 5),
             sliderInput("concerts", "Ваше отношение к концертам:" , min= 1, max =10, value = 5),
             sliderInput("music", "Ваше отношение к музыке:" , min= 1, max =10, value = 5),
             sliderInput("shopping", "Ваше отношение к шопингу:" , min= 1, max =10, value = 5),
             sliderInput("yoga", "Ваше отношение к йоге:" , min= 1, max =10, value = 5)
             ),
  
  tabPanel("Рекомендованные",
           sidebarLayout(
             sidebarPanel(
          
             selectInput(inputId = "char1", label = "Фильтр 1", choices = c("none","sports","tvsports","exercise","dining","museums","art","hiking","gaming","clubbing","reading","tv","theater","movies","concerts","music","shopping","yoga")),
           sliderInput(inputId = "char1_range", label = "Ограничения для фильтра 1" , min= 1, max =10, value = c(4,6) ),
           selectInput(inputId = "char2", label = "Фильтр 2", choices = c("none","sports","tvsports","exercise","dining","museums","art","hiking","gaming","clubbing","reading","tv","theater","movies","concerts","music","shopping","yoga")),
           sliderInput(inputId = "char2_range", label = "Ограничения для фильтра 2" , min= 1, max =10, value = c(4,6) ),
           selectInput(inputId = "char3", label = "Фильтр 3", choices = c("none","sports","tvsports","exercise","dining","museums","art","hiking","gaming","clubbing","reading","tv","theater","movies","concerts","music","shopping","yoga")),
           sliderInput(inputId = "char3_range", label = "Ограничения для фильтра 3" , min= 1, max =10, value = c(4,6) )
             ),
           mainPanel(
             tableOutput(outputId = "table")
           )
           )
           )
  )
)


server <- function(input, output) {
    
    # создание таблицы для ввода данных
    output$table <- renderTable({
        m <- as.numeric(input$gender)
        n <- as.numeric(input$age)

        new <- as.matrix(data.frame(
            sports = input$sports,
            tvsports = input$tvsports,
            exercise = input$exercise,
            dining = input$dining,
            museums = input$museums,
            art = input$art,
            hiking = input$hiking,
            gaming = input$gaming,
            clubbing = input$clubbing,
            reading = input$reading,
            tv = input$tv,
            theater = input$theater,
            movies = input$movies,
            concerts = input$concerts,
            music = input$music,
            shopping = input$shopping,
            yoga = input$yoga
          ))
        new <-t(new)[,1]
        
        # обработка данных
        
        library(readr)
        library(dplyr)
        load("dataframes.RData")
        SD2NANA1 = filter(SD2NA, gender == m , age >= n-3,  age <= n+3)
        SD2NANA2 = select(SD2NANA1, -age, -gender, -career_c)
        SD2NANA2 <- as.data.frame(SD2NANA2)
        rownames(SD2NANA2) = SD2NANA2$iid # дублирую iid в порядковый столбец (который не используется при расчетах)
        SD2NANA2_0 = select(SD2NANA2, -iid) 
        big <- as.matrix(SD2NANA2_0) %>% t() # транспонирую матрицу big
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
        
        # фильтрация значений по предпочитаемым характеристикам
        if (input$char1 != "none"){
          done <- filter(done, done[input$char1] >= input$char1_range[1] & done[input$char1] <= input$char1_range[2])
          if(input$char2 != "none" & nrow(done) >= 0){
            done <- filter(done, done[input$char2] > input$char2_range[1] & done[input$char2] <= input$char2_range[2])
            if(input$char3 != "none" & nrow(done) >= 0){
              done <- filter(done, done[input$char3] > input$char3_range[1] & done[input$char3] <= input$char3_range[2])
            }
          }  
        }
        vari = apply(done, 2, FUN = var)
        done1 = rbind(done, apply(done, 2, FUN = var))
        done2 = done1[nrow(done1),]
        done3 = done2[,-c(1,2)]
        done3 = as.data.frame(t(done3))
        done3 = mutate(done3, name = rownames(done3))
        done4 = arrange(done3, V1)
        stable = head(done4$name,10) # если закончить на этом моменте, то только набор харакетиристик
        done5 = done[, names(done) %in% stable] # если закончить на этом моменте, то набор харакетиристик и показатель
        done6 = rbind(done5, apply(done5, 2, FUN = median))
       done6
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
