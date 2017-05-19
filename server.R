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
             
             sliderInput("sports", "Ваше отношение к спорту:" , min= 1, max =10, value = 5),
             sliderInput("tvsports", "Ваше отношение к спорту:" , min= 1, max =10, value = 5),
             sliderInput("excersice", "Ваше отношение к спорту:" , min= 1, max =10, value = 5),
             sliderInput("dining", "Ваше отношение к спорту:" , min= 1, max =10, value = 5),
             sliderInput("museums", "Ваше отношение к спорту:" , min= 1, max =10, value = 5),
             sliderInput("art", "Ваше отношение к спорту:" , min= 1, max =10, value = 5),
             sliderInput("hiking", "Ваше отношение к спорту:" , min= 1, max =10, value = 5),
             sliderInput("gaming", "Ваше отношение к спорту:" , min= 1, max =10, value = 5),
             sliderInput("clubbing", "Ваше отношение к спорту:" , min= 1, max =10, value = 5),
             sliderInput("reading", "Ваше отношение к спорту:" , min= 1, max =10, value = 5),
             sliderInput("tv", "Ваше отношение к спорту:" , min= 1, max =10, value = 5),
             sliderInput("theater", "Ваше отношение к спорту:" , min= 1, max =10, value = 5),
             sliderInput("movies", "Ваше отношение к спорту:" , min= 1, max =10, value = 5),
             sliderInput("concerts", "Ваше отношение к спорту:" , min= 1, max =10, value = 5),
             sliderInput("music", "Ваше отношение к спорту:" , min= 1, max =10, value = 5),
             sliderInput("shopping", "Ваше отношение к спорту:" , min= 1, max =10, value = 5),
             sliderInput("yoga", "Ваше отношение к спорту:" , min= 1, max =10, value = 5)
             ),
  
  tabPanel("Рекомендованные",
           
           selectInput(inputId = "char1", label = "Фильтр 1", choices = c("none","sports","tvsports","excersice","dining","museums","art","hiking","gaming","clubbing","reading","tv","theater","movies","concerts","music","shopping","yoga")),
           sliderInput(inputId = "char1_range", label = "Ограничения для фильтра 1" , min= 1, max =10, value = c(4,6) ),
           selectInput(inputId = "char2", label = "Фильтр 2", choices = c("none","sports","tvsports","excersice","dining","museums","art","hiking","gaming","clubbing","reading","tv","theater","movies","concerts","music","shopping","yoga")),
           sliderInput(inputId = "char2_range", label = "Ограничения для фильтра 2" , min= 1, max =10, value = c(4,6) ),
           selectInput(inputId = "char3", label = "Фильтр 3", choices = c("none","sports","tvsports","excersice","dining","museums","art","hiking","gaming","clubbing","reading","tv","theater","movies","concerts","music","shopping","yoga")),
           sliderInput(inputId = "char3_range", label = "Ограничения для фильтра 3" , min= 1, max =10, value = c(4,6) ),
           
           tableOutput(outputId = "table")
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
            excersice = input$excersice,
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
              
        SpeedDating <- read_csv("SpeedDating.csv")
        SD2 <- select(SpeedDating, iid, age, gender, career_c, sports, tvsports, exercise, dining, museums, art, hiking, gaming, clubbing, reading, tv, theater, movies, concerts, music, shopping, yoga, attr3_1, sinc3_1, intel3_1, fun3_1, amb3_1)
        SD2NA <- na.omit(unique(SD2))
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
        SD2NANA2 = SD2NA %>% filter(gender == m , age >= n-3,  age <= n+3) %>% select( -age, -gender, -career_c)
        SD2NANA2 <- as.data.frame(SD2NANA2)
        rownames(SD2NANA2) = SD2NANA2$iid
        SD2NANA2_0 = select(SD2NANA2, -iid)
        big <- as.matrix(SD2NANA2_0)
        new <-t(new)[,1]
        big <- abs(big-new)
        big <- as.data.frame(big)
        big$sum<-rowSums(big)
        big$iid <- SD2NANA2$iid
        antitop <- head(arrange(big, sum), 10)
        people_like_me <- antitop$iid
        # кто понравился таким же как я 
        SDFlike7_1 = filter(SDFlike7, iid %in% people_like_me )
        SDFdec_1 = filter(SDFdec1, iid %in% people_like_me )
        # кандинаты по like
        SD22NAlp = filter(SD22NA, iid %in% SDFlike7_1$pid)
        
        SD22NAlp <- filter(SD22NAlp, SD22NAlp[input$char1] > input$char1_range[1] & SD22NAlp[input$char1] < input$char1_range[2])
        #if()
        SD22NAlp <- filter(SD22NAlp, SD22NAlp[input$char1] > input$char1_range[1] & SD22NAlp[input$char1] < input$char1_range[2])
        SD22NAlp <- filter(SD22NAlp, SD22NAlp[input$char1] > input$char1_range[1] & SD22NAlp[input$char1] < input$char1_range[2])
        
        SD22NAlp <- arrange()
        
        SD22NAlp
        
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)