library(shiny)


ui <- fluidPage(
  titlePanel("Заполните информацию с себе"),
  numericInput('age', 'Age', value=0,
               ,min = 1, max = 100, step=1),
  
  radioButtons('gender', "Gender", 
               c(
                 "Male" = 1,
                 "Female" = 0
               )),
  
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
  sliderInput("yoga", "Ваше отношение к спорту:" , min= 1, max =10, value = 5),
  
  tableOutput(outputId = "table") 
)



server <- function(input, output) {
  
    output$table <- renderTable({
      data.frame(
        gender = input$gender,
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
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)