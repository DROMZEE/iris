library(shiny)
library(DT)

ui <- fluidPage(
  titlePanel("Etude du dataset Iris"),
  
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        'input.dataset === "Plot"',
        helpText("Modifier les variables"),
        
        selectInput("varX", 
                    label = "sélectionnez la variables sur l'axe X",
                    #choices = sort(names(iris)) [names(iris) != "Species"] ,
                    choices = c("Longueur des sépales" = "Sepal.Length", 
                                "Largeur des sépales" = "Sepal.Width",
                                "Largeur sépale" = "Petal.Length", 
                                "Largeur des pétales" = "Petal.Width"),
                    selected = "Sepal.Length"),
        
        selectInput("varY", 
                    label = "sélectionnez la variables sur l'axe Y",
                    choices = c("Longueur des sépales" = "Sepal.Length", 
                                "Largeur des sépales" = "Sepal.Width",
                                "Longueur des pétales" = "Petal.Length", 
                                "Largeur des pétales" = "Petal.Width"),
                    selected = "Sepal.Width"),
        
        selectInput("filter", 
                    label = "Filtre",
                    #choices = sort(names(iris)) [names(iris) != "Species"] ,
                    choices = c("Longueur des sépales" = "Sepal.Length", 
                                "Largeur des sépales" = "Sepal.Width",
                                "Longueur des pétales" = "Petal.Length", 
                                "Largeur des pétales" = "Petal.Width"),
                    selected = "Sepal.Length"),
        
        sliderInput("range", 
                    label = "Fourchettes retenues",
                    min = 0, max = 10, value = c(0, 10)),
        
        actionButton("update", "afficher")
      ),
      
      conditionalPanel(
        'input.dataset === "Table"',
        helpText("Affiche 5 lignes par défaut."),
        checkboxGroupInput("show_varsIris", "séléectionnez les colonnes à afficher",
                           names(iris), selected = names(iris))
      ),
      
      conditionalPanel(
        'input.dataset === "Calcul"',
        selectInput("calcul", 
                    label = "sélectionnez la variable sur laquelle calculer la moyenne.",
                    #choices = sort(names(iris)) [names(iris) != "Species"] ,
                    choices = c("Longueur des sépales" = "Sepal.Length", 
                                "Largeur des sépales" = "Sepal.Width",
                                "Longueur des pétales" = "Petal.Length", 
                                "Largeur des pétales" = "Petal.Width"),
                    selected = "Sepal.Length")
      )
      
      
      
    ),
    
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel("Plot",  plotOutput("Plot1")),
        tabPanel("Table", DT::dataTableOutput("mytable")),
        tabPanel("Calcul", textOutput("selected_var")),
        
        textOutput("selected_var"),
        textOutput("min_max")
      )
      
    )
  )
)


server <- function(input, output) {
  
  output$selected_var <- renderText({ 
    paste("La moyenne des ", input$calcul, "est de : ", mean(iris[[input$calcul]]))
  })
  
  output$min_max <- renderText({ 
    paste("You have chosen a range that goes from",
          input$range[1], "to", input$range[2])
  })
  
  
  updateAll <- eventReactive(input$update, {
    iris <- iris[(iris[[input$filter]] >= input$range[1])  & (iris[[input$filter]] <= input$range[2]) ,]
    
    plot(iris[[input$varX]] , iris[[input$varY]], col = iris$Species ,
         xlab = input$varX,
         ylab = input$varY,
         
         main = "my super plot")
  })
  #output for onlet Plot
  output$Plot1 <- renderPlot({
    
    updateAll()
    
    
  })
  output$mytable <- DT::renderDataTable({
    DT::datatable(iris[, input$show_varsIris, drop = FALSE], options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  })
  
  
}

shinyApp(ui, server)

