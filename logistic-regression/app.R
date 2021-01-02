library(shiny)
library(lattice)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Maximum likelihood showcase"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
           numericInput("beta_0",
                        "Value of beta 0",
                        min = -100,
                        max = 100,
                        value=0,
                        step = 0.1),
        
            numericInput("beta_1",
                        "Value of beta 1",
                        min = -100,
                        max = 100,
                        value=0,
                        step = 0.1),
           
           sliderInput("beta_0_range",
                       "Range for beta_0",
                       min=-20,
                       max=20,
                       value=c(-20, 20)
                       ),
           
           sliderInput("beta_1_range",
                       "Range for beta_1",
                       min=-20,
                       max=20,
                       value=c(-20, 20)
           ),
           
           sliderInput("granularity",
                       "Granularity of grid",
                       min=10,
                       max=50,
                       value=15
           )
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("inputPlot"),
           plotOutput("MLplot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    X <<- c(2, 2.1, 5, 20, 30, 1.5, 22, 3, 5, 10, 25, 23)
    Y <<- c(0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1)
    Xseq <<- seq(min(X), max(X), length=100)
    
    logisticScore <- function(X, beta0, beta1){
        return(1/( 1 + exp(-1 * (beta0 + beta1 * X))))
    }
    
    MLscore <- function(Y, X, beta0, beta1, eps=0.00001){
        # Getting the logistic value output
        score = logisticScore(X, beta0, beta1)
        
        # Getting the other event score
        score_inverse = 1 - score
        
        # Getting the logs
        score_log = log(score + eps)
        score_inverse_log = log(score_inverse + eps)
        
        # Multiplying with Y 
        product_1 = Y * score_log
        product_2 = (1 - Y) * score_inverse_log
        
        # Adding and summing each coordinates
        return(sum(product_1 + product_2))
    }
    
    output$inputPlot <- renderPlot({
        # Plotting the data 
        plot(X, Y, pch=21, col='black', bg='steelblue', ylim=c(-0.5, 1.5), cex=2, main="Logistic curve")
        abline(h=0, lty=2)
        abline(h=1, lty=2)
        
        # Calculating the product 
        logistic = logisticScore(Xseq, input$beta_0, input$beta_1)
        lines(Xseq, logistic, lwd=2)
    })
    
    output$MLplot <- renderPlot({
        rangeBeta0 <- seq(input$beta_0_range[1], input$beta_0_range[2], length=input$granularity)
        rangeBeta1 <- seq(input$beta_1_range[1], input$beta_1_range[2], length=input$granularity)
        
        # Expanding the grid
        grid = expand.grid(rangeBeta0, rangeBeta1)
        names(grid) <- c("beta0", "beta1")
        
        # Getting the scores
        scores <- c()
        for(i in 1:nrow(grid)){
            params <- grid[i, ]
            scores <- c(scores, MLscore(Y, X, as.numeric(params[1]), as.numeric(params[2])))
        }
        
        # Adding to the original frame 
        grid$scores <- scores
        
        # Outputing a level plot
        levelplot(scores ~ beta0 * beta1, data=grid, col.regions=heat.colors(100), main='Maximum likelihood heatmap')
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
