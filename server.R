#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("function.R")
source("library.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    df <- iris

    output$scatter1 <- renderPlot({
        return(df %>% 
                   ggplot(aes(x = Sepal.Length, y = Sepal.Width, fill = Species, colour = Species)) + 
                   geom_point() +
                   gginteractive(interactive = FALSE)
               )
    })
    
    output$scatter2 <- renderPlot({
        return(df %>% 
                   ggplot(aes(x = Petal.Length, y = Petal.Width, fill = Species, colour = Species)) + 
                   geom_point()
        )
    })
    
    output$info <- renderText({
        xy_str <- function(e) {
            if(is.null(e)) return("NULL\n")
            paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
        }
        xy_range_str <- function(e) {
            if(is.null(e)) return("NULL\n")
            paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
                   " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
        }
        
        paste0(
            "click: ", str(input$plot_click)#,
            #"dblclick: ", xy_str(input$plot_dblclick),
            #"hover: ", xy_str(input$plot_hover),
            #"brush: ", xy_range_str(input$plot_brush)
        )
    })
    
    output$scatter3 <- renderPlotly({
        p <- plot_ly(data = df, 
                     x = ~Petal.Length, y = ~Petal.Width, color = ~Species, 
                     type = "scatter", mode = "markers", source = "scatter3")
        
        s <- event_data("plotly_click", source = "scatter3")
        if (length(s) != 0) {
            p <- 
                p %>% 
                add_trace(data = df %>% dplyr::filter(Petal.Length == s$x & Petal.Width == s$y),
                          type = "scatter", 
                          marker = list(color = 'rgba(255, 255, 255, .1)',
                                        line = list(color = 'rgba(152, 0, 0, .8)',width = 2)
                                        )
                          )
        }
        return(p)
    })
    
    output$selection <- renderPrint({
        s <- event_data("plotly_click", source = "scatter3")
        if (length(s) == 0) {
            "Click on a cell in the heatmap to display a scatterplot"
        } else {
            cat("You selected: \n\n")
            str(s)
        }
    })
    
    output$high <- renderPlotly({
        highlight_key(df) %>%
            GGally::ggpairs(aes(color = Species), columns = 1:4) %>%
            ggplotly(source = "high") %>%
            highlight("plotly_selected")
    })
    
    output$high_selection <- renderPrint({
        s <- event_data("plotly_click", source = "high")
        if (length(s) == 0) {
            "Click on a cell in the heatmap to display a scatterplot"
        } else {
            cat("You selected: \n\n")
            str(s)
        }
    })
    
    values <- reactiveValues()
    values$DT <- df
    
    rm_values <- reactiveValues()
    rm_values$DT <- define_0rowdf_with_otherdf(df)
    
    #output$value_plot <- 
})
