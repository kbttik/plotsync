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

# 使用するdf
df <- iris
# 使用するかのindex: 使用するならTRUE
index_use <- rep(TRUE, NROW(df))

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    # output$scatter1 <- renderPlot({
    #     return(df %>% 
    #                ggplot(aes(x = Sepal.Length, y = Sepal.Width, fill = Species, colour = Species)) + 
    #                geom_point() +
    #                gginteractive(interactive = FALSE)
    #            )
    # })
    # 
    # output$scatter2 <- renderPlot({
    #     return(df %>% 
    #                ggplot(aes(x = Petal.Length, y = Petal.Width, fill = Species, colour = Species)) + 
    #                geom_point()
    #     )
    # })
    # 
    # output$info <- renderText({
    #     xy_str <- function(e) {
    #         if(is.null(e)) return("NULL\n")
    #         paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
    #     }
    #     xy_range_str <- function(e) {
    #         if(is.null(e)) return("NULL\n")
    #         paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
    #                " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
    #     }
    #     
    #     paste0(
    #         "click: ", str(input$plot_click)#,
    #         #"dblclick: ", xy_str(input$plot_dblclick),
    #         #"hover: ", xy_str(input$plot_hover),
    #         #"brush: ", xy_range_str(input$plot_brush)
    #     )
    # })
    # 
    # output$scatter3 <- renderPlotly({
    #     p <- plot_ly(data = df, 
    #                  x = ~Petal.Length, y = ~Petal.Width, color = ~Species, 
    #                  type = "scatter", mode = "markers", source = "scatter3")
    #     
    #     s <- event_data("plotly_click", source = "scatter3")
    #     if (length(s) != 0) {
    #         p <- 
    #             p %>% 
    #             add_trace(data = df %>% dplyr::filter(Petal.Length == s$x & Petal.Width == s$y),
    #                       type = "scatter", 
    #                       marker = list(color = 'rgba(255, 255, 255, .1)',
    #                                     line = list(color = 'rgba(152, 0, 0, .8)',width = 2)
    #                                     )
    #                       )
    #     }
    #     return(p)
    # })
    # 
    # output$selection <- renderPrint({
    #     s <- event_data("plotly_click", source = "scatter3")
    #     if (length(s) == 0) {
    #         "Click on a cell in the heatmap to display a scatterplot"
    #     } else {
    #         cat("You selected: \n\n")
    #         str(s)
    #     }
    # })
    # 
    
    # # 使用するかのindex: 使用するならTRUE
    #index <- reactiveValues(event =NULL, use = rep(TRUE, NROW(df)))
    
    # observe({
    #     # 値の更新
    #     index$event <- event_data(event = "plotly_click", source = "high")
        # if (length(index$event) != 0){
        #     index$use <- set_flag_without_prev(index_use, as.integer(index$event$key))
        # }
    # })
    
    output$high <- renderPlotly({
        
        click_event <- event_data(event = "plotly_click", source = "high")
        click_event_oppo <- event_data(event = "plotly_click", source = "high_rm")
        if (length(click_event) != 0){
            index_use <<- set_flag_without_prev(index_use, as.integer(click_event$key))
        }

        # if (length(click_event) != 0){
        #     #index_use <<- set_flag_without_prev(index_use, as.integer(s$key))
        #     index$use <- set_flag_without_prev(index_use, as.integer(click_event$key))
        #     #index_use <<- index$use
        # }
        
        #plotlyでのggpairの描画
        highlight_key(df %>% dplyr::filter(index_use)) %>%
            GGally::ggpairs(aes(color = Species), columns = 1:4) %>%
            ggplotly(source = "high") %>%
            highlight("plotly_selected")
        
        # plot_ly(data = df %>% dplyr::filter(index_use),
        #         x = ~Petal.Length, y = ~Petal.Width, color = ~Species,
        #         type = "scatter", mode = "markers", source = "high")
    })
    
  
    output$high_rm <- renderPlotly({
        
        click_event <- event_data(event = "plotly_click", source = "high_rm")
        click_event_oppo <- event_data(event = "plotly_click", source = "high")
        if (length(click_event) != 0){
            index_use <<- set_flag_without_prev(index_use, as.integer(click_event$key), tf = FALSE)
        }
        
        # if (length(click_event) != 0){
        #     #index_use <<- set_flag_without_prev(index_use, as.integer(s$key))
        #     index$use <- set_flag_without_prev(index_use, as.integer(click_event$key))
        #     #index_use <<- index$use
        # }
        
        #plotlyでのggpairの描画
        highlight_key(df %>% dplyr::filter(!index_use)) %>%
            GGally::ggpairs(aes(color = Species), columns = 1:4) %>%
            ggplotly(source = "high_rm") %>%
            highlight("plotly_selected")
        
        # plot_ly(data = df %>% dplyr::filter(index_use),
        #         x = ~Petal.Length, y = ~Petal.Width, color = ~Species,
        #         type = "scatter", mode = "markers", source = "high")
    })
    
    output$high_selection <- renderPrint({
        s <- event_data("plotly_click", source = "high_rm")

        if (length(s) == 0) {
            "Click on a cell in the heatmap to display a scatterplot"
        } else {
            cat("You selected: \n\n")
            str(s)
        }
    })
    
    # reactiveValueの効果: ボタン操作の検知の後に、更新するように動作
    # values <- reactiveValues()
    # values$DT <- df
    # 
    # rm_values <- reactiveValues()
    # rm_values$DT <- define_0rowdf_with_otherdf(df)
    
})
