highlight_key(iris) %>%
  GGally::ggpairs(aes(color = Species), columns = 1:4) %>%
  ggplotly() %>%
  highlight("plotly_selected")
