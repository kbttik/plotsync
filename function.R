
gginteractive <- function(interactive = TRUE, ...) {
  structure(
    if(interactive) {
      purrr::partial(plotly::ggplotly, ...)
    } else {
      identity
    },
    class = c("gginteractive", "function")
  )
}

ggplot_add.gginteractive <- function (object, plot, object_name) object(plot)


define_0rowdf_with_otherdf <- function(odf){
  
  def_text <- "data.frame("
  for (name in names(odf)) {
    def_text <- paste0(def_text, name, "=", class(odf[,name]), "(),")
  }
  if("character" %in% sapply(odf, class)) # characterがあれば
    def_text <- paste0(def_text, "stringAsFactors=FALSE)")
  else # なければ
    def_text <- paste0(stringr::str_sub(def_text, end = -2), ")")
  
  return(eval(parse(text = def_text)))
}

