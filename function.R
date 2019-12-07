
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


# dfから、同じ定義の空のdfを作成
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

# すでにフラグをつけてある箇所を抜いて、反転させる
set_flag_without_prev <- function(x, n, tf = TRUE){
  if(tf) # TRUEベース
    x[x][n] <- FALSE
  else # FALSEベース
    x[!x][n] <- TRUE
  
  return(x)
}
