#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#' get a pretty table that can be displayed in Jupyter notebooks
#' @return nothing
#' examples
#' jupy_table(mtcars,digits=2)
jupy_table<-function(df,digits=3){
  classes <- sapply(df,class)
  classes <- as.data.frame(classes)
  classes <- classes%>%mutate(align = case_when(
    classes == 'factor' ~ 'l',
    classes == 'character' ~ 'l',
    T ~ 'r'
  ))

  alignment<-classes%>%select(align)%>%pull()
  table<-knitr::kable(df,format='html',
                      align = alignment,
                      digits = digits,
                      format.args = list(decimal.mark = ".", big.mark = ","),
  )%>%as.character()%>%IRdisplay::display_html()
  invisible()
}
