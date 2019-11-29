wqg_table <- function(variable, guideline, term,
                      ph, hardness, methyl_mercury, chloride){
  x <- try(do.call("rbind", lapply(term, function(x){
    y <- lookup_limits2(
      ph = ph, 
      hardness = hardness, 
      methyl_mercury = methyl_mercury, 
      chloride = chloride,
      variable = variable,
      term = x)
    y$Term <- x
    dplyr::mutate_if(y, is.factor, as.character)
  })), silent = TRUE)
  if(is_try_error(x))
    return()
  x <- add_missing(x, variable, term)
  x
}

wqg_rmd <- function(x, output){
  output <- paste0(tolower(output), "_document")
  markobj <- c('---',
               'title: "Water Quality Guideline"',
               paste('output:', output),
               '---',
               '',
               '## Water Quality Guideline',
               '',
               '```{r}',
               'print(x)',
               '```')
  knitr::knit(text = markobj)
}
# a <- 1:5
# markobj <- c('---',
#              'title: "test"',
#              'output: html_document',
#              '---',
#              '',
#              '## R Markdown',
#              '',
#              'This is an R Markdown document.',
#              '```{r}',
#              'b <- 11:15',
#              'print(a)',
#              'print(b)',
#              '```')
# 
# txt <- knitr::knit(text = markobj)
# 
# ## convert to html file
# markdown::markdownToHTML(text = txt, output = "test.html")
# 
# ## open file in browser
# browseURL("test.html")
# wqg_report <- function(x){
#   
# }