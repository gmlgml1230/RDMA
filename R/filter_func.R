
#' @import shiny

scfilter.func <- function(btn.num){
  vapply(X = 1:btn.num,
         FUN = function(x){
           dimension.chr <- input[[NS(x, "filterborder")]]
           operator.chr <- input[[NS(x, "operator")]]
           expression.chr <- input[[NS(x, "expression")]]
           if(dimension.chr == "country"){
             paste("country", operator.chr, expression.chr)
           } else if(dimension.chr == "device"){
             paste("device", operator.chr, expression.chr)
           } else if(dimension.chr == "page"){
             paste("page", operator.chr, expression.chr)
           } else if(dimension.chr == "query"){
             paste("query", operator.chr, expression.chr)
           } else {
             paste("searchAppearance", operator.chr, expression.chr)
           }
         },
         FUN.VALUE = character(1))
}

dtfilter.func <- function(btn.num){
  vapply(X = 11:btn.num,
         FUN = function(x){
           dimension.chr <- input[[NS(x, "filterborder")]]
           operator.chr <- input[[NS(x, "operator")]]
           expression.chr <- input[[NS(x, "expression")]]
           and_or.chr <- input[[NS(x, "and_or")]]

           if(is.null(and_or.chr)){
             and_or.chr <- " "
           } else {
             if(and_or.chr == "AND"){and_or.chr <- "&"} else {and_or.chr <- "|"}
           }

           if(operator.chr == "~~"){
             paste0(and_or.chr, " grepl('", expression.chr, "', ", dimension.chr, ") ")
           } else if(operator.chr == "=="){
             paste0(and_or.chr, " ", dimension.chr, " == '", expression.chr, "' ")
           } else if(operator.chr == "!~"){
             paste0(and_or.chr, " grepl('", expression.chr, "', ", dimension.chr, ") == 0 ")
           } else if(operator.chr == "!="){
             paste0(and_or.chr, " ", dimension.chr, " != '", expression.chr, "' ")
           } else if(operator.chr == ">="){
             paste0(and_or.chr, " ", dimension.chr, " >= ", expression.chr, " ")
           } else if(operator.chr == "<="){
             paste0(and_or.chr, " ", dimension.chr, " <= ", expression.chr, " ")
           }
         },
         FUN.VALUE = character(1)) %>% paste(., collapse = " ")
}
