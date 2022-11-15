##https://stackoverflow.com/questions/7881766/using-r-to-parse-out-surveymonkey-csv-files
sm_header_function <- function(x, rep_val){
  
  orig <- x
  
  sv <- x
  sv <- sv[1,]
  sv <- sv[, sapply(sv, Negate(anyNA)), drop = FALSE]
  sv <- t(sv)
  sv <- cbind(rownames(sv), data.frame(sv, row.names = NULL))
  names(sv)[1] <- "name"
  names(sv)[2] <- "value"
  sv$grp <- with(sv, ave(name, FUN = function(x) cumsum(!startsWith(name, rep_val))))
  sv$new_value <- with(sv, ave(name, grp, FUN = function(x) head(x, 1)))
  sv$new_value <- paste0(sv$new_value, " ", sv$value)
  new_names <- as.character(sv$new_value)
  colnames(orig)[which(colnames(orig) %in% sv$name)] <- sv$new_value
  orig <- orig[-c(1),]
  return(orig)
}

##for csv
#sm_header_function(df, "X") 
##for excel
#sm_header_function(df, "...")