standardize <- function(x, na.rm=TRUE){
  return((x - mean(x, na.rm=na.rm) ) / sd(x, na.rm=na.rm))
}

`%!in%` <- Negate(`%in%`)

first_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

log_na <- \(x){
  return(ifelse(is.na(x) | x <= 0, NA, log(x)))
}