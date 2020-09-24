

#' extract_df_crossref
#'
#' @param dois  a list of dois
#'
#' @return a data fraem
#' @export extract_df_crossref
#'
#'@import rcrossref tidyr retractcheck
#'
#' @examples
#' dois <- c("10.5284/1011335","10.1126/science.169.3946.635")
#' extract_df_crossref(dois)
#'
extract_df_crossref <- function(dois){
  if(length(dois) == 0 ) return(NA)
  if(sum(retractcheck::check_doi(dois)))

  resultList <- rcrossref::cr_cn(dois, "citeproc-json-ish")
  resultLong <- do.call(rbind, lapply(resultList,function(x){
    data.frame(doi= x$DOI, varname = names(x), varvalue = as.character(x) )
  }))

  resultWide <- tidyr::spread(resultLong, key="varname", value = "varvalue")

  resultWide$author <- as.character(resultWide$author)
  resultWide$authorList <- sapply(resultWide$author, function(x){res <- eval(parse(text = x))})
  resultWide$cleanAuthor <- sapply(resultWide$authorList, function(x){
    authors <- ""
    input <- x[c("given","family")]
    if(!is.null(input[[1]])){
      df <- data.frame(matrix(unlist(input), nrow=length(input), byrow=T))
      authors <- paste(apply(df, 2,paste, collapse = " "), collapse = "; ")
    }
    if(authors != "")  return(authors)
    input <- x[c("literal")]
    if(! is.null(input[[1]])) {
      df <- data.frame(matrix(unlist(input), nrow=length(input), byrow=T))
      authors <- paste(apply(df, 2,paste, collapse = " "), collapse = "; ")
    }
    return(authors)
  })

  return(resultWide)
}
