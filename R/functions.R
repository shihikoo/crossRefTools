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
    data.frame(doi= x["DOI"], varname = names(x), varvalue = as.character(x), drop = F)
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


#' find_dois
#'
#' @param query  query
#' @param limit number papers to extract
#'
#' @return a data fraem
#' @export find_dois
#'
#'@import rcrossref
#'
#' @examples
#' query <- "Survival in frontotemporal lobar degeneration and related disorders: latent class predictors and brain functional correlates"
#' find_dois(query = query)
#'
#' queryList <- c("COVID-19 and Myocarditis: What Do We Know So Far?", "Neurologic complications of COVID-19")
#' sapply(queryList, find_dois)
#'
find_dois <- function(query, limit = 1){
  if(length(query) == 0 ) return(NULL)
    myData <- rcrossref::cr_works(query = query, limit = 1)
    doi <- myData$data$doi
    title <- myData$data$title
    if(agrepl(query, title)) return(doi)
  return(NULL)
}
