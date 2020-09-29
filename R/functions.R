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
  if(length(dois) == 0 ) return(NULL)
  if(sum(retractcheck::check_doi(dois)) == 0) return(NULL)
  
  resultList <- rcrossref::cr_cn(dois, "citeproc-json-ish")
  if(length(resultList) == 0) return(NULL)
  
  if(length(dois) ==1 ) resultList <- list(resultList)
  
  resultLong <- do.call(rbind, lapply(resultList, function(x){
    data.frame(doi= x["DOI"], varname = names(x), varvalue = as.character(x), drop = F)
  }))
  
  resultWide <- tidyr::spread(resultLong, key="varname", value = "varvalue")
  
  resultWide$author <- lapply(resultWide$author,as.character)
  resultWide$authorList <- lapply(resultWide$author, function(x){res <- eval(parse(text = x))})
  
  resultWide$cleanAuthor <- sapply(resultWide$authorList, function(x){
    if(length(x) == 0) return("")
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
  
  resultWide$created <- lapply(resultWide$created,as.character)
  resultWide$dateList <- lapply(resultWide$created, function(x){res <- eval(parse(text = x))})
  
  resultWide$year <- sapply(resultWide$dateList, function(x){
    if(length(x) == 0) return("")
    year <- ""
    input <- x[c("date-parts")]
    if(!is.null(input[[1]])){
      df <- data.frame(matrix(unlist(input), nrow=length(input), byrow=T))
      
      year <- df[1]
    }
    return(as.numeric(year))
  })
  
  return(resultWide)
}

#' find_dois
#'
#' @param input_title  input title to serach for
#' @param input_year  input year to search for
#' @param limit number papers to extract
#' @param title_distance_threshold a double number. The treshold of the distance between true title and false
#' 
#' @return a data fraem
#' @export find_dois
#'
#'@import rcrossref stringdist
#'
#' @examples
#' input_title <- "Survival in frontotemporal lobar degeneration and related disorders"
#' input_year <- 2009
#' find_dois(input_title, input_year)
#'
find_dois <- function(input_title, input_year = NA, limit = 2, title_distance_threshold = 0.93){
  clean_title <- function(x){
    x <- tolower(x)
    x <- gsub("&rsquo;","'",x)
    return(x)
  }
  compare_year <- function(year1, year2){
    issame <- abs(year1 - year2) < 2
    if(is.na(issame)) return(FALSE)
    return(issame)
  }
  
  if(length(input_title) == 0 ) return(NA)
  result <- rcrossref::cr_works(query = input_title, limit = limit)
  myData <- result$data
  if(nrow(myData) == 0) return(NA)
  
  if(!is.null(myData$published.print)) {
    myData$year <- sapply(myData$published.print,  function(x){
      if(is.null(x)) return(NA)
      year <- as.numeric(strsplit(x, "-")[[1]][1])
    })
  } else if(!is.null(myData$published.online))  {
    myData$year <- sapply(myData$published.online,  function(x){
      if(is.null(x)) return(NA)
      year <- as.numeric(strsplit(x, "-")[[1]][1])
    })
  } else{
    myData$year <- NA 
  }
  
  myData$same_year <- sapply(myData$year, function(x){
    return(is.na(x) | compare_year(input_year, x))
  })
  index <- which(myData$same_year)
  if(length(index) == 0) return(NA) else myData <- myData[index,]
  
  myData$distance <- sapply(myData$title,  function(x){
    title1 = clean_title(x)
    title2 = clean_title(input_title)
    return(1-stringdist::stringdist(title1,title2,method='jw',p=0.1))}
  )
  myData$same_title <- myData$distance > title_distance_threshold
  index <- which(myData$same_title)
  if(length(index) == 0) return(NA) else myData <- myData[index,]
  
  index <- which(myData$distance == max(myData$distance, na.rm = T))[[1]]
  doi <- myData$doi[index]
  return(doi)
}