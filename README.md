# crossRefTools-

Tools to retrive data from crossref with dois into data frame


```r
install.packages("devtools")
devtools::install_github("shihikoo/crossrefTools")
library(crossrefTools)

dois <- c("10.5284/1011335","10.1126/science.169.3946.635")
extract_df_crossref(dois)
