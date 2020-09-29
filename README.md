# crossrefTools

Tools to retrive data from crossref with dois into data frame

Installation
```r
install.packages("devtools")
devtools::install_github("shihikoo/crossrefTools")
library(crossrefTools)
```

Extract metadata with dois from dois
```r
dois <- c("10.5284/1011335","10.1126/science.169.3946.635")
extract_df_crossref(dois)
```

Identify dois with title

```r
find_dois("COVID-19 and Myocarditis: What Do We Know So Far?")

```
