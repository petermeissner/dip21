#### packages
library(stringr)
library(hellno)
library(dip21)

#### getting started ###########################################################

# options <-  dip21_search_options()

tmp <-
  dip21_search(
    wahlperiode = 18,
    vorgangstyp = "Gesetz",
    res         = "content"
  )

tmp2 <- dip21_details(tmp)

xml <- tmp2[[1]]

rvest::html_nodes(xml, "table a")





