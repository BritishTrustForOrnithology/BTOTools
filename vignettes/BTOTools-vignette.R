## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- fig.show='hold'---------------------------------------------------------
library(BTOTools)
temp1<-data.frame(cbc_code='R.', 
                  count=10,
                  onekm='TL1234', 
                  stringsAsFactors = FALSE)
rescale_1km_to_2km(temp1,'onekm')
rescale_1km_to_10km(temp1,'onekm')

## ---- fig.show='hold'---------------------------------------------------------
library(BTOTools)
data("centroids050")
plot(centroids050$easting, centroids050$northing, asp = 1)

