# install if not already
install.packages("haven")
install.packages("tidyverse")

library(haven)

setwd("C:/Users/k_the/Downloads")
# path to your .xpt file
demographics = read_xpt("DEMO_L.xpt")

day1 = read_xpt("DR1IFF_L.xpt")
