# install if not already
install.packages("haven")
install.packages("tidyverse")

library(haven)
library(tidyverse)

# path to your .xpt file
demographics = read_xpt("../raw_data/DEMO_L.xpt")
day1_foods = read_xpt("../raw_data/DR1IFF_L.xpt")
day1_nutri = read_xpt("../raw_data/DR1TOT_L.xpt")
