setup <- function(){
  needed <- c("here", "brms", "data.table", "lubridate", "tidyverse", "dplyr", "ggpubr", 
              "ggplot2", "timetk", "RColorBrewer", "scales", "reshape2", "wrapr")
  for(package in needed){
    if(!sum(installed.packages() %in% package)){
      install.packages(package)
    }
    
    require(package, character.only = TRUE)
  }
}

setup()

