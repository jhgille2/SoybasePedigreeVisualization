##################################################
## Project: Soybase Pedigree Visualization
## Script purpose: Process the grin_results file
## Date: 2021-04-20
## Author: Jay Gillenwater
##################################################

library(tidyverse)
library(janitor)
library(vroom)

# Read in all results
AllResults <- vroom("./Data/grin_results.txt")
PINumbers <- vroom("./Data/PI_Numbers.csv") %>% mutate(PI_Num = str_remove(PI_Num, " "))
CrossData <- vroom("./Data/SoybasePedigreeData.csv")

AllResults_reduced <- AllResults %>%
  clean_names() %>%
  select(grin_accession, 
         flower_color, 
         height, 
         hilum_color, 
         lodging, 
         maturity_group, 
         oil, 
         protein, 
         pubescence_color, 
         seed_quality, 
         seed_weight, 
         yield)

AverageTrait <- function(TraitValue){
  TraitValue %>% 
    str_split(";") %>%
    unlist() %>%
    as.numeric() %>%
    mean(na.rm = TRUE)
}

AllResults_reduced %>%
  mutate(height       = map_dbl(height, AverageTrait), 
         lodging      = map_dbl(lodging, AverageTrait), 
         oil          = map_dbl(oil, AverageTrait), 
         protein      = map_dbl(protein, AverageTrait), 
         seed_quality = map_dbl(seed_quality, AverageTrait), 
         seed_weight  = map_dbl(seed_weight, AverageTrait), 
         yield        = map_dbl(yield, AverageTrait)) -> AllResults_cleaned


left_join(CrossData, PINumbers) %>% 
  left_join(., AllResults_cleaned, by = c("PI_Num" = "grin_accession")) -> CrossData_withTraits

write_csv(CrossData_withTraits, "./Data/CrossData_withTraits.csv")
write_csv(AllResults_cleaned, "./Data/GrinTraits.csv")

