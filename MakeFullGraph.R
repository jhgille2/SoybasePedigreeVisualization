##################################################
## Project: Soybase Pedigree Visualization
## Script purpose: Script to make and save an igraph 
## object of the full soybase pedigree
## Date: 2021-04-20
## Author: Jay Gillenwater
##################################################

# Check for required packages and install if they are not already installed
requiredPackages <- c("tidyverse",
                      "igraph")
toInstall <- requiredPackages[!(requiredPackages %in% installed.packages()[,"Package"])]
if(length(toInstall)) install.packages(toInstall)

library(tidyverse)
library(igraph)

# Read in the parentage table
AllParentage <- read_csv("./Data/SoybasePedigreeData.csv")

ComplexCrosses <- AllParentage %>% filter(grepl("\\*", Female) | grepl("\\*", Male))
SimpleCrosses  <- AllParentage %>% filter(!grepl("\\*", Female) & !grepl("\\*", Male))

ComplexParents <- ComplexCrosses %>% 
  select(Female, Male) %>% 
  unlist() %>% 
  as.character()

ComplexParents <- ComplexParents[grepl("\\*", ComplexParents) %>% which()]
ComplexParents[39] <- "((((Peking*Scott)*(Peking*Scott))*Scott)*Blackhawk)"

ComplexParentsDF <- tibble(Parent = ComplexParents) %>%
  mutate(OpenBracket = str_count(Parent, "\\("), 
         CloseBracket = str_count(Parent, "\\)"))

MatchedBrackets <- ComplexParentsDF %>%
  filter(OpenBracket == CloseBracket)

MatchedBrackets$Parent[250] <- "AP1953*(((Asgrow A3205*(Asgrow 3127*Fayette))*G2436A))"

ComplexParentPedigrees <- vector('list', length = length(MatchedBrackets$Parent))
for(i in 1:length(ComplexParentPedigrees)){
  Res <- tryCatch(
    {
      ParseCross(MatchedBrackets$Parent[[i]])
    },
    error = function(cond){
      message(paste("Parse failed for cross:", MatchedBrackets$Parent[[i]], "Index:", i))
      return(NA)
    }
  )
  
  ComplexParentPedigrees[[i]] <- Res
}

AllComplex <- reduce(ComplexParentPedigrees, rbind)
AllCrosses <- reduce(list(AllComplex, ComplexCrosses, SimpleCrosses), bind_rows)

AllCrosses %>% pivot_longer(c(Female, Male)) %>%
  dplyr::select(value, Cultivar, name) %>%
  rename(source = value, target = Cultivar) %>%
  dplyr::filter(!is.na(source)) %>%
  map_dfc(., CleanParent) -> EdgeList_AllCrosses

AllNodes <- data.frame(id = unique(unlist(select(EdgeList_AllCrosses, source, target))))

AllCrosses_igraph <- graph_from_data_frame(EdgeList_AllCrosses)

save(AllCrosses_igraph, file = "./Data/AllCrossesGraph.RData")
