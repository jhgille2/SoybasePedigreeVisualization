##################################################
## Project: Soybase Pedigree Visualization
## Script purpose: functions to convert complicated pedigrees to 
## 'computer-friendly' formats, ready for visualization as pedigree trees
## with cytoscape
## Date: 2021-04-20
## Author: Jay Gillenwater
##################################################

# Check for required packages and install if they are not already installed
requiredPackages <- c("RCy3", 
                      "dplyr",
                      "tidyr", 
                      "stringr",
                      "readr",
                      "purrr",
                      "R6DS",
                      "igraph",
                      "visNetwork")
toInstall <- requiredPackages[!(requiredPackages %in% installed.packages()[,"Package"])]
if(length(toInstall)) install.packages(toInstall)

library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(purrr)
library(R6DS)
library(RCy3)
library(igraph)
library(visNetwork)

# A function to find and replace selfing notation with something
# more easily parsable
Simplify_Selfs <- function(Cross){
  
  # Get all the selfs in the cross
  AllSelfs <- str_extract_all(Cross, "\\(\\d+\\)")
  
  if(length(AllSelfs[[1]]) == 0){
    return(Cross)
  }
  
  AllSelfs <- AllSelfs[[1]]
  
  # The number of selfing generations for each cross
  AllSelfNums <- map(AllSelfs, function(x) str_extract_all(x, "\\d+") %>% as.numeric())
  
  # Make new strings based on these selfing numbers
  NewSelfStrings <- paste('_SelfingGenerations', AllSelfNums, sep = "__")
  
  ReplaceStrings <- NewSelfStrings
  names(ReplaceStrings) <- AllSelfs %>% 
    str_replace(., "\\(", "\\\\(") %>% 
    str_replace(., "\\)", "\\\\)")
  
  str_replace_all(Cross, ReplaceStrings)
}

# A function to replace square brackets with round brackets, and 
# convert cross "x's" to the "*" operator
StandardizeBrackets <- function(Cross){
  str_replace_all(Cross, "\\[", "\\(") %>%
    str_replace_all(., "\\]", "\\)") %>% 
    str_replace_all(., " x ", " * ")
}

# A function to remove some specific whitespace
FixWhitespace <- function(Cross){
  
  # Several conditions to resolve
  # 1: Whitespace between brackets
  # 2: Whitespace between a bracket and a genotype name
  # 3: Whitespace between a genotype name and a cross operator "*"
  
  Cross %>%
    str_replace_all("\\(\\s+\\(", "\\(\\(") %>%
    str_replace_all("\\)\\s+\\)", "\\)\\)") %>%
    str_replace_all("\\( ", "\\(") %>%
    str_replace_all(" \\)", "\\)") %>%
    str_replace_all(" \\* ", "\\*")
  
}

# A function to reformat the selfing notation back to what it started as
ReformatSelfs <- function(SelfedCross){
  
  SelfSuffix <- str_extract_all(SelfedCross, "_SelfingGenerations__\\d+")
  
  if(length(SelfSuffix[[1]]) == 0){
    return(SelfedCross)
  }
  
  SelfNum   <- SelfSuffix %>% 
    unlist() %>% 
    str_remove_all("_SelfingGenerations__") %>% 
    as.numeric()
  
  NewSuffix <- paste("(", SelfNum, ")", sep = "")
  
  ReplaceStrings <- NewSuffix
  names(ReplaceStrings) <- as.character(unlist(SelfSuffix))
  
  str_replace_all(SelfedCross, ReplaceStrings)
}

# Use a stack to convert the cross notation to something easier to handle. 
# In this case, this is roughly equivalent to converting from infix
# to postfix notation.
StackPostfix <- function(Cross){
  
  OpStack    <- RStack$new()
  CrossStack <- RStack$new()
  
  for(i in seq_len(str_length(Cross))){
    
    CurrentCharacter <- str_sub(Cross, i, i)
    
    if(CurrentCharacter %in% c("*", "(")){
      OpStack$push(CurrentCharacter)
      if(CurrentCharacter == "*"){
        CrossStack$push("  ")
      }
    }else if(CurrentCharacter == ")"){
      while(OpStack$peek() == "("){
        OpStack$pop()
      }
      CrossStack$push(OpStack$pop())
    }else{
      CrossStack$push(CurrentCharacter)
    }
    
    if(i == str_length(Cross)){
      
      while(!is.null(OpStack$peek())){
        if(OpStack$peek() == "("){
          OpStack$pop()
        }else{
          CrossStack$push(OpStack$pop())
        }
      }
    }
  }
  paste(CrossStack$toList, collapse = "")
}

# A function that combines two list elements and returns a list with those
# two elements merged. Used to progressively build up a cross in a dataframe
# format from a cross in postfix notation
MergeAndReplace <- function(CrossList = LETTERS[1:5], MergeIndices = c(1, 2)){
  
  DLL <- RDLL$new(collapse = CrossList)
  
  Elem1 <- DLL$elem_at(MergeIndices[[1]])
  Elem2 <- DLL$elem_at(MergeIndices[[2]]) %>% str_remove("\\*")
  
  Combined <- paste0("(", paste(Elem1, Elem2, sep = "__x__"), ")")
  
  DLL$insert_at(MergeIndices[[1]], Combined)
  DLL$remove_at(MergeIndices[[1]] + 1)
  DLL$remove_at(MergeIndices[[1]] + 1)
  
  CleanPedigree <- function(CrossData){
    
    CrossData %>%
      str_remove_all("\\*") %>%
      ReformatSelfs()
    
  }
  
  Pedigree <- tibble(Female   = Elem1, 
                     Male     = Elem2, 
                     Cultivar = Combined) %>%
    map_dfc(CleanPedigree)
  
  NewList <- DLL$toList %>% unlist()
  
  return(list(PedigreeTable = Pedigree, NewCrossList = NewList))
}

# Format a cross in postfix notation as a dataframe
PostFix_toDF <- function(Cross_Postfix){
  
  # Split the postfix expression into genotypes
  SplitCross <- str_split(Cross_Postfix, "  ") %>% unlist()
  
  GetMergeIndices <- function(SplitCross){
    
    FirstCross <- SplitCross %>% 
      str_detect("\\*") %>% 
      which() %>% 
      min()
    
    return(c(FirstCross-1, FirstCross))
  }
  
  ParentStack <- RStack$new()
  
  while(length(SplitCross) > 1){
    
    NewMerge   <- MergeAndReplace(CrossList = SplitCross, MergeIndices = GetMergeIndices(SplitCross))
    SplitCross <- NewMerge$NewCrossList
    ParentStack$push(NewMerge$PedigreeTable)
    
  }
  
  ParentStack$toList %>% 
    purrr::reduce(bind_rows) %>%
    map_dfc(function(x) str_replace_all(x, "__x__", "\\*"))
}

# Combine the above functions 
ParseCross <- function(Cross){
  
  # First, see if any additional processing needs to be done
  CrossLen <- str_count(Cross, "\\*")
  
  # If no...
  if(CrossLen == 0){
    Cross %>% 
      Simplify_Selfs() %>%
      StandardizeBrackets() %>%
      FixWhitespace() %>%
      ReformatSelfs()
  }else{
    Cross %>%
      Simplify_Selfs() %>%
      StandardizeBrackets() %>%
      FixWhitespace() %>%
      StackPostfix() %>%
      PostFix_toDF()
  }
  
}

# All the basic cleaning functions combined
CleanParent <- function(Parent){
  Parent %>% 
    Simplify_Selfs() %>% 
    StandardizeBrackets() %>% 
    FixWhitespace() %>% 
    ReformatSelfs()
}

# This function does work. However, in the future it would (probably) be more efficient to 
# make one big graph using all the pedigree data and then extract subgraphs for a given cultivar
# instead of building them on the fly.
#
##### UPDATE: Check next section, the new function that uses a graph to search is much faster
#
# Another source of inefficiency is that right now, the function will repeatedly build the
# same parts of the graph if a cultivar is used multiple times. This is definitely solvable
# by keeping track what crosses have already been run and/or using a better data structure
# but I'm feeling lazy right now and its also like 4 am. 
# This can make the function run annoyingly slow for complicated crosses 
# (but still less than a few minutes so far for the tests I've run).
GetPedigree <- function(StartCultivar = "Holladay", PedigreeData = AllParentage, MaxDepth = 5){
  
  # Initialize the tree depth counter
  CurrentDepth <- 1
  
  # Get the starting parents
  GetParentage <- function(Geno){
    dplyr::filter(PedigreeData, Cultivar == Geno)
  }
  
  # Get the initial parents
  ParentageStart <- GetParentage(StartCultivar)
  
  if(nrow(ParentageStart) == 0){
    message("Starting cultivar not found in database")
    return(NULL)
  }
  
  # A table to hold the final set of pedigree tables
  FinalStack <- RStack$new()
  FinalStack$push(ParentageStart)
  
  # A stack to hold temporary cultivar names and
  # The pedigree tables for each cultivar
  TempCultivarStack <- RStack$new(collapse = unlist(select(ParentageStart, Female, Male)))
  TempTableStack    <- RStack$new()
  
  AllCultivarRecord <- RStack$new(collapse = unlist(ParentageStart))
  
  while(CurrentDepth < MaxDepth){
    
    while(TempCultivarStack$private$.len > 0){
      
      CurrentCultivar <- TempCultivarStack$pop()
      
      CultivarFullCross <- CurrentCultivar %>%
        ParseCross()
      
      if(typeof(CultivarFullCross) == "list"){
        
        TempCultivarStack$push(collapse = unlist(select(CultivarFullCross, Female, Male)))
        TempTableStack$push(CultivarFullCross)
        
      }else if(typeof(CultivarFullCross) == "character"){
        
        TempTableStack$push(GetParentage(CurrentCultivar))
        
      }
    }
    
    # if(CurrentDepth == 14){
    #   browser()
    # }
    
    if(length(TempTableStack$toList) == 0){
      break()
    }else{
      NewParentage <- TempTableStack$toList %>% purrr::reduce(bind_rows)
      FinalStack$push(NewParentage)
    }
    
    NewCultivars <- unlist(select(NewParentage, Female, Male))
    NewCultivars <- NewCultivars[!is.na(NewCultivars)]
    
    # NewCultivars <- NewCultivars[!(NewCultivars %in% AllCultivarRecord$toList %>% unlist())]
    # 
    # AllCultivarRecord$push(collapse = NewCultivars)
    
    TempCultivarStack <- RStack$new(collapse = NewCultivars[!is.na(NewCultivars)])
    TempTableStack    <- RStack$new()
    
    # print(CurrentDepth)
    # print(TempCultivarStack$private$.len )
    
    CurrentDepth <- CurrentDepth + 1
  }
  
  
  FinalStack$toList %>%
    purrr::reduce(bind_rows) %>%
    distinct() %>%
    {. ->> FinalCrosses} %>%
    pivot_longer(c(Female, Male)) %>%
    dplyr::select(value, Cultivar) %>%
    rename(source = value, target = Cultivar) %>%
    dplyr::filter(!is.na(source)) %>%
    map_dfc(., CleanParent) -> EdgeList
  
  return(list(CrossData = FinalCrosses, Edges = EdgeList))
}

# Read in the parentage table
AllParentage <- read_csv("./Data/SoybasePedigreeData.csv")

# Use the function to get the edges and the nodes for some genotype (here "NC-Dilday")
TreeEdges <- GetPedigree("NC-Dilday", MaxDepth = 6)$Edges
TreeNodes <- data.frame(id = unique(unlist(TreeEdges)))

# A cytoscape session has to be started for the next part. The next
# function tests if you can connect to cytoscape. Normally just wait like 20 
# seconds after a cytoscape session is opened and it should be fine. 
# Also try clicking on the cytoscape window and running the ping function
# multiple times
cytoscapePing()

# Upload the network to cytoscape
createNetworkFromDataFrames(TreeNodes, 
                            TreeEdges, 
                            title = "NC-Dilday pedigree", 
                            collection = "DataFrame Example")


## Section: Functions that do the same thing, but operate on an igraph object
####################################################################################

# Load the pre-made graph (see MakeFullGraph.R)
load("./Data/AllCrossesGraph.RData")

# This function does essentially the same thing as the GetPedigree function from above, 
# but is MUCH faster
#
# Right now, some vertices returned by this parent will only have one parent. This is because
# I'm just extracting a neighborhood of vertices up the graph from the starting cultivar based
# on how many vertices away from the starting cultivar they are.
# The upshot is that if a intermediate crosses parent is greater than this distance from
# the starting cultivar, it is excluded. This is solvable by extending the graph by one at the 
# final vertices if the vertex has only one parent, but I haven't implemented this yet. 
GetPedigree_fromGraph <- function(graph = AllCrosses_igraph, cultivar = "NC-Roy", MaxDepth = 5){
  
  LocalGraph <- make_ego_graph(graph, order = MaxDepth, cultivar, mode = "in")
  LocalGraph <- LocalGraph[[1]]
  
  Igraph_toDataframe <- function(graph){
    
    Edges <- get.edgelist(graph) %>%
      as.data.frame() %>%
      rename(source = V1, target = V2)
    
    Nodes <- V(graph) %>% names()
    NodeDF <- data.frame(id = Nodes)
    
    return(list(Edges = distinct(Edges), Nodes = distinct(NodeDF), FullGraph = graph))
  }
  
  Igraph_toDataframe(LocalGraph)
}

# An example
NC_Roy_dfs <- GetPedigree_fromGraph(cultivar = "NC-Roy", MaxDepth = 20)

cytoscapePing()

createNetworkFromDataFrames(NC_Roy_dfs$Nodes, 
                            NC_Roy_dfs$Edges, 
                            title = "NC-Roy pedigree - igraph", 
                            collection = "DataFrame Example")

# Make a network using the visNetwork package
pedigree_VisNetwork_fromGraph <- function(graph = AllCrosses_igraph, cultivar = "NC-Dilday", MaxDepth = 5){
  
  GraphData <- GetPedigree_fromGraph(graph, cultivar, MaxDepth)
  
  Edges <- GraphData$Edges %>% rename(from = source, to = target)
  Nodes <- GraphData$Nodes %>% mutate(label = id, 
                                      SoybaseURL = paste0("https://soybase.org/uniformtrial/index.php?filter=", id, "&page=lines&test=ALL"), 
                                      title = paste0("<p><a href=", SoybaseURL, ">", id,"</a></p>"))
  
  visNetwork(Nodes, Edges) %>%
    visEdges(arrows = "to") %>%
    visHierarchicalLayout(direction = "UD", 
                          sortMethod = "directed", 
                          levelSeparation = 150, 
                          edgeMinimization = FALSE)
}
pedigree_VisNetwork()


# A function to convert the pedigree graph to a dataframe and a text representation of the graph
Pedigree_asString <- function(PedigreeGraph, FullGraph = AllCrosses_igraph){
  
  Pedigree_toDF <- function(PedigreeGraph){
    
    SoybaseString <- "https://soybase.org/uniformtrial/index.php?filter=L49-4091&page=lines&test=ALL"
    
    # Topological sort of pedigree
    Ped_topo <- topo_sort(PedigreeGraph) %>% names()
    
    Ped_edges <- as_data_frame(PedigreeGraph, what = "edges")
    Ped_edges$to <- factor(Ped_edges$to, levels = Ped_topo)
    
    # Convert the edgelist to a dataframe with columns for female parent, male parent, and cultivar
    Ped_edges %>% 
      distinct() %>%
      pivot_wider(id_cols = to, names_from = name, values_from = from) %>%
      arrange(desc(to)) %>% 
      mutate(to = as.character(to)) %>%
      rename(Cultivar = to) %>%
      select(Female, Male, Cultivar) %>%
      mutate(CrossString = paste(Cultivar, "=", Female, "X", Male), 
             SoybaseURL = paste0("https://soybase.org/uniformtrial/index.php?filter=", Cultivar, "&page=lines&test=ALL"))
    
  }
  
  EdgeDF <- Pedigree_toDF(PedigreeGraph)
  MissingParents <- unique(which(is.na(EdgeDF$Male) | is.na(EdgeDF$Female)))
  
  if(length(MissingParents) > 0){
    for(i in 1:length(MissingParents)){
      
      MissingIndex <- MissingParents[[i]]
      
      res <- tryCatch({
        
        CultivarName <- as.character(EdgeDF$Cultivar[[MissingIndex]])
        
        NewRow <- make_ego_graph(FullGraph, order = 1, CultivarName, 
                                 mode = "in")[[1]] %>% Pedigree_toDF()
        
        NewRow[1, ]
      }, 
      error = function(cond){
        EdgeDF[MissingIndex, ]
      })
      
      EdgeDF[MissingIndex, ] <- res
      
    }
  }

  
  # String representation of the cross
  Ped_String <- reduce(EdgeDF$CrossString, paste, sep = ", ")
  
  return(list(PedigreeDF = EdgeDF, PedigreeString = Ped_String))
}

# Make a pedigree network from the PedigreeDF value returned by the function above
pedigree_VisNetwork_fromDF <- function(graphDF){
  
  GraphData <- graphDF %>% 
    dplyr::select(Cultivar, Male, Female, SoybaseURL) %>%
    pivot_longer(c(Male, Female)) %>%
    select(value, Cultivar, SoybaseURL) %>%
    rename(from = value, to = Cultivar)
  
  Edges <- GraphData %>% select(from, to)
  Nodes <- GraphData %>% select(from, to) %>% unlist() %>% unique()
  Nodes <- Nodes[!is.na(Nodes)]
  
  Nodes <- data.frame(id = Nodes) %>%
    mutate(label = id, 
           SoybaseURL = paste0("https://soybase.org/uniformtrial/index.php?filter=", id, "&page=lines&test=ALL"), 
           title = paste0("<p><a href=", SoybaseURL, ">", id,"</a></p>"))
    
  visNetwork(Nodes, Edges) %>%
    visEdges(arrows = "to") %>%
    visHierarchicalLayout(direction = "UD", 
                          sortMethod = "directed", 
                          levelSeparation = 150, 
                          edgeMinimization = FALSE) -> p
  return(p)
}

PedigreeVisnetwork <- function(graph = AllCrosses_igraph, cultivar = "NC-Roy", MaxDepth = 5){
  
  PedigreeList <- GetPedigree_fromGraph(graph = graph, 
                                      cultivar = cultivar, 
                                      MaxDepth = MaxDepth) %>% 
                .$FullGraph %>%
                Pedigree_asString()
  
  PedNetwork <- pedigree_VisNetwork_fromDF(PedigreeList$PedigreeDF)

  return(list(PedigreeData = PedigreeList, NetworkPlot = PedNetwork))
}
