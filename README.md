# Soybase Pedigree Visualization  
Visualization tools for the Soybase soybean parentage data.

## Overview  
The main (and only for now) script is **CytoscapePedigree.R**. This script uses the soybean parentage data in the **SoybasePedigreeData.csv** table to  make pedigree trees for a given soybean cultivar with Cytoscape. Because of this [Cytoscape](https://cytoscape.org/) has to be installed for the script to work. 

After that, the easiest way to use this script would probably be to clone this repository as a new project in RStudio and run it from there. I'd also recommend using the [yFiles Hierarchic layout](http://manual.cytoscape.org/en/stable/Navigation_and_Layout.html#yfiles-layouts) in cytoscape to visualize the pedigrees. The other layouts have tended to look pretty confusing. 

## Future work  
- A shiny app that includes cytoscape would be nice so that everything can be done in the same place. The [cyjShiny](https://github.com/cytoscape/cyjShiny) package looks promising, but I'll have to mess around with it.  
- Add functions to make more traditional text based pedigrees. While not as helpful for visualizing the pedigrees (IMO), could still be helpful to augment the plots with an alternate representation of the same information. 
