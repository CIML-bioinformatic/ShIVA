
library(shiny)
library(shinydashboard)
library(gplots)
library(scales)
library(shinyFiles)
library(shinybusy)
library(dplyr)
library(shinycssloaders)
library(DT)
library(plotly)
library(Seurat)
library(reshape2)
library(reshape)
library(stringr)
library(corrplot)
library(shinyWidgets)
library(biomaRt)
library(pheatmap)
library(heatmaply)
library(leiden)
library(webshot)
library(shinyalert)
library(stringr)
library(kableExtra)
library(org.Mm.eg.db)
library(clusterProfiler)
library(org.Hs.eg.db)

options(shiny.maxRequestSize = 1000 * 1024^10)


path = getwd()

#source all the files in the dir to load the modules
for (nm in list.files(path, pattern = "\\_module.[RrSsQq]$")) {
  source(file.path(path, nm))
}


help_infos <- read.csv("References/help_infos.csv", header = T)

cc.genes_mouse <- readRDS("References/mouse_cell_cycle_genes.rds")


#some functions used in different modules

# Basic function to convert human to mouse gene names
convertHumanGeneList <- function(x){
  
  human = readRDS("References/human_db_biomart")
  mouse = readRDS("References/mouse_db_biomart")
  
  genesV2 = getLDS(attributes = c("hgnc_symbol"), filters = "hgnc_symbol", values = x , mart = human, attributesL = c("mgi_symbol"), martL = mouse, uniqueRows=T)
  
  humanx <- unique(genesV2[, 2])
  
  return(humanx)
}


# Basic function to convert mouse to human gene names
convertMouseGeneList <- function(x){
  
  human = readRDS("References/human_db_biomart")
  mouse = readRDS("References/mouse_db_biomart")
  
  genesV2 = getLDS(attributes = c("mgi_symbol"), filters = "mgi_symbol", values = x , mart = mouse, attributesL = c("hgnc_symbol"), martL = human, uniqueRows=T)
  humanx <- unique(genesV2[, 2])
  
  return(humanx)
}


source('dowbloadTable_module.R', local = TRUE)
source('downloadPlot_module.R', local = TRUE)
