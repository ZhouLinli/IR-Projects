#common used library
library(readxl)
library(tidyverse)
library(formattable)
library(stringr)

#define themes functions
theme_lz <- function(){ 
  font <- "Helvetica"   #assign font family up front
  theme_minimal() %+replace%    #replace elements already strips axis lines, 
    theme(
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      plot.title = element_text(family = font, size = 9, face = 'bold',hjust = 0.5, vjust = 2),
      axis.title = element_text(family = font, size = 9), 
      axis.text = element_text(family = font, size = 9), 
      axis.text.x = element_text(family = font, size = 9, margin = margin(t=5,b=10)),
      axis.ticks = element_blank(),          #strip axis ticks
      axis.text.y=element_blank(),
      legend.title = element_text(family = font, size=9),
      legend.text = element_text(family = font, size=7),
      legend.position="bottom",
      legend.direction = "horizontal",
      strip.text = element_text(family = font, size = 9, margin = margin(t=5,b=10)),#move up is + 1
      strip.background = element_blank()
    )}

theme_lz_ppt <- function(){
  font <- "Helvetica" #assign font family up front
  theme_minimal() %+replace% #replace elements already strips axis lines,
    theme(
      panel.grid.major = element_blank(), #strip major gridlines
      panel.grid.minor = element_blank(), #strip minor gridlines
      plot.title = element_text(family = font, size = 24, hjust = 0.5, vjust = 2),
      axis.title = element_text(family = font, size = 20),
      axis.text = element_text(family = font, size = 20),
      axis.text.x = element_text(family = font, size = 20, margin = margin(t=5,b=10)),
      axis.ticks = element_blank(), #strip axis ticks
      axis.text.y=element_blank(),
      legend.title = element_text(family = font, size = 20),
      legend.text = element_text(family = font, size = 20),
      legend.position="bottom",
      legend.direction = "horizontal",
      strip.text = element_text(family = font, size = 20, margin = margin(t=5,b=10)),#move up is + 1
      strip.background = element_blank()
    )}

mycolors<-c("#13294b","#5c88da","#69b3e7","#da291c","#641F45","#fcb53b","#6C6F70")#1-3blue,4-5red,6yellow,7grey

