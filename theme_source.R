#common used library
library(readxl)
library(tidyverse)
library(formattable)
library(stringr)
library(ggrepel)
library(writexl)
library(waffle)
library(patchwork)
library(wordcloud)
library(broom)

mycolors<-c("#13294b","#5c88da","#69b3e7","#da291c","#641F45","#fcb53b","#6C6F70")#1-3blue,4-5red,6yellow,7grey

#define themes functions
#
theme_lz.html <- function(){ 
theme_minimal() %+replace%    #replace elements already strips axis lines, 
  theme(
    panel.grid.major = element_blank(),    #no major gridlines
    panel.grid.minor = element_blank(),    #no minor gridlines
    axis.ticks = element_blank(),          #strip axis ticks
    axis.text.y=element_blank(),
    legend.position="bottom",
    legend.direction = "horizontal",
    strip.background = element_blank()#facet background
  )}

#font_import("/Users/linlizhou/Library/Fonts/Georgia.ttf")
#library(extrafont)
#extrafont::loadfonts()
theme_lz <- function(){ 
  font <- "Helvetica"   #assign font family up front
  theme_minimal() %+replace%    #replace elements already strips axis lines, 
    theme(
      #plot.margin = margin(t = 20, r = 10, b = 40,l = 10,unit = "pt"), 
      plot.margin=unit(c(2,0,
                    1.5,0),"cm"),
      panel.grid.major = element_blank(),    #no major gridlines
      panel.grid.minor = element_blank(),    #no minor gridlines
      plot.title = element_text(family = font, size = 8, face = 'bold',hjust = 0, vjust = 0),#hjust=0 left, vjust=-1 close to graph (move to the bottom)
      plot.subtitle=element_text(size=8, hjust=0.5, face="italic", color="black"),
      axis.title = element_text(family = font, size = 9), 
      axis.text = element_text(family = font, size = 9), 
      axis.text.x = element_blank(),#element_text(family = font, size = 9, margin = margin(t=5,b=10)),
      axis.ticks = element_blank(),          #strip axis ticks
      axis.text.y=element_blank(),
      legend.title = element_text(family = font, size=9),
      legend.margin=margin(t=-25),
      legend.text = element_text(family = font, size=7),
      legend.position="top",
      legend.direction = "horizontal",
      strip.text = element_text(family = font, size = 9, margin = margin(t=5,b=10)),#facet label text, move up is + 1
      #strip.position = "bottom",
      strip.background = element_blank()#facet background
    )}

theme_lz_ppt <- function(){
  font <- "Helvetica" #assign font family up front
  theme_minimal() %+replace% #replace elements already strips axis lines,
    theme(
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      plot.title = element_text(family = font, size = 28, hjust = 0.5, vjust = 2),
      axis.title = element_text(family = font, size = 20),
      axis.text = element_text(family = font, size = 20),
      axis.text.x = element_text(family = font, size = 20, margin = margin(t=5,b=10)),
      axis.ticks = element_blank(), #strip axis ticks
      axis.text.y=element_blank(),
      legend.title = element_text(family = font, size = 20),
      legend.text = element_text(family = font, size = 20),
      legend.position="bottom",
      legend.direction = "horizontal",
      strip.text = element_text(family = font, size = 20, margin = margin(t=5,b=10)),
      strip.background = element_blank()
    )}

