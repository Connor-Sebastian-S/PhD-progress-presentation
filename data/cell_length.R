library(tidyverse)
library(splitstackshape) 
library(stringr)
library(reshape2)
library(plotly)
library(dplyr)

# Read in the CSV 
cell_dataset <- read.csv('Position000000.csv')

# How many frames are there in total, and what cell are we interested in?
cell = 1
frame_count = 45

# Define a few different variables with only specific columns
cellid <- subset(cell_dataset, select = 1)
frames <- subset(cell_dataset, select = 3)
mother <- subset(cell_dataset, select = 2)
daughter <- subset(cell_dataset, select = 4)
length <- subset(cell_dataset, select = 8)

# Create a data.frames
df <- data.frame(length)
df$length <- str_squish(df$length)
df$length <- gsub('\\[', '', df$length)
df$length <- gsub('\\]', '', df$length)
df$length <- gsub(". ", ", ", df$length)
df <- cSplit(df, 'length', ',')
df <- t(df)

df2 <- data.frame(frames)
df2$frames <- str_squish(df2$frames)
df2$frames <- gsub('\\[', '', df2$frames)
df2$frames <- gsub('\\]', '', df2$frames)
df2$frames <- gsub(". ", ", ", df2$frames)
df2 <- cSplit(df2, 'frames', ',')
df2 <- t(df2)

# Get new cell count count
cell_count = ncol(df2)

# Fill data.frames
df3 <- df2
df3[,]=matrix(ncol=ncol(df3), rep(NA, prod(dim(df3))))

for(i in 1:cell_count){
for (j in 1:frame_count){
      df3[df2[i,j],j] <- df[i,j]
  }
}

# Run this separately!
df3[df2[1,46],46] <- df[1,46]


df3 <- tibble::rowid_to_column(df3, "CellID")
df4 <- data.frame(df3)
df4 <- tibble::rowid_to_column(df4, "frame")

# Melt data.frame for plotting
df5 <- melt(df4, id.vars="frame")
df5$variable <- gsub('X', ' Cell', df5$variable)

# Plot
ggplotly(ggplot(data=df5, aes(frame,value, col=variable)) + 
           geom_point() + 
           geom_line() +
           labs(title = "Cell length over time", colour ="Cell ID")+
           xlab("Frame") +
           ylab("Length")
           )

df_daughter <- data.frame(daughter) 
df_daughter$daughter <- str_squish(df_daughter$daughter)
df_daughter$daughter <- gsub('\\[', '', df_daughter$daughter)
df_daughter$daughter <- gsub('\\]', '', df_daughter$daughter)
df_daughter$daughter <- gsub(". ", ", ", df_daughter$daughter)
df_daughter <- cSplit(df_daughter, 'daughter', ',')
df_daughter = df_daughter[,-1]
df_daughter <- data.frame(df_daughter) 
df_daughter <- tibble::rowid_to_column(df_daughter, "CellID")
df_daughter <- t(df_daughter)
df_daughter <- data.frame(df_daughter) 
df_daughter <- tibble::rowid_to_column(df_daughter, "frame")

df_daughter_filtered <- filter(df_daughter, frame == 12)

df_mother <- data.frame(mother) 
df_mother <- tibble::rowid_to_column(df_mother, "CellID")

df_mother_filtered <- filter(df_mother, CellID == 3)



dfdf <- filter(df5, variable == ' Cell1')

length_mean<-dfdf%>%
  summarise(value=mean(value, na.rm = TRUE))
print(length_mean)

length_median<-dfdf%>%
  summarise(value=median(value, na.rm = TRUE))
print(length_median)

length_min<-dfdf%>%
  summarise(value=min(value, na.rm = TRUE))
print(length_min)

length_max<-dfdf%>%
  summarise(value=max(value, na.rm = TRUE))
print(length_max)
