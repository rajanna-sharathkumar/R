library(raster) # to get map shape file
library(ggplot2) # for plotting and miscellaneuous things
library(ggmap) # for plotting
library(plyr) # for merging datasets
library(scales) # to get nice looking legends
library(maps)

rm(list=ls())

#function to get map data
get_map_data <- function(){
  #Get map Data
  usa.df <- map_data("state")
  # str(usa.df)
  
  #Rename coulmn region as state
  colnames(usa.df) [5] <- "state"
  usa.df$state <- as.factor(usa.df$state)
  usa.df$group <- as.numeric(usa.df$group)
  return (usa.df)
}

#function to get abbreviation and join with map data
get_abb <- function(usa.df){
  #Get state names
  x <- map("state.carto")
  state.names <- data.frame("state"=x$names)
  # str(state.names)
  
  #Get abbreviation of state names
  abb <- state.abb[state.names$state]
  
  #create a data frame with state names and its abbreviation
  state_abb <- data.frame("state" = state.names$state,"stateabb" = abb)
  # levels(xyz$state)
  
  #Convert to lower case
  state_abb$state <- tolower(state_abb$state)
  
  #Merge the abbreviated data with the map data
  usa.df <- join(usa.df, state_abb, by = "state", type = "inner")
  # str(usa.df)
}

get_file_data <- function(){
  #Read data from csv file
  usa.dat <- read.csv(file.choose(), header = T,fill = TRUE)[ ,c('Year','id','state','Top1_adj')]
  return (usa.dat)
}

plot_income <- function(usa.df,usa.dat,year,year1){
  
  if(missing(year1)){
    disp <- paste("Top 1% of income earners in ", year, sep=" ")
    file_name <- paste(year,"Income map.pdf",sep = "_")
    
    #Remove the redundant data by taking subset of data
    usa.dats <- subset(usa.dat,Year==year)
    usa.dats$state <- tolower(usa.dats$state)
    colnames(usa.dats)[colnames(usa.dats)=="Top1_adj"] <- "plot"
    # str(usa.dats)
    # levels(usa.dat$state)
  }
  
  else{
    year_name <- paste(year,year1,sep = "-")
    disp <- paste("Top 1% of income earners in ", year_name, sep=" ")
    year_name <- paste(year,year1,sep = "_")
    file_name <- paste(year_name,"Income map.pdf",sep = "_")
    
    #Remove the redundant data by taking subset of data
    usa.year <- subset(usa.dat,Year==year)
    usa.year1 <- subset(usa.dat,Year==year1)
    plot_data <- usa.year$Top1_adj - usa.year1$Top1_adj
    
    usa.year$plot <- plot_data
    usa.dats <- usa.year
    usa.dats$state <- tolower(usa.dats$state)
    # return(usa.dats)
    # str(usa.dats)
    # levels(usa.dat$state)
  }
  #Join the file data with map data
  usa.df <- join(usa.df, usa.dats, by = "state", type = "inner")
  # str(usa.df)
  
  #Find the range and assign break points accordingly
  # diff = range(usa.df$plot)
  brk <- c(-10,-7,-5,0,5,7,10,15,17,20)
  
  #Plot the file data on map with color varying according to the data
  p <- ggplot() +
    geom_polygon(data = usa.df, aes(x = long, y = lat, group = group, fill = plot), 
                 color = "black", size = 0.25) +	
    scale_fill_distiller(palette = "Reds", breaks = brk, trans = "reverse") +
    theme_nothing(legend = TRUE) +
    labs(title = disp, fill = "")
  
  #Display map with file data
  # p
  
  #aggregate the data to plot abbreviation on to the map
  stannote <- aggregate(cbind(long, lat, group, id) ~ stateabb, data = usa.df, 
                        FUN=function(x)mean(range(x)))
  
  #Plot state abbreviations on the map
  p1 <- p + geom_text(data=stannote, aes(x=long, y=lat, group = group,label=stateabb), size=2, hjust=-0.1,vjust=0.1)
  
  #Display map with state abbreviations
  p1
  
  #save the resulting map into a pdf file
  ggsave(p1, file = file_name)
  # return (usa.dats)
  
}

###### RUN ONLY ONCE ##############

#Get map data and file data
Map_Data = get_map_data()
Map_Data = get_abb(Map_Data)
File_Data = get_file_data()

###################################

x = plot_income(Map_Data,File_Data,2012)
x = plot_income(Map_Data,File_Data,1999)
x = plot_income(Map_Data,File_Data,2012,1999)
