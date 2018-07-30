# Fruit Campfire Library
# by Hannah De los Santos
# Originated on: 7/17/18

# load libraries, data, variables, and misc functions ----

# libraries
library(shiny)
library(ggplot2)
library(ggcorrplot)
library(reshape2)
library(dplyr)
library(readr)
library(grid)
library(leaflet)
library(mapdata)
library(maps)
library(mapproj)
library(maptools)
library(jsonlite)
library(rgeos)
library(geojson)



# Data Load for both Wall and Floor ----
# data source: https://health.data.ny.gov/Health/Behavioral-Risk-Factor-Surveillance-Survey-2016/7kmq-z6s7
cleanedFullIndex <- read.csv("~/ShinyApps/NYS_Explorer/cleanedFullIndex.csv") # Created indices
averages <- read.csv("~/ShinyApps/NYS_Explorer/indexAverages.csv") # Averages of these indices by region
realdata <- read.csv("~/ShinyApps/NYS_Explorer/realdata.csv") # A subset of the brfss survey which is used
Overall_health_averages <- read.csv("~/ShinyApps/NYS_Explorer/OverallHealthAverages.csv", header=FALSE)
n<-geojsonio::geojson_read("~/ShinyApps/NYS_Explorer/nycounties_to_regions..json",what="sp")
load("~/ShinyApps/NYS_Explorer/GenHealthWrkSpce.RData", gen <- new.env())
load("~/ShinyApps/NYS_Explorer/waffleSetup.RData")
index_values<-read.csv("~/ShinyApps/NYS_Explorer/floorIndexAverages.csv")
load("~/ShinyApps/NYS_Explorer/brfssData.RData")

# Setup Floor ----
gen<-as.list(gen)
gen_health<-gen$brfss.data
gen_health<-as.data.frame(gen_health)

floorAverages<-Overall_health_averages
colnames(floorAverages) <- c("X__1","X__2")
floorAverages$X__2<-as.numeric(floorAverages$X__2)
colnames(floorAverages)[colnames(floorAverages)=="X__1"]<-"NAME"
colnames(floorAverages)[colnames(floorAverages)=="X__2"]<-"Freq"
health.ny.data<-merge(n,floorAverages,by="NAME")

education_heatmap<-as.data.frame(table(gen_health$DSRIPREG,gen_health$EDUCA))
colleges<-as.data.frame(education_heatmap[(12:22),])
colleges<-colleges[order(colleges$Freq),]
colnames(colleges)[colnames(colleges)=="Var1"]<-"NAME"
college.ny.data<-merge(n,colleges,by="NAME")

index_values<-as.data.frame(index_values)
colnames(index_values)[colnames(index_values)=="X"]<-"NAME"
index_averages<-merge(n,index_values,by="NAME")

# Setup Wall ----
regions = levels(factor(cleanedFullIndex$Region)) #gets list of regions
all_indices = colnames(cleanedFullIndex) #gets list of columns
socio = c("numadult","MEDCOST","EDUCA","RENTHOM1","CPDEMO1","EMPLOY1","INCOME2","INTERNET","STRSRENT")
selfReported = c("GENHLTH","PHYSHLTH","MENTHLTH")
physhealth = c("CVDINFR4","CVDCRHD4","CVDSTRK3","ASTHMA3","ASTHNOW","CHCSCNCR","CHCOCNCR","CHCCOPD1","HAVARTH3",
               "CHCKIDNY","DIABETE3","RMVTETH3","PREDIAB1","BPHIGH4")
mentalHealth = c("MENTHLTH","ADDEPEV2","ACEDEPRS","ACEDRINK","ACEDRUGS","ACEPRISN","ACEDIVRC","ACEPUNCH","ACEHURT",
                 "ACESWEAR","ACETOUCH","ACETTHEM","ACEHVSEX")
medicalCare = c("HLTHPLN1","PERSDOC2","CHECKUP1","LASTDEN3")
llhi = c("FLUSHOT6","PNEUVAC3","TETANUS","HIVTST6","PDIABTST","SEXHIST")
lifestyle = c("EXERANY2","SMOKE100","SMOKDAY2","USENOW3","ECIGARET","ALCDAY5","HIVRISK4","EVERWALK","EVERBIKE","EATFRUIT","EATVEGET")
careTaking = c("CAREGIV1","CRGVHRS1","CRGVEXPT","FALREDUC")
indexAttributes <- list(Socioecon=socio, Self.reported=selfReported, Physical.health=physhealth, Mental.health=mentalHealth, 
                        Medical.care=medicalCare, Long.Lasting.Health.Intervention=llhi, Lifestyle=lifestyle, Care.taking=careTaking)

# functions

# tweaks, a list object to set up multicols for checkboxGroupInput ----
# source: https://stackoverflow.com/questions/29738975/how-to-align-a-group-of-checkboxgroupinput-in-r-shiny
tweaks <- list(tags$head(tags$style(HTML("
                                         .multicol { 
                                         height: 150px;
                                         -webkit-column-count: 5; /* Chrome, Safari, Opera */ 
                                         -moz-column-count: 5;    /* Firefox */ 
                                         column-count: 5; 
                                         -moz-column-fill: auto;
                                         -column-fill: auto;
                                         } 
                                         "))
)
)

# Campfire App function ----

# In this function, this is where you do the bulk of your
# processing: subsetting data to be passed to visualizations,
# calculations, etc.

campfireApp = function(controller = NA, wall = NA, floor = NA, monitor=NA, serverFunct = NA) {
  ui <- campfireUI(controller, wall, floor, monitor)
  
  # our reactive values that we will pass to our rendering
  serverValues = reactiveValues()
  
  # function where the computation (server) happens
  campfire_server <- shinyServer(function(input, output, session) {
    
    observeEvent(input$submit, {
      # Change input to serverValues ----
      # reassign everything from the input to the serverValues
      # from now on, only reference input values with serverValues
      
      for (inputId in names(input)) {
        serverValues[[inputId]] <- input[[inputId]]
      }
      
      # Floor Computations ----
      if (serverValues$map=="Education") {
        d__f<-cbind(college.ny.data$NAME,college.ny.data$Freq)
        pal__range<-c(100,2000)
        pal_ette <-"viridis"
      }
      else if (serverValues$map=="General Health") {
        d__f<-cbind(health.ny.data$NAME,health.ny.data$Freq)
        pal__range<-c(2.5,4)
        pal_ette <-"viridis"
      }
      else if(serverValues$map=="Socio-Economic Index") {
        d__f<-cbind(index_averages$NAME, index_averages$Socioecon)
        pal__range<-c(14,17)
        pal_ette<-"viridis"
      }
      else if(serverValues$map=="Physical Health Index") {
        d__f<-cbind(index_averages$NAME, index_averages$Physical.health)
        pal__range<-c(86,93)
        pal_ette<-"viridis"
      }
      else if(serverValues$map=="Mental Health Index") {
        d__f<-cbind(index_averages$NAME, index_averages$Mental.health)
        pal__range<-c(100,102)
        pal_ette<-"viridis"
      }
      else if(serverValues$map=="Long Lasting Health Intervention Index") {
        d__f<-cbind(index_averages$NAME, index_averages$Long.Lasting.Health.Intervention)
        pal__range<-c(9.5,11.5)
        pal_ette<-"viridis"
      }
      else if(serverValues$map=="Medical Care Index") {
        d__f<-cbind(index_averages$NAME, index_averages$Medical.care)
        pal__range<-c(20,21.5)
        pal_ette<-"viridis"
      }
      else if(serverValues$map=="Lifestyle Index") {
        d__f<-cbind(index_averages$NAME, index_averages$Lifestyle)
        pal__range<-c(35.5,37.5)
        pal_ette<-"viridis"
      }
      else if(serverValues$map=="Neutral") {
        if(serverValues$order==0){
          d__f<-cbind(index_averages$NAME, index_averages$regions)
          pal__range<-c(0,1)
          pal_ette<-colorRampPalette(c("red", "orange","green"))(11)
        }
        else if(serverValues$order==1){
          d__f<-cbind(index_averages$NAME, index_averages$Socioecon)
          pal__range<-c(0,1)
          pal_ette<-colorRampPalette(c("red", "orange","green"))(11)
        }
        else if(serverValues$order==2){
          d__f<-cbind(index_averages$NAME, index_averages$Physical.health)
          pal__range<-c(0,1)
          pal_ette<-colorRampPalette(c("red", "orange","green"))(11)
        }
        else if(serverValues$order==3){
          d__f<-cbind(index_averages$NAME, index_averages$Mental.health)
          pal__range<-c(0,1)
          pal_ette<-colorRampPalette(c("red", "orange","green"))(11)
        }
        else if(serverValues$order==4){
          d__f<-cbind(index_averages$NAME, index_averages$Long.Lasting.Health.Intervention)
          pal__range<-c(0,1)
          pal_ette<-colorRampPalette(c("red", "orange","green"))(11)
        }
        else if(serverValues$order==5){
          d__f<-cbind(index_averages$NAME, index_averages$Medical.care)
          pal__range<-c(0,1)
          pal_ette<-colorRampPalette(c("red", "orange","green"))(11)
        }
        else if(serverValues$order==6){
          d__f<-cbind(index_averages$NAME, index_averages$Lifestyle)
          pal__range<-c(0,1)
          pal_ette<-colorRampPalette(c("red", "orange","green"))(11)
        }
      }
      
      d_f<-d__f
      pal_range<-pal__range
      pall<-pal_ette
      
      # Making sure inputs cannot be misconfigured in controller ----
      observe({
        if(length(input$index) > 4)
        {
          updateCheckboxGroupInput(session, "index", selected= head(input$index,4))
        }
      })
      
      # Setup for bargraph renderPlot ----
      colors <- colorRampPalette(c("red", "orange", "green"))( 11 )
      lineColors <- colorRampPalette(c("red","orange","green"))( 11 )
      selected <- rep(0, 11)
      
      # Set up colors, as well as what should be highlighted
      for(i in 1:11) {
        if(i %in% serverValues$region) {
          selected[i] <- 1
        }
      }
      
      # Set up data frame to hold selected columns
      selected <- as.data.frame(cbind.data.frame(selected, averages$Region))
      colnames(selected) <- c("isSelected","Regions")
      
      
      # Set the columns that are designated to be highlighted to have line color "black"
      for(i in 1:length(selected[["isSelected"]])) { # color all the line colors correctly
        if(selected[i,1] == 1) {
          lineColors[i] <- "white"
        }
      }
      
      # Dealing with the reordering
      #   - Fix the legend order
      #   - Fix the lineColor order
      colnum <- as.numeric(serverValues$order)
      if(colnum == 0) {averages$Region <- sort(averages$Region)}
      else{averages$Region <- reorder(averages$Region, averages[,colnum])}
      # Reorder lineColors
      averCopy <- averages[,colnum]
      df <- rbind(lineColors, averCopy)
      df <- df[,order(df[2,])]
      lineColors <- df[1,]
      
      # Remake the line colors, leaving the "black" colors where they are
      newlineColors <- colorRampPalette(c("red","orange","green"))(11)
      for(i in 1:length(lineColors)) {
        if(lineColors[i] == "white") {
          newlineColors[i] <- "white"
        }
      }
      lineColors <- newlineColors
      
      # Custom theme function ----
      theme_new <- function(){
        theme(
              axis.text=element_text(colour="white"),
              axis.title=element_text(colour="white"),
              plot.title=element_text(colour="white"),
              legend.text=element_text(colour="white"),
              plot.background = element_rect(fill = "black"),
              legend.background = element_rect(fill = "black"),
              panel.background = element_rect(fill = 'black', colour = 'white'),
              plot.margin=unit(c(0,0,0,0),"cm")
          )
      }
      # All the generic plots used for renderPlot ----
      # Generic bargraphRender function ----
      barRender <- function(index, c, lc) {
        minY <- floor(min(averages[[index]]))
        maxY <- ceiling(max(averages[[index]]))
        ggplot(averages, aes(x=Region, y=averages[[index]], fill=Region, color=Region)) +
          geom_bar(stat="identity", size=1.5) +
          ggtitle(paste("Average", index, "score by Region", sep = " ")) +
          ylab(paste(index, "Index Score", sep = " ")) +
          geom_text(aes(label=round(averages[[index]], digits = 3)), vjust=1.6, color="white", size=3.5) +
          scale_fill_manual(values = c) +
          scale_color_manual(values = lc) +
          coord_cartesian(ylim = c(minY, maxY)) +
          theme_new() +
          theme(axis.title.x = element_blank(),
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank())
      }
      # Generic densityRender functon ----
      # Param r_vector : Represents a vector of regions, by number. The numbers correspond to the levels of the facted regions from the original brfss DSRIPREG feature. Range: 1 - 11
      # Param i_vector : Represents a vector of indexes by number. Range: 1-3
      # Pre : 1 >= length(r_vector) >= 11 && max(r_vector) <= 11 && min(r_vector) >= 1
      # Pre : 1 >= length(i_vector) >= 8  && max(i_vector) <= 8  && min(i_vector) >= 1
      # Post : Makes length(i_vector) density plots of the indexes chosen in i_vector
      densityRender <- function(r_vector, i_vector){
        if(length(i_vector) > 3) {stop("Error in i_vector, too many indices, max length 3")}
        if(max(i_vector) > 8) {stop("Error in i_vector, index out of bounds, max 8")}
        
        regions = levels(factor(cleanedFullIndex$Region))
        all_indices = colnames(cleanedFullIndex)
        
        fullIndexSubset = subset(cleanedFullIndex, Region %in% regions[r_vector], append(i_vector, match("Region", all_indices)))
        
        correct_indices <- all_indices[i_vector]
        ggplot(fullIndexSubset, aes_string(correct_indices[1])) +
          geom_density(aes(fill=factor(Region)), alpha=0.4) +
          labs(title=paste("Density of", correct_indices[1], "by region"),
               subtitle=paste(correct_indices[1], "grouped by region", sep=" "),
               x=correct_indices[1],
               fill="Region") +
          theme_new()
      }
      # Generic boxplotRender function ----
      boxplotRender <- function(r_vector, i_vector) {
        regions = levels(factor(cleanedFullIndex$Region))
        all_indices = colnames(cleanedFullIndex)
        subsetIndex = subset(cleanedFullIndex, Region %in% regions[r_vector], append(i_vector, match("Region", all_indices)))
        plot1 <- ggplot(subsetIndex, aes(Region, subsetIndex[,1], group = Region)) +
          geom_boxplot(fill="red", color="green") +
          ylab("") +
          ggtitle(paste("Boxplot of", all_indices[i_vector[1]], "by selected regions", sep=" ")) +
          theme_new()
        
        plot2 <- ggplot(subsetIndex, aes(Region, subsetIndex[,2], group = Region)) +
          geom_boxplot(fill="plum", color="grey40") +
          ylab("") +
          ggtitle(paste("Boxplot of", all_indices[i_vector[2]], sep=" ")) +
          theme_new()
        
        if(length(i_vector) == 1) {
          return(plot1)
        }
        else {
          return(c(plot1,plot2))
        }
      }
      # Generic scatterRender function ----
      # Pre : r_vector from 1-11
      # Pre : i_vector length = 2, i_vector from 1-8
      # Post : single scatterplot which has one of the i_vector indexes on the x and the other on the y
      scatterRender <- function(r_vector, i_vector){
        if(length(i_vector) != 2) {stop("Number of indices must be 2")}
        dimensions = all_indices[i_vector]
        subsetIndex = subset(cleanedFullIndex, Region %in% regions[r_vector], append(i_vector, match("Region", all_indices)))
        
        # Added code to only sample if there is enough variation in both indexes
        copyi1 <- subsetIndex[,1]
        copyi2 <- subsetIndex[,2]
        fact <- factor(copyi1)
        i1Levels <- nlevels(fact)
        fact <- factor(copyi2)
        i2Levels <- nlevels(fact)
        
        
        if(i1Levels > 50 && i2Levels > 50) {
          mySample = subsetIndex[sample(1:nrow(subsetIndex), 1000),]
        }
        else {
          mySample = subsetIndex
        }
        g = ggplot(mySample, aes_string(x=dimensions[1], y=dimensions[2]))
        print(g + geom_point(aes(col=Region)) +
                geom_smooth(method="glm", se=F) + 
                labs(title=paste(dimensions[1], "Index vs", dimensions[2], "Index", sep=" "),
                     x=dimensions[1],
                     y=dimensions[2]) +
                theme_new())
      }
      # Generic divergingVerticalRegionBarRender function ----
      divergingVerticalRegionBarRender <- function(r_vector, i_vector) {
        regionAverages <- aggregate(cleanedFullIndex, by=list(cleanedFullIndex$Region), FUN=mean, na.rm=TRUE)
        trimmedData <- cleanedFullIndex[-9]
        stateAverages <- colMeans(trimmedData)
        
        #devData structure
        devData = data.frame(regions[r_vector], c(""), c(""))
        names(devData) = c("Region", "Deviation", "Status")
        
        for(i in i_vector){
          index = colnames(cleanedFullIndex)[i] 
          regionDeviations = c()
          regionStatus = c()
          for(region in regions[r_vector]){
            regionAverage = subset(regionAverages, Group.1 == region)[i+1] 
            stateAverage = stateAverages[index] 
            if(!is.na(regionAverage) & !is.na(stateAverage)){ 
              deviation = as.numeric(regionAverage) - as.numeric(stateAverage)
              regionDeviations[[length(regionDeviations)+1]] = deviation 
              regionStatus[[length(regionStatus)+1]] = ifelse(deviation > 0, "above", "below") 
            }
          }
        }
        
        devData$Deviation = regionDeviations
        devData$Status = regionStatus
        
        devData = devData[order(devData$Deviation), ]
        devData$Region = factor(devData$Region, levels=devData$Region)
        
        
        ggplot(devData, aes(x=Region, y=Deviation, label=Deviation)) +
          geom_bar(stat='identity', aes(fill=Status), width=.5) +
          scale_fill_manual(name="Region Average",
                            labels=c("Above State Average", "Below State Average"), 
                            values=c("above"="#00ba38", "below"="#f8766d")) + 
          labs(subtitle=sprintf("Normalized %s", index), 
               title=sprintf("Deviance from average %s value for selected regions", index)) + 
          coord_flip() +
          theme_new()
      }
      # Generic divergingVerticalIndexBarRender function ----
      divergingVerticalIndexBarRender = function(r_vector, i_vector){
        
        regions = levels(factor(cleanedFullIndex$Region))
        indices = colnames(cleanedFullIndex)
        
        trimmedData = cleanedFullIndex[-9] 
        
        stateAverages = colMeans(trimmedData)
        
        regionOfInterest = regions[r_vector]
        
        regionData = subset(cleanedFullIndex, cleanedFullIndex$Region %in% regionOfInterest, select = -c(Region))
        stateData = subset(cleanedFullIndex, select = -c(Region))
        stateAverages = colMeans(stateData)
        regionAverages = colMeans(regionData)
        
        devData = data.frame(indices[i_vector], c(""), c(""))
        names(devData) = c("Index", "Deviation", "Status")
        
        for(r in r_vector){
          region = regions[r]
          
          indicesDeviations = c()
          indicesStatus = c()
          
          for(index in indices[i_vector]){
            indexAverage = regionAverages[r]
            
            stateAverage = stateAverages[index]
            if(!is.na(indexAverage) & !is.na(stateAverage)){
              deviation = as.numeric(indexAverage) - as.numeric(stateAverage)
              indicesDeviations[[length(indicesDeviations)+1]] = deviation
              indicesStatus[[length(indicesStatus)+1]] = ifelse(deviation > 0, "above", "below")
            }
          }
          
          devData$Deviation = indicesDeviations
          devData$Status = indicesStatus
          
          devData = devData[order(devData$Deviation), ]
          devData$Region = factor(devData$Index, levels=devData$Index)
          
          g <- ggplot(devData, aes(x=Index, y=Deviation, label=Deviation)) + #initializes plot
            geom_bar(stat='identity', aes(fill=Status), width=.5) + 
            scale_fill_manual(name="Index Average", labels=c("Above Index Average", "Below Index Average"), 
                              values=c("above"="#00ba38", "below"="#f8766d")) + 
            labs(subtitle=sprintf("Normalized %s", region), 
                 title=sprintf("Deviance from average index score for %s", region)) +
            coord_flip() +
            theme_new()
          return(g)
        }
      }
      # Generic divergingVerticalIndexHorzRender function ----
      divergingVerticalIndexHorzRender = function(r_vector, i_vector, rotate=FALSE){
        
        regions = levels(factor(cleanedFullIndex$Region))
        indices = colnames(cleanedFullIndex)
        
        trimmedData = cleanedFullIndex[-9] 
        
        stateAverages = colMeans(trimmedData)
        
        regionOfInterest = regions[r_vector]
        
        regionData = subset(cleanedFullIndex, cleanedFullIndex$Region %in% regionOfInterest, select = -c(Region))
        stateData = subset(cleanedFullIndex, select = -c(Region))
        stateAverages = colMeans(stateData)
        regionAverages = colMeans(regionData)
        
        devData = data.frame(indices[i_vector], c(""), c(""))
        names(devData) = c("Index", "Deviation", "Status")
        
        for(r in r_vector){
          region = regions[r]
          
          indicesDeviations = c()
          indicesStatus = c()
          
          for(index in indices[i_vector]){
            indexAverage = regionAverages[r]
            
            stateAverage = stateAverages[index]
            if(!is.na(indexAverage) & !is.na(stateAverage)){
              deviation = as.numeric(indexAverage) - as.numeric(stateAverage)
              indicesDeviations[[length(indicesDeviations)+1]] = deviation
              indicesStatus[[length(indicesStatus)+1]] = ifelse(deviation > 0, "above", "below")
            }
          }
          
          devData$Deviation = indicesDeviations
          devData$Status = indicesStatus
          
          devData = devData[order(devData$Deviation), ]
          devData$Region = factor(devData$Index, levels=devData$Index)
          
          g = ggplot(devData, aes(x=Index, y=Deviation, label=Deviation)) #initializes plot
          
          
          g = g + geom_bar(stat='identity', aes(fill=Status), width=.5) + 
            scale_fill_manual(name="Index Average", labels=c("Above Index Average", "Below Index Average"), 
                              values=c("above"="#00ba38", "below"="#f8766d")) + 
            labs(subtitle=sprintf("Normalized %s", region), 
                 title=sprintf("Deviance from average index score for %s", region))
          if(rotate == TRUE) {g = g + coord_flip()}
          
          g = g + theme_new() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
          
          return(g)
        }
      }
      # Generic waffleRender function ----
      waffleRender <- function(indices,name){
        for (i in 1:length(names(indices))) {
          if(names(indices)[i] == name){
            oneindex <- indices[[i]]
            index <- data.frame(region = names(oneindex),ncases = round(oneindex))
            index[,1] = names(oneindex)
            ndeep <- 5
            
            tb4waffles <- expand.grid(y = 1:ndeep,
                                      x = seq_len(ceiling(sum(index$ncases) / ndeep)))
            
            regionvec <- rep(index$region, index$ncases)
            tb4waffles$region <-c(regionvec, rep("empty", nrow(tb4waffles) - length(regionvec)))
            
            p <- ggplot(tb4waffles, aes(x = x, y = y, fill = region)) + 
              geom_tile(color = "white") + # The color of the lines between tiles
              scale_fill_manual("attibutes",
                                values=c("#CC0000", "#006600", "#669999", "#00CCCC", 
                                         "#660099", "#CC0066", "#FF9999", "#FF9900", 
                                         "black", "indianred", "purple", "cyan", "darksalmon","yellow","blue"))+
              coord_flip()+
              theme_new() +  
              theme(axis.title.x=element_blank(),
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank(),
                    axis.title.y = element_blank(),
                    axis.text.y = element_blank(),
                    axis.ticks.y = element_blank(),
                    plot.title = element_text(hjust = 0.5)) +
              ggtitle(paste("Compositions of", name,"index",sep = " "))
            
            return(p)
          }
        }  
      }
      # Generic contourRender function ----
      contourRender = function(r_vector, i_vector) {
        regions = levels(factor(cleanedFullIndex$Region))
        indices = colnames(cleanedFullIndex)
        
        x = indices[i_vector[1]]
        y = indices[i_vector[2]]
        z = indices[i_vector[3]]
        
        density2d = subset(cleanedFullIndex, Region %in% regions[r_vector], select=append(i_vector, match("Region", indices)))
        
        ggplot(mapping=aes_string(x=x, y=y, z=z, color="chartreuse")) +
          stat_density2d(data=density2d, aes(color=sprintf("%s",z))) + 
          labs(title=sprintf("Average %s in specified regions", z),
               subtitle=sprintf("plotted as a function of %s and %s", x, y)) +
          theme(legend.title = element_blank()) +
          theme_new()
      }
      # Generic correlogramRender function ----
      correlogramRender <- function(r_vector){
        data <- matrix(nrow = 9 , ncol = 0)
        for (i in 1:nrow(cleanedFullIndex)) {
          if(cleanedFullIndex[i,9]==r_vector){
            data <- rbind(data,cleanedFullIndex[i,])
          }
        }
        corr <- round(cor(na.omit(data)[,1:8]), 1)
        ggcorrplot(corr, hc.order = TRUE, 
                   type = "lower", 
                   lab = TRUE, 
                   lab_size = 3, 
                   method="circle", 
                   colors = c("tomato2", "white", "springgreen3"), 
                   title=paste("Correlations between indices in", r_vector),
                   ggtheme=theme_bw) +
          theme_new() +
          theme(axis.text.x = element_text(size=10),
                axis.text.y = element_text(size=10))
      }
      # Generic indexCorrelRender function ----
      indexCorrelRender <- function(index){
        attributes = indexAttributes[[index]]
        mymat = realdata[attributes]
        
        indexSubset = cleanedFullIndex[index]
        
        Correlations <- c()
        for(attr in attributes){
          corr = sprintf("%0.2f", cor(indexSubset, mymat[attr]))
          Correlations <- c(Correlations, corr)
        }
        
        newdataframe <- as.data.frame(t(rbind(attributes, Correlations)))
        ggplot(data=newdataframe, aes(x=attributes, y=Correlations, fill=attributes)) +
          geom_bar(stat="identity") +
          theme_new() +
          theme(axis.text.x=element_blank(),
                axis.ticks.x=element_blank()) + 
          ggtitle(sprintf("Correlation between overall %s and its attributes", index))
      }
      # Generic featureCorRender function ----
      featureCorRender <- function(index){
        Correlations <- c()
        names <- c()
        for(i in 1:8){
          if(index == names(cleanedFullIndex)[i]) {next} #if you comment out this line, it will display 1 to 1 relationship with itself
          
          Correlations <- c(Correlations, sprintf("%0.2f", cor(cleanedFullIndex[,i], cleanedFullIndex[index])))
          names <- c(names, names(cleanedFullIndex)[i])
        }
        
        newdataframe <- as.data.frame(t(rbind(names,Correlations)))
        ggplot(data=newdataframe[1:7,], aes(x=names, y=Correlations, fill=names)) +
          geom_bar(stat="identity") +
          theme_new() +
          theme(axis.text.x=element_blank(),
                axis.ticks.x=element_blank()) + 
          ggtitle(sprintf("Correlations between %s and other indices", index))
      }
      # Generic featureBarRender function ----
      featureBarRender <- function(index) {
        attributes = indexAttributes[[index]]
        mymat = realdata[attributes]
        
        indexSubset = cleanedFullIndex[index]
        
        Correlations <- c()
        for(attr in attributes){
          corr = sprintf("%0.2f", cor(indexSubset, mymat[attr]))
          Correlations <- c(Correlations, corr)
        }
        Correlations <- as.data.frame(Correlations)
        rownames(Correlations) <- indexAttributes[[index]]
        
        indexOfCharts <- returnTopTwo(Correlations)
        first <- rownames(Correlations)[indexOfCharts[1]]
        second <- rownames(Correlations)[indexOfCharts[2]]
        
        p1 <- ggplot(brfss.data, aes(brfss.data[[first]], fill = brfss.data[[first]])) +
          geom_histogram(stat="count") +
          ggtitle(paste("Catagories of", first, "by count")) +
          theme_new() +
          theme(axis.title.x = element_blank(),
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank())
        p2 <- ggplot(brfss.data, aes(brfss.data[[second]], fill = brfss.data[[second]])) +
          geom_histogram(stat="count") +
          ggtitle(paste("Catagories of", second, "by count")) +
          theme_new() +
          theme(axis.title.x = element_blank(),
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank())
        return(list(p1,p2))
      }
      # Return the index of the top 2 from the data frame ----
      returnTopTwo <- function(relations) {
        dataFrame <- c()
        for(i in 1:length(relations)) {
          dataFrame[i] <- relations[i]
        }
        dataFrame <- as.matrix(dataFrame[[1]])
        dataFrame <- as.numeric(dataFrame)
        max <- max(dataFrame)
        indexOfMax <- -1
        for(i in 1:length(dataFrame)) {
          if(dataFrame[i] == max) {
            indexOfMax <- i
          }
        }
        dataFrame[indexOfMax] <- -1
        max <- max(dataFrame)
        indexOfMax2 <- -1
        for(i in 1:length(dataFrame)) {
          if(dataFrame[i] == max) {
            indexOfMax2 <- i
          }
        }
        return(c(indexOfMax, indexOfMax2))
      }
      # ##########################################
      # Using the render functions to create correct plots ----
      # ##########################################
      # Mode 0 (start screen) renders ----
      ind <- colnames(averages)
      if(serverValues$mode == '0') {
        if(length(serverValues$index)<1) { # There is nothing selected, show default
          p1 <- barRender("Socioecon", colors, lineColors)
        }
        else {
          selectedLabel <- ind[as.numeric(serverValues$index[1])]
          p1 <- barRender(selectedLabel, colors, lineColors)
        }
        
        if(length(serverValues$index)<2) {
          p2 <- barRender("Physical.health", colors, lineColors)
        }
        else {
          selectedLabel <- ind[as.numeric(serverValues$index[2])]
          p2 <- barRender(selectedLabel, colors, lineColors)
        }
        if(length(serverValues$index)<3) {
          p3 <- barRender("Long.Lasting.Health.Intervention", colors, lineColors)
        }
        else {
          selectedLabel <- ind[as.numeric(serverValues$index[3])]
          p3 <- barRender(selectedLabel, colors, lineColors)
        }
        if(length(serverValues$index)<4) {
          p4 <- barRender("Medical.care", colors, lineColors)
        }
        else {
          selectedLabel <- ind[as.numeric(serverValues$index[4])]
          p4 <- barRender(selectedLabel, colors, lineColors)
        }
      }
      
      # Mode 1 (region comparison) single index ----
      if(serverValues$mode == '1' && length(serverValues$index)==1) {
        indexNames <- colnames(averages)
        index <- indexNames[as.numeric(serverValues$index[1])]
        r_vector <- c(as.numeric(serverValues$region))
        i_vector <- c(as.numeric(serverValues$index[1]))
        p1 <- densityRender(r_vector, i_vector)
        p2 <- boxplotRender(r_vector, i_vector)
        p3 <- divergingVerticalRegionBarRender(r_vector, i_vector)
        p4 <- waffleRender(newindices, index)
      }
      
      # Mode 1 (region comparison) dual index ----
      if(serverValues$mode == '1' && length(serverValues$index)==2) {
        r_vector <- c(as.numeric(serverValues$region))
        i_vector <- c(as.numeric(serverValues$index))
        plot1 <- boxplotRender(r_vector, i_vector[1])
        plot2 <- boxplotRender(r_vector, i_vector[2])
        p1 <- densityRender(r_vector, i_vector[1])
        p2 <- densityRender(r_vector, i_vector[2])
        p3 <- scatterRender(r_vector, i_vector[1:2]) 
        p4 <- plot1 # NOTE : fix for boxplot when multiple indexes are selected
      }
      # Mode 1 (region comparison) triple index ----
      if(serverValues$mode == '1' && length(serverValues$index)==3) {
        r_vector <- c(as.numeric(serverValues$region))
        i_vector <- c(as.numeric(serverValues$index))
        p1 <- densityRender(r_vector, i_vector[1])
        p2 <- densityRender(r_vector, i_vector[2])
        p3 <- densityRender(r_vector, i_vector[3])
        p4 <- contourRender(r_vector, i_vector[1:3])
      }
      # Mode 2 (region explorer) ----
      if(serverValues$mode == '2') {
        regions <- averages$Region
        index <- as.numeric(serverValues$region[1])
        r_vector <- as.numeric(serverValues$region)
        i_vector <- c(1:8)
        p1 <- divergingVerticalIndexBarRender(r_vector, i_vector)
        p2 <- correlogramRender(regions[index])
        p3 <- divergingVerticalIndexHorzRender(r_vector, i_vector)
        i_vector <- c(1,2)
        p4 <- scatterRender(r_vector, i_vector)
      }
      # Mode 3 (index explorer) ----
      if(serverValues$mode == '3') {
        indexNames <- colnames(averages)
        index <- indexNames[as.numeric(serverValues$index[1])]
        plots <- featureBarRender(index)
        p1 <- indexCorrelRender(index)
        p2 <- featureCorRender(index)
        p3 <- plots[[1]]
        p4 <- plots[[2]]
      }
      # Add to serverValues ----
      # Add p1:4 to serverValues for Wall ----
      # add this to serverValues, which we will use to create the plot
      # if you want to access a value when plotting, add it to serverValues
      serverValues[["p1"]] <- p1
      serverValues[["p2"]] <- p2
      serverValues[["p3"]] <- p3
      serverValues[["p4"]] <- p4
      # Add to serverValues for Floor ----
      serverValues[["d_f"]] <- d_f
      serverValues[["pal_range"]] <- pal_range
      serverValues[["pall"]] <- pall

    })
    
    serverFunct(serverValues, output, session)
    
  })
  
  # options(shiny.port = 6666)
  shinyApp(ui, server = campfire_server)
}

# campfire ui ----

# This is the section that controls what the user sees
# when they load the app. No need to edit anything in 
# this section.

campfireUI = function(controller, wall, floor, monitor) {
  ui <- shinyUI(bootstrapPage(
    HTML('<script type="text/javascript">
         $(function() {
         $("div.Window").hide(); 
         var tokens = window.location.href.split("?");
         if (tokens.length > 1) {
         var shown_window = tokens[1];
         $("div."+shown_window).show();
         } else {
         $("div.WindowSelector").show();
         }
         });
         </script>'),
    div(class="WindowSelector Window",
        HTML('<h2><a href="?Controller">Controller</a></h2>'),
        HTML('<h2><a href="?Wall">Wall</a></h2>'),
        HTML('<h2><a href="?Floor">Floor</a></h2>'),
        HTML('<h2><a href="?Monitor">External Monitor</a></h2>'),
        style='position: absolute; 
        top: 50%; left: 50%; 
        margin-right: -50%; 
        transform: translate(-50%, -50%)'
    ),
    div(class="Controller Window",
        controller
    ),
    div(class="Wall Window",
        wall 
    ),
    div(class="Floor Window",
        floor
    ),
    div(class="Monitor Window",
        monitor
    )
    
    ))
  
  return(ui)
}
