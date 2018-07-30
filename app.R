# set up ----

# set working directory, if not already set - only works in RStudio (with rstudioapi)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# script with campfire functionality for floors and wall
source("~/ShinyApps/NYS_Explorer/NYS_Lib.R")

# running app ----

campfireApp(
  
  # controller: where selections and possibly shiny app information reside ----
  controller = fluidPage(tweaks,
                         fluidRow(column(width = 12, 
                                         list(h3("Controller: NYS Health Data Explorer"), 
                                              # checkbox slector for which regions to look at
                                              tags$div(align = 'left', 
                                                       class = 'multicol', 
                                                       checkboxGroupInput(inputId = "region",
                                                                          label = "Regions to Examine:",
                                                                          choices = c("Capital District"=1, "Central New York"=2,
                                                                                      "Finger Lakes"=3, "Long Island"=4,
                                                                                      "Mid-Hudson"=5, "Mohawk Valley"=6,
                                                                                      "New York City"=7, "North Country"=8,
                                                                                      "Southern Tier"=9, "Tug Hill Seaway"=10,
                                                                                      "Western Region"=11),
                                                                          inline = F)
                                              ) 
                                         ))),
                         fluidRow(column(width = 12, 
                                         list( 
                                              # checkbox slector for which indices to look at
                                              tags$div(align = 'left', 
                                                       class = 'multicol', 
                                                       checkboxGroupInput(inputId = "index",
                                                                          label = "Indices to Examine:",
                                                                          choices = c("Socio-Econmic"=1, "Physical Health"=2,
                                                                                      "Mental Health"=3, 
                                                                                      "Long Lasting Health Interventions"=4,
                                                                                      "Medical Care"=5, "Lifestyle"=6,
                                                                                      "Self Reported Health"=7, "Care Taking"=8),
                                                                          inline = F)
                                              ) 
                                         ))),
                         fluidRow(column(width = 12, 
                                         list( 
                                           # checkbox slector for which order to order the starting bars in
                                           tags$div(align = 'left', 
                                                    class = 'multicol', 
                                                    radioButtons(inputId = "order",
                                                                       label = "Indices to Examine:",
                                                                       choices = c("Alphabetical"=0,"Socio-Economic"=1, 
                                                                                   "Physical Health"=2,"Mental Health"=3, 
                                                                                   "Long Lasting Health Interventions"=4,
                                                                                   "Medical Care"=5, "Lifestyle"=6,
                                                                                   "Self Reported Health"=7, "Care Taking"=8),
                                                                       inline = F)
                                           ) 
                                         ))),
                         # select which modes to look at
                         fluidRow(column(width = 4, 
                                         tags$div(align = 'left',
                                                  selectInput("mode", "Mode to Examine:",
                                                              choices = list("Select a mode"=0,"Region Comparison"=1, 
                                                                             "Region Explorer"=2, "Index Explorer"=3),
                                                              selected=NULL))),
                                  column(width = 8,
                                         tags$div(align = 'left',
                                                  selectInput("map", "Choose a Map:",
                                                              choices = c("Education","General Health","Socio-Economic Index",
                                                                          "Physical Health Index","Mental Health Index",
                                                                          "Long Lasting Health Interventions Index",
                                                                          "Medical Care Index", "Lifestyle Index",
                                                                          "Neutral"),
                                                              selected = "Neutral")))),
                         # action button to update pictures
                         fluidRow(column(width = 2, 
                                         tags$div(align = 'left',
                                                  actionButton("submit","Update"))),
                                  column(width = 10,
                                         tags$div(align = 'left',
                                                  actionButton("reset","Reset Map"))))
  ),
  
  # Wall output ----
  wall = fluidPage(
            fluidRow(
              column(width = 3,
                plotOutput("image1", height = "650px")),
              column(width = 3,
                plotOutput("image2", height = "650px")),
              column(width = 3,
                plotOutput("image3", height = "650px")),
              column(width = 3,
                plotOutput("image4", height = "650px")),
    style="background: rgb(0, 0, 0);"
  )),
  
  # Floor output ----
  floor = div(
    leafletOutput("ny_regions",width=1515,height=900),
    style="background: rgb(255, 255, 255);"
  ),
  
  # External monitor output ----
  monitor = fluidPage(
              fluidRow(
                verbatimTextOutput("monitorText"),
                style="background: rgb(0, 0, 0);"
              ),
              style="background: rgb(0, 0, 0);"
            ),
  
  # render output here ----
  
  serverFunct = function(serverValues, output, session) {
    # this is where we access the serverValues passed in the library script
    # only plots should be rendered here -- little to no computation.
    
    # Monitor render ----
    output$monitorText <- renderText({ 
      return("This is the monitor!!")
    })
    
    # Wall four plot renders ----
    load("~/ShinyApps/NYS_Explorer/defaultRenders.RData")
    output$image1 <- renderPlot({
      if(is.null(serverValues$submit)) {
        defaultRenders[[1]]
      }
      else {
        serverValues$p1
      }
    })
    output$image2 <- renderPlot({
      if(is.null(serverValues$submit)) {
        defaultRenders[[2]]
      }
      else {
        serverValues$p2
      }
    })
    output$image3 <- renderPlot({
      if(is.null(serverValues$submit)) {
        defaultRenders[[3]]
      }
      else {
        serverValues$p3
      }
    })
    output$image4 <- renderPlot({
      if(is.null(serverValues$submit)) {
        defaultRenders[[4]]
      }
      else {
        serverValues$p4
      }
    })
      
    # #####################
    # Floor render ----   #
    # #####################
    # Floor render setup ---- 
    rv<-reactiveValues()
    rv$vec<-c()
    
    user_input<-"New York"
    proxy_map<-"General"
    observeEvent(serverValues$map, {
      if(serverValues$map!=user_input){
        updateCheckboxGroupInput(session, "region", selected = character(0))
        updateCheckboxGroupInput(session, "index", selected = character(0))
        updateSelectInput(session,"mode",selected = 0)
        updateRadioButtons(session,"order",selected = 0)
        user_input<-serverValues$map
      }
    })

    # Floor renderLeaflet ----
    output$ny_regions<-renderLeaflet({
      if(!is.null(serverValues$submit)){
        newDf <- serverValues$d_f
        color_range<-serverValues$pal_range
        palette<-serverValues$pall
        if(serverValues$map=="Neutral"){
          factpal<-colorFactor(palette,newDf[,2])
          ny_regions<-leaflet(n) %>%
            setView(lng=-76,lat=43,zoom=7)%>%
            addProviderTiles(providers$CartoDB.DarkMatter)%>%
            addPolygons(stroke=TRUE,weight=1,smoothFactor = 0.3,opacity = 1,
                        fillOpacity = 1, fillColor = ~factpal(newDf[,2]),
                        layerId = n$NAME,label = ~paste0(NAME," : ",newDf[,2]))
          return(ny_regions)
        }
        else {
          pal<-colorNumeric(palette,color_range)
          ny_regions<-leaflet(n)%>%
            setView(lng=-76,lat=43,zoom=7)%>%
            addProviderTiles(providers$CartoDB.DarkMatter)%>%
            addPolygons(stroke=TRUE, weight=1,  smoothFactor = .3, opacity=1,
                        fillOpacity = 1,fillColor = ~pal(newDf[,2]),
                        layerId = n$NAME,label=~paste0(NAME," : ",newDf[,2]))
          return(ny_regions)
        }
      }
      else{
        ny_regions<-leaflet(n)%>%
          setView(lng=-76,lat=43,zoom=7)%>%
          addProviderTiles(providers$CartoDB.DarkMatter)%>%
          addPolygons(stroke=TRUE, weight=1,  smoothFactor = .3, opacity=1, 
                      fillOpacity = 1,fillColor = topo.colors(11, alpha = NULL),
                      layerId = n$NAME,label=~paste0(NAME))
        return(ny_regions)
      }
    })
    # Floor render observes ----
    observe({
      rv$vec<-serverValues$region
    })
    
    observe({
      if(!is.null(serverValues$submit)) {
        if(serverValues$map!=proxy_map) {
          proxy_map<<-serverValues$map
        }
        else {
          proxy<-leafletProxy("ny_regions")
          proxy%>%clearGroup("Outline")
          if(length(rv$vec) > 0){
            for(i in 1:length(rv$vec)) {
              if(rv$vec[i]==1) {region<-subset(n,n$NAME=="Capital District")}
              else if(rv$vec[i]==2) {region<-subset(n,n$NAME=="Central New York")}
              else if(rv$vec[i]==3) {region<-subset(n,n$NAME=="Finger Lakes")}
              else if(rv$vec[i]==4) {region<-subset(n,n$NAME=="Long Island")}
              else if(rv$vec[i]==5) {region<-subset(n,n$NAME=="Mid-Hudson")}
              else if(rv$vec[i]==6) {region<-subset(n,n$NAME=="Mohawk Valley")}
              else if(rv$vec[i]==7) {region<-subset(n,n$NAME=="New York City")}
              else if(rv$vec[i]==8) {region<-subset(n,n$NAME=="North Country")}
              else if(rv$vec[i]==9) {region<-subset(n,n$NAME=="Southern Tier")}
              else if(rv$vec[i]==10) {region<-subset(n,n$NAME=="Tug Hill Seaway")}
              else if(rv$vec[i]==11) {region<-subset(n,n$NAME=="Western Region")}
              proxy%>%addPolylines(stroke = TRUE,weight = 5,color = "white",data = region,group = "Outline")
            }
          }
        }
      }
    })
    
    # Floor render reset ----
    observeEvent(serverValues$reset,{
      proxy<-leafletProxy("ny_regions")
      proxy%>%clearGroup("Outline")
      updateCheckboxGroupInput(session = session,"region",selected=character(0))
      updateCheckboxGroupInput(session = session,"index",selected=character(0))
      updateSelectInput(session=session,"mode",selected = 0)
      updateRadioButtons(session=session,"order",selected=0)
      rv$vec<-c()
    })
  }
)

