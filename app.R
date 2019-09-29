#importing the libraries, need to run one of below commented statements 
#in case the required packages are not installed
#install.packages("shiny")
#install.packages("leaflet")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("htmlwidgets")
#install.packages("r2d3")
#install.packages("networkD3")
#install.packages("ggiraph")
#install.packages("scales")
#install.packages("shinythemes")

library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(htmlwidgets)
library(r2d3)
library(networkD3)
library(ggiraph)
library(scales)
library(shinythemes)

#####importing data
otp_heat <- read.csv("otp_heat.csv")
otp_heat <- otp_heat[-1]

temp_flight2=  read.csv( "otp_time_dep.csv")
temp_flight2 <- temp_flight2[-1]

sankey<- read.csv( "sankey.csv")
sankey <- sankey[-1]

airport<- read.csv("airport.csv")
airport <- airport[-1]

route<- read.csv( "route.csv")
route <- route[-1]

crowd<- read.csv( "crowd.csv")
crowd <- crowd[-1]

route3 <- read.csv("complete_route.csv")
route3 <- route3[-1]

otp_san <- read.csv( "san_complete.csv")
otp_san <- otp_san[-1]



## ui of the app
ui <- fluidPage(
  theme = shinytheme("flatly"),
  navbarPage("Flight Performance", id = "home",
             tabPanel("Home",
                      titlePanel(h3("Introduction & Motivation")),
                      includeText("intro.txt"),
                      br(),
                      titlePanel(h3("What is the source of the data used?")),
                      includeText("source.txt"),
                      br(),
                      tags$a(href = " https://data.gov.au/dataset/ds-dga-cc5d888f-5850-47f3-815d-08289b22f5a8/distribution/dist-dga-38bdc971-cb22-4894-b19a-814afc4e8164/details?q=airline", "Data Source 1"),
                      br(),
                      tags$a(href = "https://data.gov.au/dataset/ds-dga-6d7414c5-64b7-4ee1-bc14-77cb3dc9e03a/distribution/dist-dga-b9284142-d794-412e-9a28-b5843c5c44f8/details?q=airline,", "Data Source 2"),
                      br(),
                      titlePanel(h3("Tab Information")),
                      includeText("tab.txt"),
                      br(),
                      titlePanel(h3("Contact Info")),
                      includeText("contact.txt"),
                      br(),
                      tags$a(href = "mailto: njai0003@student.monash.edu", "Contact")
                 
                      
             ),
             tabPanel("Airports and Routes",value = "subTab3",
                      sidebarLayout(
                        sidebarPanel(
                         
                          HTML(paste(
                          "This page has the information of all the Origin Routes for each Airport.",
                            br(),br(),
                            "In case you hover over the airport, you can see the origin routes for that particular airport.",
                           br(),br(),
                         " When the user clicks on the airport, it will open another tab, \"Airport Crowd\" that will show the Crowd statistics of selected airport.",
                          br(),br(),
                          "User can come back and choose to see the statistics of other airports as well."))
                        ),
                        
                        mainPanel(
                          leafletOutput("map")#,
                          # includeText("delayrate.txt")
                        ))),
             
             tabPanel("Airport Crowd",value = "subTab2",
                      sidebarLayout(
                        sidebarPanel(
                          actionButton("button", "Go back to Routes"),
                          actionButton("refresh", "Refresh to default"),
                          HTML(paste(br(),br(),"This tab is linked with the tab \"Airport Crowd\" where the crowd statistics 
                                     will be displayed for the selected Airport over the years", br(),br(),
                               "User can also hover on the graph and find the actual value of the crowd",
                               br(),br(), "For further details on the Selected Airport, use: ")),
                          uiOutput("link")
                          
                        ),
                        
                        mainPanel(
                          textOutput("error"),
                          plotOutput("plot", hover = hoverOpts("plot_hover"), height = "500"),
                          uiOutput("hover_info"),
                          br(),
                          br()
                        ))),
             tabPanel("Travel Routes and Delays",value = "subTab4",
                      sidebarLayout(
                        sidebarPanel(
                         HTML(paste(br(), "This page consists of the Route to avoid while taking a particular
                                    Airlines", br(), br(),
                                    "The Airlines are represented by the Blue nodes and Airports by the Red nodes",
                                    br(), br(),
                                    "Grey color represents the links", br(), br(), 
                                    "Hovering over any Airline node, will highlight the route to avoid.", br(), br())),
                         
                           actionButton("Airport","Airport",style="color: black; background-color: red; border-color: red"),
                         actionButton("Airline","Airline",style="color: black; background-color: blue; border-color: blue")
                         
                        ),
                        mainPanel(
                          sankeyNetworkOutput("sankey",  width = "600"),
                          br()
                          
                        ))
             )
             ,
             tabPanel("Travel Time and Delays",value = "subTab1",
                      sidebarLayout(
                        sidebarPanel(
                          HTML(paste(br(),"This page consists of the Time to avoig travelling through a particular Airline.",
                                     br(), br(),
                                     "Going from Grey to Blue, we get the aiport with minimum to maximum Delays respectively.",
                                     br(), br(),
                                     "Clicking over any tile, will highlight the month you should avoid travelling through that airline in Red color"
                          ))
                        ),
                        mainPanel(
                          ggiraphOutput("heatmap",  height = "600",
                                     width = "800"),
                          br(),
                          br()
                        ))
             ),
             tabPanel("Airline Performance",value = "subTab5",
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons("choice", "Performance Type:",
                                       c("Delay Score" = "Delay_percent",
                                         "Cancel Score" = "Cancel_percent",
                                         "Total(Cancel_Delay)" = "Total_percent"),
                                       selected = "Total_percent"),
                          
                          selectInput(
                            inputId = "airlines",
                            label = "Airlines:",
                            choices = c("ALL", levels(unique(temp_flight2$Airline))),
                            size = 6,selectize = FALSE,
                            selected = "ALL"
                          ),
                          HTML(paste("This page consists of the Airline performance in terms of Delay and Cancel Score.",
                                     br(), br(),
                                     "User can choose measure in terms of which they want to see the performance of the Airline",
                                     br(),br(),
                                     "User can also choose to select a particular Airline to show the performance over the years",
                                     br()))
                          
                         
                        ),
                        mainPanel(
                          fluidRow(
                            column(
                              width = 8,
                          d3Output("airbar", width = 900, height = 400 ),
                          br(),
                          br()))
                        ))
             )
  )
)


server <- function(input, output, session) {  
  
  
  ## subtab3 - Airports and Routes
  
  # generating the leaflet for the Australia map to show the routes for each airport
  output$map <- renderLeaflet({
    
    leaflet(data = airport) %>% addTiles() %>%
      addProviderTiles("Stamen.Watercolor")%>%
      addCircleMarkers(
        ~lng, 
        ~lat,  color = "black",weight = 5,fillOpacity = 0.5,
        radius = 5, label = ~Airport,
        labelOptions = labelOptions(noHide = TRUE, interactive  = TRUE, 
                                    offset=c(0,-15), textOnly = TRUE, direction = "left"))
    
  })
  
  # creating the mouseover and mouseout events
  
  map_lat_lng <- reactive ({ # capturing the lng and lat values of the hovered airport
    req(input$map_marker_mouseover)
    map_hover  <- input$map_marker_mouseover
    x <- data.frame(map_hover$lat, map_hover$lng)
    colnames(x) <- c("Latitude", "Longitude")
    x
  })
  
  # obeserve event for the mousover
  observeEvent(input$map_marker_mouseover,{
    
    icons <- makeIcon(
      iconUrl = "https://raw.githubusercontent.com/lifeeka/leaflet.bezier/master/demo/plane.png",
      iconWidth = 20, iconHeight = 30,
      iconAnchorX = 10, iconAnchorY = 20,
    )
    df = map_lat_lng() # using above lat lng values captured
    
    
    x2 <-route3[(route3$origin_lng==df$Longitude & route3$origin_lat==df$Latitude) ,
                ]
    # finding all the possible routes where this airport act as a origin
    
    a <- paste("No origin flights from this airport")
    
    x1 <- route[route$group %in% x2$group, ]
    
    # leaflet proxy to clear all the hovers
    map <- leafletProxy("map", data = map_lat_lng()) %>%
      clearShapes() %>%
      clearGroup("hover") %>%
      addMarkers(icon = icons, group = "hover")%>% 
      clearGroup("Polylines")
    
    
    if(length(rownames(x1))==0){ # adding hover in case no flight exists
      
      map <- map%>%
        addMarkers(map_lat_lng()$Longitude, map_lat_lng()$Latitude,
                   icon = icons, group = "hover",label   = ~a)
      
    }
    else{
      for( i in unique(x1$group)){ # generating the routes for the airport
        
        map <- map%>%
          addPolylines(data = x1[x1$group == i,], lng=~lng,lat=~lat, group = ~group, 
                       color ="green", weight = 2 )
      }
    }
    map
  })
  
  # mouse out event to clear all teh text and routes
  observeEvent(input$map_marker_mouseout,{
    leafletProxy("map") %>%
      clearShapes() %>%
      clearGroup("hover") %>%
      clearPopups()%>%
      clearGroup("Polylines")
    
    
  })
  
  ## subtab2 - to generate the crowd statistics, and action button
  
  # action button to go back to the airport routes tab, subtab3
  observeEvent(input$button, {
    # claering all teh orutes and hover texts in leaflet and switching the tabs
    leafletProxy("map") %>%
      clearShapes() %>%
      clearPopups()%>%
      clearGroup("hover") %>%
      clearGroup("Polylines")
    
    updateTabsetPanel(session, "home",selected =  "subTab3")
    
  })
  
  observeEvent(input$refresh, {
    # refreshing the page to default
    output$plot <- renderPlot({
      
      ggplot(crowd, aes(x =Year ,y = Dom_Pax_Total, group = 1)) +
        geom_point(colour = "black", size = 1.5)+
        geom_line(color = "red", size =1) +  
        theme(axis.text=element_text(size=7),
              axis.title=element_text(size=8,face="bold"),
              panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank())+
        scale_y_continuous(labels = comma)+
        scale_x_continuous(breaks = seq(2004, 2019, by= 2))+
        facet_wrap(~AIRPORT, ncol = 3)+ 
        theme(strip.text.x = element_text(size = 6),
              strip.text.y = element_text(size = 8),
              axis.title=element_text(size=14,face="bold")) +
        ylab(" Crowd Rush")+
        ggtitle("Crowd rush over the years at all the airports")
      
    })
    
    updateTabsetPanel(session, "home",selected =  "subTab2")
  })
  
  # plot for the overall crowd stats, all the facets are generated
  output$plot <- renderPlot({
    
    ggplot(crowd, aes(x =Year ,y = Dom_Pax_Total, group = 1)) +
      geom_point(colour = "black", size = 1.5)+
      geom_line(color = "red", size =1) +  
      theme(axis.text=element_text(size=7),
            axis.title=element_text(size=8,face="bold"),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank())+
      scale_y_continuous(labels = comma)+
      scale_x_continuous(breaks = seq(2004, 2019, by= 2))+
      facet_wrap(~AIRPORT, ncol = 3)+ 
      theme(strip.text.x = element_text(size = 6),
            strip.text.y = element_text(size = 8),
            axis.title=element_text(size=14,face="bold")) +
      ylab(" Crowd Rush")+
      ggtitle("Crowd rush over the years at all the airports")
    
  })
  
  
  url_link <- a("Airport", href="https://en.wikipedia.org/wiki/List_of_airports_in_Australia")
  output$link <- renderUI({
    url_link
  })
  
  # when the airport at the subtab3 is clicked, another tab, subtab2 is opened with the 
  #crowd stats of that airport
  
  observeEvent(input$map_marker_click, {
    req(input$map_marker_click)
    click <- input$map_marker_click
    
    a <- airport[airport$lng == click$lng & airport$lat == click$lat, "Airport"]
    
    
    url <- paste0("https://en.wikipedia.org/wiki/", a, "_Airport" )
    
    url_link <- a("Airport", href=url)
    output$link <- renderUI({
      url_link
    })
    
    text2<-paste("You've selected point ", a)
    
    map <- leafletProxy("map")
    map %>% clearPopups() %>% clearGroup("Polylines") %>% 
      addPopups(click$lng, click$lat, text2)
    
    #filtering the data w.r.t. that airport
    crowd_facet <- crowd[tolower(crowd$AIRPORT) == tolower(a),]
    output$error <- NULL
    
    # if no crowd data of that aiport is present, all teh factes are generated for available data
    if (nrow(crowd_facet)==0){
      
      output$error <- renderText({ paste0(a," has missing passenger data, please click the button")})
      
      output$plot <- renderPlot({
        
        ggplot(crowd, aes(x =Year ,y = Dom_Pax_Total, group = 1)) + 
          geom_point(colour = "black", size = 1.5)+
          geom_line(color = "red", size =1) +  
          theme(axis.text=element_text(size=7),
                axis.title=element_text(size=8,face="bold"),
                panel.grid.minor.y = element_blank(),
                panel.grid.minor.x = element_blank())+
          scale_y_continuous(labels = comma)+
          scale_x_continuous(breaks = seq(2004, 2019, by= 2))+
          facet_wrap(~AIRPORT, ncol = 3)+ 
          theme(strip.text.x = element_text(size = 6),
                strip.text.y = element_text(size = 8),
                axis.title=element_text(size=14,face="bold")) +
          ylab(" Crowd Rush")+
          ggtitle("Crowd rush over the years at all the airports")
        
        
      })
      
    }
    # otherwise the filtered data plot is generated
    else {
      output$plot <- renderPlot({
        
        ggplot(crowd_facet, aes(x =Year ,y = Dom_Pax_Total, group = 1)) + 
          geom_point(colour = "black", size = 5)+
          geom_line(color = "red", size =2) +  
          theme(axis.text=element_text(size=7),
                axis.title=element_text(size=8,face="bold"),
                panel.grid.minor.y = element_blank(),
                panel.grid.minor.x = element_blank())+
          scale_x_continuous(breaks = seq(2004, 2019, by= 1))+
          ylab(" Crowd Rush")+
          ggtitle(paste("Crowd rush over the years at ", a) )+ 
          theme(strip.text.x = element_text(size = 6 ),
                strip.text.y = element_text(size = 8),
                axis.title=element_text(size=14,face="bold"))
        
      })
    }
    
    # when the crowd plots are hovered, tooltip with the details will open up.
    output$hover_info <- renderUI({
      
      hover <- input$plot_hover
      
      point <- nearPoints(crowd, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
      
      if (nrow(point) == 0) return(NULL)
      
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                      "left:", left_px + 1, "px; top:", top_px + 1, "px;")
      tooltip <- paste0("<br>Year: ", point$Year,  "</br> Crowd: ", point$Dom_Pax_Total   )
      wellPanel(
        style = style,
        p(HTML(tooltip))
      )
    })
    
    updateTabsetPanel(session, "home", "subTab2")
    
  })
  
  
  # when the crowd plots are hovered, tooltip with the details will open up.
  # below is the tooltip code
  output$hover_info <- renderUI({
    
    hover <- input$plot_hover
    
    point <- nearPoints(crowd, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    
    if (nrow(point) == 0) return(NULL)
    
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 1, "px; top:", top_px + 1, "px;")
    tooltip <- paste0("<br>Year: ", point$Year,  "</br> Crowd: ", point$Dom_Pax_Total   )
    wellPanel(
      style = style,
      p(HTML(tooltip))
    )
  })
  
  ## Travel Routes and Delays
  # Output for the sankey diagram showing the routes and airlines which must be avoided
  # i.e. route and airline combination that must be avoided
  
  output$sankey <- renderSankeyNetwork({
    
    # creating links dataframe
    links=data.frame(source=sankey$source, 
                     target=sankey$target, value=sankey$value)
    links$group = as.factor(sankey$group)
    # creating the nodes dataframe
    nodes=data.frame(name=c(as.character(links$source), as.character(links$target)) %>% unique())
    
    
    # creating IDs for the links source and targets
    links$IDsource=match(links$source, nodes$name)-1 
    links$IDtarget=match(links$target, nodes$name)-1
    
    # asisgning groups to nodes as it is there for links so as to asisgn different colors to each group
    nodes$group <- as.factor(ifelse(nodes$name %in% unique(otp_san$Airline), 1,2))
  
    # asisgning colors
    mycolor1 <- JS('d3.scaleOrdinal().domain([1,2, 3, 4]).range(["blue","red", "grey", "grey"])')
    
    # Make the Network
    sn <- sankeyNetwork(Links = links, Nodes = nodes,
                        Source = "IDsource", Target = "IDtarget",
                        Value = "value", NodeID = "name", NodeGroup = "group", 
                        nodeWidth = 10, margin = list(left = 0), iterations = 0, 
                        sinksRight=TRUE,  colourScale = mycolor1, LinkGroup = "group", fontSize = 10) 
     
    
    
    # assigning back the source and target IDs to the network so as to pull in the JS
    sn$x$links$IDsource <-links$IDsource
    sn$x$links$IDtarget <-links$IDtarget
    
    # D3 function for the hover and higlighting the route(links)
    clickJS <-' function(){
      link = d3.selectAll(".link");
      
      d3.selectAll(".node").on("mousedown.drag", null).select("title foreignObject body pre")
      .text(function(d) { 
      if(d.group != 2){
      if ( d.sourceLinks.length != 0 ){
          if ( d.targetLinks.length != 0 ){
      t =  d.targetLinks[0].source.name + " -> " + d.name + " ->" + 
      d.sourceLinks[0].target.name +" ,   Delay/Cancel Score: "+ parseInt(d.value)
      
          }
          else {
           t =d.name + " -> " + d.sourceLinks[0].target.name  +" ,   Delay/Cancel Score: "+ parseInt(d.value)
          }
      }
          else{
           t =  d.targetLinks[0].source.name + " ->" + 
     d.name  +" ,   Delay/Cancel Score: "+ parseInt(d.value)
      }
      return t; }
      else{
      return d.name;
      }});
      
      d3.selectAll(".node").on("mouseover",function(d) { 
   
      link
      .style("stroke-opacity", function(d1) {
      if(d.group != 2){
     
        return d1.source.name == d.name || d1.target.name  == d.name ? 1 : 0;} })
        .select("title foreignObject body pre")
      .text(function (d1) {
            
      t = d1.source.name + " ->"+  d1.target.name + ", Score:" + parseInt(d1.value)
      return t; })
      
        
      })
      d3.selectAll(".node").on("mouseout",function(d) {   
      link
      .style("stroke-opacity", function(d1) { return 1; });
      })
    }
      ' 
    onRender(sn,  clickJS) # on render function to implement the D3 query
  })

  
  ## gerenting the heatmap for subtab1 
  
  # heatmap wor each airline and its month
  output$heatmap <- renderggiraph({
    tooltip = sprintf("Airline: %s<br/>Month: %s<br/>Delays: %.02f", 
                      otp_heat$Airline, otp_heat$Month_Num, otp_heat$Delays) 
    data_id = sprintf("%s", otp_heat$Airline)
    
    # ggplot for the heatmap
    p <- ggplot(data = otp_heat, aes(x = Airline, y = Month_Num) ) +
      geom_tile_interactive(aes(fill = Delays, tooltip = tooltip, data_id = data_id), colour = "white") +
      scale_fill_gradient(low = "grey", high = "blue") +
      coord_equal()+
      theme_minimal() +
      theme(
        legend.position = "right",
        panel.grid.minor = element_line(color = "transparent"),
        panel.grid.major = element_line(color = "transparent"),
        axis.ticks.length   = unit(2, units = "mm"),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
        axis.title = element_text(size = 9, colour = "gray30"),
        axis.text.y = element_text(hjust = 1, size = 9, colour = "gray40"),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 9, colour = "gray40"),
        legend.title=element_text(face = "bold", hjust = 0.5, size=8),
        legend.text=element_text(size=6))+
      scale_y_continuous(breaks = seq(1,12, by = 1))+
      ggtitle("Heatmap of Airlines v/s Month, month to avoid for the airline")+
      ylab("Month")
    ggiraph(ggobj = p)
  })
  
  
  observeEvent(input$heatmap_selected,{
      
      tooltip = sprintf("Airline: %s<br/>Month: %s<br/>Delays: %.02f", 
                        otp_heat$Airline, otp_heat$Month_Num, otp_heat$Delays) 
      data_id = sprintf("%s", otp_heat$Airline)
  
      # finding the month with max delay_cancel score so as to decide the time to avoid
      df <- otp_heat[otp_heat$Airline==input$heatmap_selected,]
      
      df <- aggregate(df[, c("Delays")], by = list(df$Airline), max, na.rm = T)
      colnames(df) <- c("Airline","Delays")
      
      mnt <- as.numeric(otp_heat[otp_heat$Airline == df$Airline & otp_heat$Delays== df$Delays, "Month_Num"])
      
      #
      text = paste("For Airline ",input$heatmap_selected, " Avoid Month: ", mnt)
     
      p <- ggplot(data = otp_heat, aes(x = Airline, y = Month_Num) ) +
        geom_tile_interactive(aes(fill = Delays, tooltip = tooltip, data_id = data_id), colour = "white") +
        scale_fill_gradient(low = "grey", high = "blue") +
        coord_equal()+
        theme_minimal() +
        theme(
          legend.position = "right",
          panel.grid.minor = element_line(color = "transparent"),
          panel.grid.major = element_line(color = "transparent"),
          axis.ticks.length   = unit(2, units = "mm"),
          plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
          axis.title = element_text(size = 9, colour = "gray30"),
          axis.text.y = element_text(hjust = 1, size = 9, colour = "gray40"),
          axis.text.x = element_text(angle = 90, hjust = 1, size = 9, colour = "gray40"),
          legend.title=element_text(face = "bold", hjust = 0.5, size=8),
          legend.text=element_text(size=6))+
        scale_y_continuous(breaks = seq(1,12, by = 1))+
        ggtitle("Heatmap of Airlines v/s Month, month to avoid for the airline")+
        ylab("Month")+
        annotate_interactive(
          "tile", x =input$heatmap_selected, y = mnt, 
           color = " red",fill = "red", 
          alpha = .5)
     
      output$heatmap <- renderggiraph({
      ggiraph(ggobj = p, selection_type = "single" )
 
  }) 
  }) 
  
  ## generating the subTab 5 which is the flight performance, uses d3 code for this
  
  tab_list <- NULL
  
  #filerting the flight records
  sel_flights <- reactive({

    if (input$airlines != "ALL") temp_flight2 <- filter(temp_flight2, Airline == input$airlines)
    
    temp_flight2 <- temp_flight2[, 
                                 c("Year","Airline", "Departing_Port", input$choice)]
    colnames(temp_flight2)[colnames(temp_flight2) == input$choice] <- "Percent"
    
    temp_flight2
    
  })
  
  # bar graph to be made
  bar_graphD3=reactive({
    grouped <- ifelse(input$airlines != "ALL", 
                      expr(Year), expr(Airline)
    )
    flightdata <- sel_flights() %>% # grouping on the basis of selected variable
      group_by(!! grouped) %>%
      summarise(Percent = round(mean(Percent),2)) %>%
      mutate(
        y = Percent,
        x = !! grouped
      )
   
    flightdata <- flightdata %>%
      mutate(label = x)
   
    
    # passing to r2d3 funtion to generate graoh using d3 script
    r2d3(flightdata, "v1.js")
  })
  
  
  output$airbar = renderD3({
    bar_graphD3()
    
  })
  
  # airline/year bar click event
  observeEvent(input$airlines != "", {
   
   # if (input$airlines == "ALL") {
      updateSelectInput(session, "airlines", selected = input$airlines)
  #  }
  }, ignoreInit = TRUE )
  
}

shinyApp(ui, server)