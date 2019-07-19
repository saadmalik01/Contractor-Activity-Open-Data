
##server.R## 



                         ####   Reactive dataframes ####




# fuction to create dataframe to be used for the chart.

shinyServer(function(input, output,session) {
  
  ####instructions for each tab####
  
  
  #for intro tab.
  observeEvent(input$introhelp, {
    
    rintrojs::introjs(session, options = list(
      "skipLabel"="Close",
      steps = data.frame(position = c("auto","auto","top","auto"
      ),
      
      element = c(NA,"#tabs","#introbox1", "#xx"
      ),
      intro = c("The following walk through is designed to help understand how to use this page. Click on the next button to go to the next instruction. Click back to move to the previous instruction. Click close to close the instructions.",
                "Each tab can be selected by clicking on the button for the tab.",
                "Another way to jump to the desired tab is to click the relevant box for the tab.",
                "This concludes the walk through for this page. Click on 'Done' to close this box."))
    ))
    
  })
  
  #for linechart tab.
  observeEvent(input$help, {
    
    rintrojs::introjs(session, options = list(
      "skipLabel"="Close",
      steps = data.frame(position = c("auto","auto","auto","auto","top","auto","top","auto","auto","auto"
                                      ),
                         
                         element = c(NA,"#board","#hscp","#Pharmacy","#y","#Dates","#c","#Plot","#pharmacyline", "#xx"
                         ),
                         intro = c("The following walk through is designed to help understand how to use this page. Please select at least one pharmacy in order to view the chart. Click on the next button to go to the next instruction. Click back to move to the previous instruction. Click close to close the instructions.",
                                   "This 'Filter by NHS Board' box filters pharmacies by NHS Board. Click on the arrow pointing downwards to view NHS Boards. Click on a NHS Board to select it.",
                                   "This 'Filter by Health and Social Care Partnership' box filters pharmacies by Health and Social Care Partnership (HSCP). Click on the arrow pointing downwards to view HSCPs. HSCPs belonging to the NHS board selected via 'Filter by NHS Board' will appear in this box. Click on a HSCP to select it.",
                                   "Type the name or postcode of the pharmacy here. Pharmacies belonging to the HSCP selected via the 'Filter by Health and Social Care Partnership' will appear in this box. At least one pharmacy has to be selected in order to generate the chart. A maximum number of five pharmacies can be shown on the chart.",
                                   "The 'Select Measure' box selects the measure to be displayed on the chart.",
                                   "This is a date slider. Drag the circles on either side to choose a date range.",
                                   "This selects the colour pallete for the chart. 'Blues' shows pharmacies in varying shades of blue. 'Colours' shows pharmacies in differrent colours.",
                                   "Click on 'Download Chart' to download the chart as a png.",
                                   "This is a line chart which shows pharmacy trends for the selected time period and measure. It will be generated when one or more pharmacies have been selected via the 'Select Pharmacy' box.",
                                   "This concludes the walk through for this page. Click on 'Done' to close this box."))
    ))
    
  })
  
  #for map.
  observeEvent(input$help2, {
    
    rintrojs::introjs(session, options = list(
      "skipLabel"="Close",
      steps = data.frame(position = c("auto","top","top","auto","auto"
      ),
      
      element = c(NA,"#Dates3","#y2","#map","#xx"
      ),
      intro = c("The following walk through is designed to help understand how to use this page. Click on the next button to go to the next instruction. Click back to move to the previous instruction. Click close to close the instructions.",
                "The 'Select time period' box shows the three months for which pharmacy activity can be seen on the map. Click on the arrow pointing downwards to view all the avaialble time periods. Click on a time period to select it.",
                "The 'Select Measure' box shows the selected measure. At least on measure needs to be selected to show pharmacies on the map. Click on the arrow pointing downwards to view all available measures. Click on a measure to select it.",
                "The map shows activity for all pharmacies in Scotland for the selected time period and measure. Each pharmacy is represented as a circle on the map. Drag the map by holding on to the right click button on the mouse and moving the mouse in the desired direction. Click the ' + ' button to zoom into the map. Click ' - ' to zoom out of the map. The zoom function also works by scrolling the mouse. High activity is represented by darker shades of orange. Low activity is represented by lighter shades of orange.", 
                "This concludes the walk through for this page. Click on 'Done' to close this box."
                ))
    ))
    
  })
  
  #for linechart tab.
  observeEvent(input$help3, {
    
    rintrojs::introjs(session, options = list(
      "skipLabel"="Close",
      steps = data.frame(position = c("auto","auto","auto","auto","top","auto","top","auto","auto"
      ),
      
      element = c(NA,"#board2","#hscp2","#Pharmacy2","#x","#Dates2","#downloaddata","#pharmacytable", "#xx"
      ),
      intro = c("The following walk through is designed to help understand how to use this page. Please note that at least one pharmacy and one measure need to be selected in order to generate the table. Click on the next button to go to the next instruction. Click back to move to the previous instruction. Click close to close the instructions.",
                "This 'Filter by NHS Board' box filters pharmacies by NHS Board. Click on the arrow pointing downwards to view NHS Boards. Click on a NHS Board to select it.",
                "This 'Filter by Health and Social Care Partnership' box filters pharmacies by Health and Social Care Partnership (HSCP). Click on the arrow pointing downwards to view HSCPs. HSCPs belonging to the NHS board selected via 'Filter by NHS Board' will appear in this box. Click on a HSCP to select it.",
                "Type the name or postcode of the pharmacy here. Pharmacies belonging to the HSCP selected via the 'Filter by Health and Social Care Partnership' will appear in this box. At least one pharmacy has to be selected in order to generate the chart.",

                
                "This 'Select Measure' box selects one or more measures to be displayed in the data table. At least one measure needs to be selected to generate the data table.",
                "This is a date slider. Drag the circles on either side to choose a date range.",
                "Clicking the 'Download Data' button downloads the data table as a CSV.",
               
                "This is the the data table. The table will be generated when one or more measures have been selected in the 'Select Measure' box and one or more pharmacies have been selected in 'Select Pharmacy' box.",
                "This concludes the walk through for this page. Click on 'Done' to close this box."))
    ))
    
  })
  
  
  #### observe events to follow select input ####
  
  #board
  observeEvent(input$board, # id= input$Measure for select input in bubble chart.
               {
                 lapply(c("board2"), #updates all other tabs.
                        function(x) updateSelectInput(session = session,
                                                      inputId = x,
                                                      selected = input$board)) # updates with 
                 #the new measure.
               }
  )
  
  observeEvent(input$board2, # id= input$Measure for select input in bubble chart.
               {
                 lapply(c("board"), #updates all other tabs.
                        function(x) updateSelectInput(session = session,
                                                      inputId = x,
                                                      selected = input$board2)) # updates with 
                 #the new measure.
               }
  )
  
  #hscp
  observeEvent(input$hscp, # id= input$Measure for select input in bubble chart.
               {
                 lapply(c("hscp2"), #updates all other tabs.
                        function(x) updateSelectInput(session = session,
                                                      inputId = x,
                                                      selected = input$hscp)) # updates with 
                 #the new measure.
               }
  )
  observeEvent(input$hscp2, # id= input$Measure for select input in bubble chart.
               {
                 lapply(c("hscp"), #updates all other tabs.
                        function(x) updateSelectInput(session = session,
                                                      inputId = x,
                                                      selected = input$hscp2)) # updates with 
                 #the new measure.
               }
  )
  

  
  

  
  AllPharmacies  <- reactive({
    ContractorActivity %>%
      filter(Pharmacy %in% input$Pharmacy) %>%
      select(Date, Pharmacy, input$y) 
             
    
  })
  
 
 # function to create dataframe for datatable.
  
  AllPharmacies2  <- function(){
   
    # set up date for slider.
    a <- ContractorActivity[ContractorActivity$Date >= input$Dates2[1] & ContractorActivity$Date <= input$Dates2[2] ,]
    
    
     a  %>%
      select(Date, Pharmacy, input$x) %>%
      filter(Pharmacy %in% input$Pharmacy2) %>%
      mutate(Date=format(as.Date(Date), "%b-%Y")) 
    
  }
  
  ####observe event to take user to desired tab####
  
  #go to boxplot tab:
  observeEvent(input$trendtablink, {
    
    updateTabsetPanel(session, "tabs",selected = "Contruct a Chart")
  })
  
  #go to trend tab.
  observeEvent(input$maptablink, {
    
    updateTabsetPanel(session, "tabs",selected = "Populate a Map")
    
  })
  
  #go to bubble tab.
  observeEvent(input$tabletablink, {
    
    updateTabsetPanel(session, "tabs",selected = "Create a Table")
    
  })
  
  #### observe events for hb and pharmacy inputs####
 
  ###for the chart
  
  #making hscp choices dependant on board.
  observeEvent(input$board, {
    updateSelectInput(session,
                      "hscp",
                      choices = ContractorActivity$HSCP[ContractorActivity$Healthboard %in% 
                                                          input$board])
    
  })
  
  
  
  #making pharmacy choices dependant on hscp.
  observeEvent(input$hscp,{
    updateSelectizeInput(session,
                         "Pharmacy",
                         choices = ContractorActivity$Pharmacy[ContractorActivity$HSCP %in% 
                                                                 input$hscp])
  })
  
  
  
  
  
  
  
  
  ##for the table
  #making hscp choices dependant on board.
  observeEvent(input$board2,{
    updateSelectizeInput(session,
                         "hscp2",
                         choices = ContractorActivity$HSCP[ContractorActivity$Healthboard %in% 
                                                             input$board2])
  })
  
  
  
  #making pharmacy choices dependant on hscp.
  observeEvent(input$hscp2,{
    updateSelectizeInput(session,
                         "Pharmacy2",
                         choices = ContractorActivity$Pharmacy[ContractorActivity$HSCP %in% 
                                                                 input$hscp2])
    
  })
  

  
                             ####   line chart ####
  

  
  #create a reactive plot.
  plot <- reactive({  
    
    # set up Date for slider.
    AllPharmacies <- AllPharmacies()[AllPharmacies()$Date >= input$Dates[1] & AllPharmacies()$Date <= input$Dates[2] ,]
    

    
    #rename datatable coloum names with spaces and £ sign for cost.
    names <- ifelse(input$y == "Cost", "Cost (£)",
           ifelse(input$y == "MASItems","MAS Items", 
           ifelse(input$y == "MASCapitationPayment","MAS Capitation Payment (£)",
           ifelse(input$y =="CMSItems","CMS Items",
           ifelse(input$y == "CMSCapitationPayment","CMS Capitation Payment (£)",
           ifelse(input$y == "EHCItems","EHC Items",
           ifelse(input$y =="SmokingCessationItems","Smoking Cessation Items",
           ifelse(input$y =="SmokingCessationPayment", "Smoking Cessation Payment (£)",
           ifelse(input$y =="InstalmentDispensings","Instalment Dispensings",
           ifelse(input$y =="MethadoneDispensingFeeNumber", "Methadone Dispensing Fee Number",
           ifelse(input$y =="MethadoneDispensingFeeRate","Methadone Dispensing Fee Rate",
           ifelse(input$y =="FinalPayments" , "Final Payments",
           ifelse(input$y =="SupervisionDispensingFeeNumber", "Supervision Dispensing Fee Number",
           ifelse(input$y =="SupervisionFeeRate","Supervision Fee Rate",
           ifelse(input$y =="MASRegistrations","MAS Registrations",
           ifelse(input$y =="CMSRegistrations","CMS Registrations",
            "Items"))))))))))))))))
    
    #creating a ggplot for the line chart.
    p <- ggplot() +
      geom_line(data=AllPharmacies, aes(x=Date, y=get(input$y),
                                               group=Pharmacy, 
                                               colour=str_wrap(Pharmacy,20), 
                                        text = paste("Pharmacy",":",str_wrap(Pharmacy,30),
                                                     '<br>',
                                                     "Date",":",as.yearmon(Date),
                                                     '<br>',
                                                     input$y,":", get(input$y))),size=0.7) +   
      #geom_point(size=1) +
      theme_bw() +
      #theme(axis.text.x = element_text(size=8, angle = 90,hjust=0)) +
      ggtitle(paste0(
        #"Total"," ",
        names,
        ",", " ",
        as.yearmon(input$Dates[1])," ","to"," ",as.yearmon(input$Dates[2]))) + 
      theme(axis.line = element_line(colour = "black"), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            plot.title = element_text(hjust = 0.5),
            legend.key.size = unit(1.7, "cm"),
            legend.title=element_blank()) +
      labs(y = paste(names(lab_choices)[lab_choices == input$y], "")) +


      scale_y_continuous(labels=comma) +
      scale_x_date(date_labels =  "%b %Y") 
   #   scale_colour_manual(values = c("#000066","#0066CC","#3399CC","#66CCFF", "#99CCFF"))
     # scale_colour_manual(c("#000066","#08519c","#3182bd","#6baed6","#aac1cf"))
  
    #  scale_colour_brewer(palette=input$c)
      if(input$c=="Blues") {
        p <- p +  scale_colour_manual(values = Blues) }
    else {
      p <- p +  scale_colour_manual(values = Colours) 
      
    }
    
    
    #ifelse(input$c=="Blues",Blues,Colours))


   p
  })
  

  
#render plot
  output$pharmacyline <- renderPlotly({
   
    
    # convert created ggplot into ggplotly.
    p <- ggplotly(plot(), tooltip = c("text")) %>% 
      
      layout(#autosize=F,
             #width = 1320, height = 650,
        width = (0.95*as.numeric(input$dimension[1])), 
        height = as.numeric(input$dimension[2]),
             # orient the legend
             #legend = list(font = list(size = 9),orientation = "v", xanchor="auto", x = 0.5,yanchor="auto", y =-0.3), 
             legend = list(font = list(size = 9),orientation = "v", x = 1, y=0.9), #1.1
             margin=list(l=20, r=150,b=100,t=100,pad=5)) %>% 
      config(displayModeBar = FALSE) #,
    
    #   displaylogo = FALSE, #choose what to display in plotly toolbar.
    #   collaborate = FALSE,
    #   editable = FALSE,
    #   
    #   modeBarButtonsToRemove = list(
    #     "sendDataToCloud",
    #     "zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", 
    #     "zoomOut2d", "autoScale2d", "resetScale2d",
    #     "hoverClosestCartesian", "hoverCompareCartesian", 
    #     "zoom3d","pan3d", "orbitRotation", "tableRotation", 
    #     "handleDrag3d", "resetCameraDefault3d", "resetCameraLastSave3d",
    #     "hoverClosest3d", "zoomOutGeo", "resetGeo", "hoverClosestGeo",
    #     "hoverClosestGl2d", "hoverClosestPie", "toggleHover", "resetViews",
    #     "toggleSpikelines")) 
    
    
    p <- style(p,hoverlabel = list(font=list(size=12))) #hover text size
    
    p
  

  
   
  })
  
#download plot.
  
  output$Plot <- downloadHandler(
    
    filename =  "plot.png",
    content = function(file) {
      
      ggsave(file,device = "png",width=15, height=8.5)
      
    }
  )   
  

#                             ####    Map ####
  

  
  output$map <- renderLeaflet({  
 

    #Creating base map.  
    leaflet(options = leafletOptions(minZoom = 6)) %>%   
      addProviderTiles("OpenStreetMap.BlackAndWhite")%>% 
     # addFullscreenControl(position = "topleft", pseudoFullscreen = TRUE) %>%
      setView(lng = -3.2 , lat = 57.8, zoom = 7) %>%
    #  zoomControlPosition('topright') %>%
      #add static legend.
      
      addLegend("topleft", #values=colorData, #title=colorBy,  
                
                # 5 bins   
                
                #colors = c( "#000066","#99CCFF","#FFFF99","#FF9933","#FF0000"), 
                
                #labels = c("High","","","","Low"))  
                
                                  
                # 10 bins 
                
                # red= high
            #    colors = c("#660000",
             #              "#FF0000",
              #             "#FF6600",
               #            "#FF9933",
                #           "#FFFF99",
                 #          "#CCFFFF",
                  #         "#66CCFF",
                   #        "#3399CC",
                    #       "#0066CC",
                     #      "#000066"),
                
              
            # proper colour pallete for red/blue.
        #     colors = c("#993404",
        #     "#d95f0e",
        #     "#fe9929",
        #     "#fed98e",
        #     "#ffffd4",
        #     "#bdd7e7",
        #     "#6baed6",
         #    "#3182bd",
        #     "#08519c",
        #     "#000066"),
            
            #blues only.
            colors =      
          # c("#08306b","#08519c","#2171b5","#4292c6","#6baed6","#9ecae1","#c6dbef","#deebf7","#f7fbff"),
          
          c("#7f2704","#a63603","#d94801","#f16913","#fd8d3c",
            "#fdae6b","#fdd0a2","#fee6ce","#fff5eb"),
        
        #  c("#800026","#bd0026","#e31a1c","#fc4e2a",
         #   "#fd8d3c","#feb24c","#fed976","#ffeda0","#ffffcc"),
   
             
             #   labels = c("High","","","","","","","","","Low"),
        labels = c("High","","","","","","","","Low"),
                opacity = 0.8)  
  })
  
  #using observe to prevent map from zooming out.
  
  observe({
  
    
   ContractorActivity <- ContractorActivityM %>%
    filter(dategroup %in% input$Dates3) 

      #Set up colour variable.  
       colorBy <- input$y2

        # Set up the colourscheme.  
            
          colorData <-    ContractorActivity[[colorBy]]
         
         
        # pal <- 
         #  colorFactor(palette="plasma", colorData, reverse=TRUE, n = 10)  

           pal <- 
             colorFactor(palette=    #c("#08306b","#08519c","#2171b5","#4292c6","#6baed6","#9ecae1","#c6dbef","#deebf7","#f7fbff"),
                           c("#7f2704","#a63603","#d94801","#f16913","#fd8d3c",
                             "#fdae6b","#fdd0a2","#fee6ce","#fff5eb"),
                           
                            #   c("#800026","#bd0026","#e31a1c","#fc4e2a",
                         #    "#fd8d3c","#feb24c","#fed976","#ffeda0","#ffffcc"),
                         colorData, reverse = TRUE, n = 9) 
           
         # Prepare text for tooltip:  
            
          #option 1
          
          # mytext=paste("<b>","Pharmacy: ",ContractorActivity$Pharmacy, "<br/>",  colorBy,": ", ContractorActivity[[colorBy]], sep="") %>%  
          # lapply(htmltools::HTML)  
          
          #option 2
          
          mytext=paste("<b>","Pharmacy ","</b>", "<br/>", ContractorActivity$Pharmacy,"<br/>", "<b>","Time period ", "</b>", "<br/>",  input$Dates3, "<br/>","<b>", paste(names(lab_choices)[lab_choices == input$y2], ""), "</b>","<br/>",ContractorActivity[[colorBy]], sep="") %>%
            lapply(htmltools::HTML)
          
      
          #proxy map.
          leafletProxy("map",data= ContractorActivity) %>%
            clearMarkers() %>%  
            addCircleMarkers(~Longitude, ~Latitude, radius=7,   
                             stroke=TRUE, fillOpacity=2.0, fillColor=~pal(colorData),
                             color = "black",weight=1,opacity=0.7,
                             label = mytext,  
                             labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "11px", direction = "auto")) 
         
       
           
          })  
  

  

  
#                           ####            Data table ####


  
  # Print data table.
  output$pharmacytable <-  DT::renderDataTable(server = FALSE,{
    
    

    #rename datatable coloum names with spaces and £ sign for cost.
    col <- ifelse(input$x == "Cost", "Cost (£)",
           ifelse(input$x == "MASItems","MAS Items", 
           ifelse(input$x == "MASCapitationPayment","MAS Capitation Payment (£)",
           ifelse(input$x =="CMSItems","CMS Items",
           ifelse(input$x == "CMSCapitationPayment","CMS Capitation Payment (£)",
           ifelse(input$x == "EHCItems","EHC Items",
           ifelse(input$x =="SmokingCessationItems","Smoking Cessation Items",
           ifelse(input$x =="SmokingCessationPayment", "Smoking Cessation Payment (£)",
           ifelse(input$x =="InstalmentDispensings","Instalment Dispensings",
           ifelse(input$x =="MethadoneDispensingFeeNumber", "Methadone Dispensing Fee Number",
           ifelse(input$x =="MethadoneDispensingFeeRate","Methadone Dispensing Fee Rate",
           ifelse(input$x =="FinalPayments" , "Final Payments",
           ifelse(input$x =="SupervisionDispensingFeeNumber", "Supervision Dispensing Fee Number",
           ifelse(input$x =="SupervisionFeeRate","Supervision Fee Rate",
           ifelse(input$x =="MASRegistrations","MAS Registrations",
           ifelse(input$x =="CMSRegistrations","CMS Registrations",
                  "Items"))))))))))))))))
    
    

          #data table.
          DT::datatable(AllPharmacies2(),
                        colnames= c("Date","Pharmacy",col),
                        filter = 'top',
                        class = 'cell-border stripe',
                  
                         extensions = c('Buttons'),
                         options = list(
                           #initComplete = JS( # gives black header
                            #"function(settings, json) {",
                             #"$(this.api().table().header()).css({'background-color': '#FFF', 'color': '#000'});",
                             #"}"),
                           
                      
                           pageLength = 20,
                          
                           
                           dom = 'Bfrtip',
                           buttons = c('copy', 'print')), #"csv".
                           rownames = FALSE) %>%
            formatRound(input$x,0)
                         
           
    
      
  })
  


#  option to downalod the data
  output$downloaddata <- downloadHandler(
    filename = function() {
      paste("Data", "csv", sep=".")
    },
    content = function(file) {
      write.csv(AllPharmacies2(), file)
    }
  )
  


  
      
  
}
)
  