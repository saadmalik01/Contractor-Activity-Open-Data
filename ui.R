
##ui.R## 

shinyUI(fluidPage(theme = shinytheme("lumen"), 
                  shinyjs::useShinyjs(),
                  introjsUI(),
                  #Prevent red text error messages from appearing
                  #throughout the app
                 
      #### styling ####
      
                  tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                  
                   tags$style(
                   type = "text/css",
                    ".shiny-output-error { visibility: hidden; }", 
                 ".shiny-output-error:before { visibility: hidden; }"
                  ), 
                  
                 
                 tags$head(
                   tags$style(HTML("
                                   #introhelp  {
                                   display:block;
                                   height: 25px;
                                   width: 25px;
                                   
                                   
                                   padding:3px; 
                                   font-size:75%;
                                   text-align:center;
                                   position: absolute;
                                   
                                   
                                   
                                   border-radius: 50%;
                                   border: 1px solid grey;
                                   
                                   background-color: #376092!important;
                                   color:white
                                   
                                   }
                                   
                                   "))
                   ),
                 
                 tags$head(
                   tags$style(HTML("
                                   #help  {
                                   display:block;
                                   height: 25px;
                                   width: 25px;
                                   
                                   
                                   padding:3px; 
                                   font-size:75%;
                                   text-align:center;
                                   position: absolute;
                                   
                                   
                                   
                                   border-radius: 50%;
                                   border: 1px solid grey;
                                   
                                   background-color: #376092!important;
                                   color:white
                                   
                                   }
                                   
                                   "))
                   ),
                 
                 tags$head(
                   tags$style(HTML("
                                   #help2  {
                                   display:block;
                                   height: 25px;
                                   width: 25px;
                                   
                                   
                                   padding:3px; 
                                   font-size:75%;
                                   text-align:center;
                                   position: absolute;
                                   
                                   
                                   
                                   border-radius: 50%;
                                   border: 1px solid grey;
                                   
                                   background-color: #376092!important;
                                   color:white
                                   
                                   }
                                   
                                   "))
          ),
          tags$head(
            tags$style(HTML("
                            #help3  {
                            display:block;
                            height: 25px;
                            width: 25px;
                            
                            
                            padding:3px; 
                            font-size:75%;
                            text-align:center;
                            position: absolute;
                            
                            
                            
                            border-radius: 50%;
                            border: 1px solid grey;
                            
                            background-color: #376092!important;
                            color:white
                            
                            }
                            
                            "))
            ),
               #limit select input expansion.
               tags$style(mycss),
               tags$style(mycss2),

              #colour remains stable.
              # tags$style(type="text/css", "#downloaddata {background-color:#0072B2;color: #FFFFFF;
               #             border-style: solid;border-color:#0066a0}"),
              
              #change colour of download data button.
              tags$head(tags$style(".blue{background-color:#0072B2;} .blue{color: #FFFFFF;}
                                   .blue{border-style: solid;}.blue{border-color: #0066a0;}")),
               
               #change colour of download chart button.
               tags$head(tags$style(".blue2{background-color:#0072B2;} .blue2{color: #FFFFFF;}
                                   .blue2{border-style: solid;}.blue2{border-color: #0066a0;}")),
              

                #to make plotly chart size adjustable.
                  tags$head(tags$script('
                        var dimension = [0, 0];
                                    $(document).on("shiny:connected", function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                    });
                                    $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                    });
                                    ')),
              #change colour of picker input.
              #colour remains stable.
              tags$style("
               #x ~ .choicesOpt  {background-color:#FFFFFF;color: #000000;
                           border-style: solid;border-color:#0066a0}"),
          
          tags$head( #CSS styles
            
            #Style sidebars/well panels
            tags$style(".well {background-color:#ffffff; border: 0px solid #336699;
                       padding: 5px; box-shadow: none; }",
                       
                       #landing page boxes
                       ".landing-page-box {width:100%; height:100%; min-height:31vh; background-color:white;
                       border: 1px solid #AAAAAA; margin-bottom: 5px; float: left; transition: 0.5s ease; position: relative; object-fit: scale-down;}",
                       ".landing-page-box-about {width:100%; height:100%; min-height:10.7vh; background-color:white;
                       border: 1px solid #AAAAAA; margin-bottom: 5px; float: left; position: relative; object-fit: scale-down;}",
                       ".landing-page-box:hover, .landing-page-box-about:hover {-webkit-transform: scale(1.05); 
                       -ms-transform: scale(1.05); transform: scale(1.05); }", #hover effect on boxes
                       #landing page icons
                       ".landing-page-icon {width:100%; height:85%; min-height:12vh; background-color: white;
                       border: 0px ; position: absolute; object-fit: scale-down;}",
                       ".landing-page-about-icon {width:100%; height:65%; min-height:5vh; background-color: white;
                       border: 0px; position: absolute; object-fit: scale-down;}",
                       #landing-page titles for boxes
                       ".landing-page-box-title {font-size: 16px; text-align:center; color: darkblue;
                       font-weight: bold; background-color: none; width:100%; max-height: 20px; margin-top: 10px;
                       
                       }",
             #landing page titles for ABOUT boxes
             ".landing-page-box-about-title {font-size: 16px; text-align:center; color: darkblue;
             font-weight: bold; background-color: none; width:100%; max-height: 20px; margin-top: 5px; }",
             #landing page buttons
             ".landing-page-button {text-align:center;
             background-image:none; color: black; white-space: normal; border-radius: 0;border: 0px;
             font-size: 14px; min-height: 25vh; position: absolute; margin-bottom: 0px; margin-top: 5px; float: middle;width: 100%; opacity: 0;}",
             ".landing-page-button-about {text-align:center;
             background-image:none; color: black; white-space: normal; border-radius: 0; border:0px ;
             font-size: 14px; position: absolute; min-height: 7vh; margin-bottom: 0px; margin-top: 1px; float: middle; width: 100%; opacity:0;}",
             #hover effect on landing page buttons
             ".landing-page-button:hover , .landing-page-button:active , .landing-page-button-about:hover, .landing-page-button-about:active {opacity: 3; 
             background-color: #fff; /* fallback */
             background-color: rgba(255, 255, 255, 0.8);
             color: darkblue;
             border-color: #fff; /* fallback */
             border-color: rgba(255, 255, 255, 0.8); transition: background-color 0.3s ease-in,
             color 0.3s ease-in;}",
             #center image - for normal icons
             "img.center {object-fit: scale-down; position:absolute; width:100%; height:100%; margin-left:auto; margin-right: auto; display: block; padding:20px;}",
             #center image - for about icons
             "img.centerabout {object-fit: scale-down; position:absolute; width:100%; height:100%; margin-left:auto; margin-right: auto; display: block; padding:8px;}",
             #landing-page column 
             ".landing-page-column {padding-right:3vh}",
             
             #landing-page icons
             ".icon-lp{font-size: 1.3em; padding-right: 4px;}"),
            HTML("<base target='_blank'>")
          ),
          
              
                  #divinding into distict tabs.
                  tabsetPanel(
                    
                    id="tabs", 
                    
                    
                                                      #### Introduction ####
                    
                    
                   
                     tabPanel(
                      "Introduction",id="intro",icon = icon("info-circle"), #logo for introduction tab
                             
                             # writing for the landing page.
                     
                             column(2,
                                    
                                    h3("Contractor Activity Open Data")
                                    
                             ),
                      
                      column(8, 
                             
                             h4("What does this application show?"),
                                    
                                    p("This application contains information on all pharmacies in Scotland. 
                                      'Contruct a Chart' allows for the selection of up to five pharmacies and compare 
                                       pharmacy activity across a range of measures. 'Populate a Map' shows 
                                      activity for all pharmacies in Scotland by financial quarter. 'Create a table' allows for the selection 
                                      of mutliple pharmacies and mutiple measures. The table can be downloaded as a CSV."), 
                                    
                                    
                                    p("The application contains a number of measures. Please refer to the glossary 
                                      tab to find the meaning of each term."),

                             tags$style(HTML('#introhelp { margin-bottom: 30px}')),
                             splitLayout(cellWidths = c("4%", "96%"), 
                                         
                                         actionButton(inputId= "introhelp", 
                                                      icon("info")),     
                                         
                                         p("Use this button to find out 
                                           how to use a page.", id="r1")
                                         
                                         ),
                             
                                    p("If you have any trouble using this tool or you have
                          further questions relating to the data, please contact
                          us at: ",
                                      tags$b(tags$a(
                                        href = "mailto:nss.isdprescribing@nhs.net",
                                        "nss.isdprescribing@nhs.net")),
                                      " and we will be happy to help."),
                             
                             tags$div( 
                               HTML(paste("", tags$span(style="color:red", #used to color the writing as red
                                                        tags$strong("Beta version of app for user testing only"), sep = "")))
                               
                             ),
                             
                             h4("Explore the data:", id="one"),
                             
                             
                             br(),
                             
                             
                             column(4, 
                                    div(id='introbox1',
                                        class="landing-page-column",
                                        div(class="landing-page-box", 
                                            div("Construct a Chart", class = "landing-page-box-title"),
                                            div(class = "landing-page-icon", style="background-image: url(line.png);
                                                background-size: auto 80%; background-position: center; background-repeat: no-repeat; "),
                                            actionButton('trendtablink', 'See pharmacy trends across time.', 
                                                         class="landing-page-button", 
                                                         icon = icon("arrow-circle-right", "icon-lp"))))),  
                             
                             column(4,
                                    div(id='introbox2',
                                        class="landing-page-column",
                                        div(class="landing-page-box", 
                                            div("Populate a Map", class = "landing-page-box-title"),
                                            div(class = "landing-page-icon", style="background-image: url(map2.png);
                                                background-size: auto 80%; background-position: center; background-repeat: no-repeat; "),
                                            actionButton('maptablink', 'Observe pharmacy activity on a map.', 
                                                         class="landing-page-button", 
                                                         icon = icon("arrow-circle-right", "icon-lp"))))
                                    ),
                             
                             column(4, 
                                    div(id='introbox3',   
                                        class="landing-page-column",
                                        div(class="landing-page-box", 
                                            div("Create a table", class = "landing-page-box-title"),
                                            div(class = "landing-page-icon", style="background-image: url(table.png);
                                                background-size: auto 80%; background-position: center; background-repeat: no-repeat; "),
                                            actionButton('tabletablink', 'Get information on mutiple pharmacies and download the information as a csv.', 
                                                         class="landing-page-button", 
                                                         icon = icon("arrow-circle-right", "icon-lp"))))) 
                             
                             
                                    
                                    
                                    )),
                    
       
                      
                      
                    
                      
                  
                    
                                                 #### Line chart #### 
                      
                   
                     
                      
                    tabPanel("Contruct a Chart",id="line",icon = icon("line-chart"),
                             
                             column(12, 
                                    
                                    h3("Trend by month"),
                                    h5("
                                    The generated chart shows pharmacy activity on the selected measure across the chosen timeperiod.
                                    ")
                                   
                                    
                                    ),
                             column(12,
                             tags$style(HTML('#help { margin-bottom: 30px}')),
                             splitLayout(cellWidths = c("3%", "97%"), 
                                         
                                         actionButton(inputId= "help", 
                                                      icon("info")),     
                                         
                                         p("Use this button to find out 
                                           how to use this page.", id="r1")
                                         
                                         )),
                                    
                             
                       
                      #select pharmacy using select input.
                      column(4, 
                             div(id='board',
                             selectInput(inputId='board', 
                                         label='Filter by NHS Board:',
                                         choices= AllHBs)),
                             
                             
                          #   selectInput("board",
                           #              "Filter by Healthboard:",
                            #             choice = c('All Healthboards' = '', AllHBs), 
                             #            multiple = FALSE,
                              #           selected = NULL),
                                 
                             
                           
                          div(id='hscp',
                             selectInput(inputId='hscp', 
                                         label='Filter by Health and Social Care Partnership:',
                                         choices= c(AllHSCPs))
                             ),
                                         
                           #  selectInput("hscp",
                            #             "Filter by Health and Social Care Partnership:",
                             #             choice = c('All HSCPS' = '', AllHSCPs), 
                              #            multiple = FALSE,
                               #           selected = NULL),             
                      
                             
                          div(id='Pharmacy',
                             selectizeInput(inputId = "Pharmacy",
                                            label = "Select Pharmacy:",
                                            choices =  AllPharmacies, 
                                            selected = "ASDA STORES LIMITED-PHARMACY DEPARTMENT-G33 1AD",
                                            multiple=TRUE,
                                            options = list(maxItems = 5L, placeholder = 'Type name or postcode of pharmacy')))
                             #
                          
                         # selectInput(inputId='Pharmacy', 
                          #            label='Select Pharmacy:',
                           #         
                            #          choices= AllPharmacies,
                             #         selected = "BOOTS THE CHEMISTS LTD 62/70 KING STREET KILMARNOCK    KA1 1PA",
                                      
                                     
                       
               
                       
                      ),
                        
                      column(4, 
                             
                             #select measure for the plot.
                             
                          #   selectInput(inputId='y', 
                           #              label='Select Measure:',
                            #             c(Choose='', lab_choices),
                             #            selectize=TRUE),
                             
                          div(id='y',
                             selectInput(inputId='y', 
                                         label='Select Measure:',
                                         choices= lab_choices)),
                         
                             
                             #Date slider for chart.
                          div(id='Dates',
                             sliderInput("Dates",
                                         "Select time period:",
                                         min = min(ContractorActivity$Date),
                                         max = max(ContractorActivity$Date),
                                         value=c(min(ContractorActivity$Date),max(ContractorActivity$Date)),
                                         timeFormat="%b %Y"))
                            
                              
                             
                             
                             
                      ),
                      
                      column(4, 
                      
                             #option for colour
                             div(id='c',
                             selectInput(inputId='c', 
                                         label='Select colour scheme:',
                                         choices=c("Blues" = "Blues",
                                                   "Colours"= "Colours"),
                                         selected="Blues")),
                      
                      div(id='Plot',       
                      downloadButton(outputId = "Plot", label = "Download Chart",class="blue2"))
                      
                      ),
                      
                     
                     mainPanel(width = 12,
                               div(id='pharmacyline',           
                     plotlyOutput(outputId = "pharmacyline",width = "100%")))
                     
                    ),
                     
                      
                     
                      
              
                    
                                                      ####  Map ####
                    
                   
                    
                    tabPanel("Populate a Map",id="map",icon = icon("globe"),
                             
                             column(10, 
                                    
                                    h3("Map by quarter"),
                                    h5("The map shows pharmacy activity for the selected time period and measure.
                                       ")
                                    
                                    
                                    
                                    
                                    ),
                             
                           
                             
                             column(4,
                                    
                                    
                                    tags$style(HTML('#help2 { margin-bottom: 30px}')),
                                    splitLayout(cellWidths = c("8%", "92%"), 
                                                
                                                actionButton(inputId= "help2", 
                                                             icon("info")),     
                                                
                                                p("Use this button to find out 
                                                  how to use this page.", id="r1")
                                                
                                                )),
                             
                             column(4,
                                    
                             div(id='Dates3',
                                 selectInput(inputId = "Dates3",
                                             label= "Select time period:",
                                             choices=unique(AllDates), 
                                             
                                             selectize = TRUE))
                             ),
                             
                             column(4,
                             
                             div(id='y2',
                                 selectInput('y2', 'Select Measure:',
                                             c(`Select Measure`='', lab_choices2),
                                             selectize=TRUE))
                             ),
                             
    
                             mainPanel(width = 12,
                                 
                                       
                                       
                                       leafletOutput(outputId = "map")
                                                 #    , height = "700",width = "1400"),
                                       
                                    #   absolutePanel(top = 15, right=8, 
                                                     
                                                     #left = 20,   
                                                     #fixed= TRUE,
                                                     
                                                     #select Date
                                                   #  selectInput(inputId = "Dates3",
                                                    #             label = "Date:",
                                                     #            choices= c(`Select Date`='',AllDates),
                                                      #           selectize=TRUE,
                                                              #   selected = "2014 Jan-Mar",
                                                       #          multiple=FALSE),
                                                
                                                  #   selectInput(inputId = "y2",
                                                   #              label= "Measure",
                                                    #             choices=lab_choices, 
                                                                
                                                     #            selectize = TRUE)
                                                                 

                                                    
                                       
                                      
                                            
                                         #  pickerInput(
                                        #      inputId = "Dates3",
                                        #      label = "Select Date", 
                                        #      choices = AllDates)
                                            #  selected = "Jun 2018"
                                            
                                                     
                                                     
                           #  ))
                                                     
                                       
                                       
                    )),
          
                      
                                                   ####      Data table ####
                      
                     
                      
                      #Insturctions for the datatable.
                      
                     tabPanel("Create a Table",id="table",icon = icon("table"),
                          
                              column(10, 
                                     
                                     h3("Datatable"),
                                     h5("The generated data table shows pharmacy activity for mutiple measures for the selected time period.
                                        
                                       
          
                                  ")

                              ),
                              
                              column(12,
                                     tags$style(HTML('#help3 { margin-bottom: 30px}')),
                                     splitLayout(cellWidths = c("3%", "97%"), 
                                                 
                                                 actionButton(inputId= "help3", 
                                                              icon("info")),     
                                                 
                                                 p("Use this button to find out 
                                                          how to use this page.", id="r1")
                                                 
                                     )),

                      #Select measure(s) for datatable.
                
                        
                      column(4, 
                  
                        
                           #  selectInput("board2",
                            #             "Filter by Healthboard:",
                             #            choice = c('All Healthboards' = '', AllHBs), 
                              #           multiple = FALSE,
                               #          selected = NULL),
                             
                         
                           
                           div(id='board2',
                               selectInput(inputId='board2', 
                                         label='Filter by NHS Board:',
                                         choices= AllHBs)),
                             
                           
                           div(id='hscp2',
                                selectInput(inputId='hscp2', 
                                           label='Filter by Health and Social Care Partnership:',
                                           choices= AllHSCPs)),
                             
                         #    selectInput("hscp2",
                          #               "Filter by Health and Social Care Partnership:",
                           #              choice = c('All HSCPS' = '', AllHSCPs), 
                            #             multiple = FALSE,
                             #            selected = NULL),             
                             
                        
                        #select pharmacy using select input.
                        div(id='Pharmacy2',
                        selectizeInput(inputId = "Pharmacy2",
                                     label = "Select Pharmacy:",
                                       choices = AllPharmacies,
                                       # selected = "ASDA STORES LIMITED-PHARMACY DEPARTMENT-G33 1AD",
                                       multiple=TRUE,
                                       options = list(placeholder = 'Type name or postcode of pharmacy')))
                        
                        
                   #  shinyWidgets::pickerInput(
                    #     inputId = "Pharmacy2",
                     #     label = "Search Pharmacy", 
                      #    choices = AllPharmacies,
                     #
                      #    options = list(
                       #     `live-search` = TRUE,
                        #   `actions-box` = TRUE,
                         #   size = 10
                          # ),
                          
                        
                  
                          
                     #     multiple = TRUE
                          
                    # ))
                     
                   
                   
                      
                  #    selectInput('x', 
                  #                'Select Measure(s):', 
                  #                lab_choices, 
                  #                multiple=TRUE, 
                  #                selectize=TRUE)
                      
                      
                  
                  
                  #Date slider for datatable.
             
              
                      ),
              
                  
                  
                  column(4, 
                         
                         div(id='x',     
                    shinyWidgets::pickerInput(
                    
                    inputId = "x", 
                    label = "Select Measure(s):", 
                    choices =lab_choices,
                    # selected = "Items",
                    options = list(
                     
                      `actions-box` = TRUE, 
                      size = 10,
                      `selected-text-format` = "count > 3"
                     
                    ), 
                   choicesOpt = list(
                      style = rep(("color: black; background: white; "),21)),
                    multiple = TRUE
                  )),
                  
                  div(id='Dates2',  
                  sliderInput("Dates2",
                              "Select time period:",
                              min = min(ContractorActivity$Date),
                              max = max(ContractorActivity$Date),
                              value=c(min(ContractorActivity$Date),max(ContractorActivity$Date)),
                              timeFormat="%b %Y"))
                  
               
                  ),
                  
                  column(4, 
                         
                         #Data download option for datatable.
                         div(id='downloaddata',  
                         downloadButton("downloaddata", "Download data",class="blue")) 
                       
                  ),
                  
                  mainPanel(width = 12, 
                  
                  DT::dataTableOutput(outputId = "pharmacytable"))
                  
                     ),
                  
                  tabPanel(
                    "Glossary of terms",icon = icon("align-justify"), #logo for introduction tab
                    
                    # writing for the landing page.
                    
                    column(2,
                           
                           h3("Glossary")
                           
                    ),
                    
                    column(8, 
                           
      
                           
                           
                         #  p("Below is a glossary 
                          #   which helps explain the meaning of each measure."),
                          
                           tags$ul( 
                             tags$li(tags$b("Date"),":	The month and year in which the prescription item was processed."),
                             tags$li(tags$b("Items"),": The number of prescription items dispensed and for which the dispenser has been reimbursed."),
                             tags$li(tags$b("Cost (£)"),": Cost refers to the Gross Ingredient Cost.	The reimbursement cost for the paid quantity based upon the NHS basic price as listed in the Scottish drug tariff or manufacturer’s price list."),
                             tags$li(tags$b("MAS Items"),":	Number of items paid through Minor Ailments Service."),
                             tags$li(tags$b("MAS Registrations"),":	Actual number of patients registered for Minor Ailments Service"),
                             tags$li(tags$b("MAS Capitation Payment"),":	Capitation Payment for Minor Ailments Scheme. This is based on the number of patients registered for current month, plus any adjustment for the previous month."),
                             tags$li(tags$b("CMS Items"),":	Number of items paid through Chronic Medication Service."),
                             tags$li(tags$b("CMS Registrations"),":	Actual number of patients registered for Chronic Medication Service."),
                             tags$li(tags$b("CMS Capitation Payment"),":	Capitation Payment for providing Chronic Medication Service."), 
                             tags$li(tags$b("EHC Items"),":	Number of items paid through Emergency Hormonal Contraception service."),
                             tags$li(tags$b("Smoking Cessation Items"),":	Number of items paid through Smoking Cessations Service."),
                             tags$li(tags$b("Smoking Cessation Payment"),":	Payment for providing Smoking Cessations Service."),
                             tags$li(tags$b("Instalment Dispensings"),":	The number of instalment dispensings for which the dispenser has claimed for and has been reimbursed."),              
                             tags$li(tags$b("Methadone Dispensing Fee Number"),":	Number of methadone dispensing fees."),
                             tags$li(tags$b("Methadone Dispensing Fee Rate"),":	Methadone dispensing fee rate, where advised."),
                             tags$li(tags$b("Supervised Dispensing Fee Number"),":	Number of supervised dispensing fees."),
                             tags$li(tags$b("Supervision Fee Rate"),":	Rate of supervised dispensing fees, where advised."),
                             tags$li(tags$b("Final Payments"),":	Total payment made to the contractor."))
                           
                         
                           
                           
                           ))
                  
                  
                  
                  
                  
                  
                      
                  )
             )
       )

