navbarPage("Population of Taiwan", id="tw",#make tab page
           theme = "readable.css",#import the theme
           tabPanel("File Upload",#first tab
                    mainPanel(
                      fluidRow(
                        h1("Upload your Data"),
                        column(4, 
                      fileInput("file1", "Choose CSV File",
                                multiple = TRUE,
                                accept = c("text/csv",
                                           "text/comma-separated-values,text/plain",
                                           ".csv"))#input csv file
                      )
                      ), 
                      fluidRow(
                        h1("Train the Model"),
                        column(4, 
                        selectInput("Datatype_model", 
                                    label = "Dataset", 
                                    choices = c("Select a dataset to train" = "", names(mappinglist)), 
                                    multiple = TRUE)#select datasets to model
                        )
                      ), 
                      fluidRow(
                        conditionalPanel(condition = "input.Datatype_model", 
                        column(4, 
                               selectInput("model", 
                                           label = "Model Type",
                                           choices = list("Auto ARIMA", 
                                                          "Bootstrap"),
                                           selected = "Auto ARIMA")#select a model
                        ), 
                        column(2, 
                               numericInput("trainyear", label = "Length of years", min = 1, value = 5)
                               )#select a year length
                      )), 
                      fluidRow(
                        conditionalPanel(condition = "input.Datatype_model", 
                        column(4, 
                        actionButton("modeltrain", "Train",
                                     style="padding:5px; opacity: .80; color: #fff; background-color: #4169E1; border-color: #4169E1")
                        ))#train model button
                      ), 
                      fluidRow(
                        h2(textOutput("message"), style = "color: red")#show "model trained" after trained the model
                      )
                    )
                    
                    
           ),
           tabPanel("Interactive Map",#second page
                    tags$head(
                      tags$style(HTML(".leaflet-container { background: white; }"))#set background to white
                    ),
                    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                    tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #4169E1}")),
                    leafletOutput("mymap", width = "1600", height = "800"),#output leaflet map
                    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,style="z-index:500;",
                                  draggable = TRUE, top = 70, left = "auto", right = 20, bottom = "auto",
                                  width = 330, height = "auto",#panel settings
                                  
                                  selectInput("Datatype", 
                                              label = "Datatype",
                                              choices = list("Population", 
                                                             "Unemployment Rate",
                                                             "Disposable Income"),
                                              selected = "Population"),#select dataset to plot
                                  sliderInput("year", 
                                              label = "Year",
                                              min = 1974, max = 2038, value = c(2017), sep = ""),#select year
                                  conditionalPanel(condition = "input.Datatype == 'Population'", selectInput("sex", 
                                                                                                             label = "Sex",
                                                                                                             choices = list("Total", 
                                                                                                                            "Female",
                                                                                                                            "Male"),
                                                                                                             selected = "Total")),#select sex
                                  selectInput("colors", "Color Scheme",
                                              rownames(subset(brewer.pal.info, category %in% c("seq", "div"))),
                                              selected = "Blues"
                                  ),#select a scheme to plot
                                  checkboxInput("legend", "Show legend", TRUE), #show legend or not
                                  actionButton('myplots', 'Show/Close Attributes', "data-toggle"='collapse', "data-target"='#demo',
                                               style="padding:5px; opacity: .80; color: #fff; background-color: #4169E1; border-color: #4169E1")#show the panel or not
                    ), 
                    
                    absolutePanel(id = "controls1", class = "panel panel-default", fixed = TRUE,style="z-index:500;",
                                  draggable = TRUE, top = 60, left = 60, right = "auto", bottom = "auto",
                                  width = 510, height = "auto",#attribute panel
                                  tags$div(id = 'demo',  class="collapse",
                                           tags$div(textOutput("greywarn"), style = "color:grey"),
                                           conditionalPanel(condition = "input.mymap_shape_click", 
                                                            h3(textOutput("countyname")),
                                                            h4(textOutput("county_num")), 
                                                            h5("Population Time Series"), 
                                                            plotOutput("timeseries", height = 300, width = 500), #time series plot
                                                            tags$div(textOutput("redwarn"), style = "color:red"), 
                                                            h5("Population Pyramid"), 
                                                            plotOutput("poppyramid", height = 400, width = 500)#population pyramid
                                                            )
                                           )
                                  
                    )
           ),
           
           
           
           tabPanel("Data Exploration",#Third page
                    fluidRow(
                      column(3,
                             selectInput("Datatype2", "Data type", c("Select One"="", c("Population", 
                                                                                        "Unemployment Rate",
                                                                                        "Disposable Income")), multiple=FALSE)#select dataset
                      ),
                      column(3,
                             conditionalPanel("input.Datatype2",
                                              selectInput("county2", "County", c("Select Counties"="", Countylist$COUNTYMIX), multiple=TRUE)#select county
                             )
                      ),
                      column(3,
                             conditionalPanel("input.Datatype2 == 'Population'",
                                              selectInput("sex2", "Sex", c("Select a Sex"="", c("Total", 
                                                                                                "Female",
                                                                                                "Male")), multiple=FALSE)#select sex
                             )
                      ),
                      column(3,
                             conditionalPanel("input.Datatype2 == 'Population'",
                                              selectInput("age2", "Age", c("Select Ages"="", unique(formalpop$Age)), multiple=TRUE)
                             )#select age
                      )
                    ),
                    fluidRow(
                      column(1,
                             numericInput("minyear", "Year (From)", min=1974, max=2018, value=1974)#select yyear
                      ),
                      column(1,
                             numericInput("maxyear", "Year (To)", min=1974, max=2018, value=2038)
                      )
                    ),
                    hr(),
                    DT::dataTableOutput("thetable")#output the table of data
                    
           )
)
