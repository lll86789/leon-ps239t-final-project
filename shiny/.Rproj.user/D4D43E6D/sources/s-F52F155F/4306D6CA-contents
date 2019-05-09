navbarPage("Population of Taiwan", id="tw",
           theme = "readable.css",
           tabPanel("File Upload",
                    mainPanel(
                      fluidRow(
                        h1("Upload your Data"),
                        column(4, 
                      fileInput("file1", "Choose CSV File",
                                multiple = TRUE,
                                accept = c("text/csv",
                                           "text/comma-separated-values,text/plain",
                                           ".csv"))
                      )
                      ), 
                      fluidRow(
                        h1("Train the Model"),
                        column(4, 
                        selectInput("Datatype_model", 
                                    label = "Dataset", 
                                    choices = c("Select a dataset to train" = "", names(mappinglist)), 
                                    multiple = TRUE)
                        )
                      ), 
                      fluidRow(
                        column(4, 
                               selectInput("model", 
                                           label = "Model Type",
                                           choices = list("Auto ARIMA", 
                                                          "Bootstrap"),
                                           selected = "Auto ARIMA")
                        ), 
                        column(2, 
                               numericInput("trainyear", label = "Length of years", min = 1, value = 5)
                               )
                      ), 
                      fluidRow(
                        column(4, 
                        actionButton("modeltrain", "Train",
                                     style="padding:5px; opacity: .80; color: #fff; background-color: #4169E1; border-color: #4169E1")
                        )
                      )
                    )
                    
                    
           ),
           tabPanel("Interactive Map",
                    tags$head(
                      tags$style(HTML(".leaflet-container { background: white; }"))
                    ),
                    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                    tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #4169E1}")),
                    leafletOutput("mymap", width = "1600", height = "800"),
                    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,style="z-index:500;",
                                  draggable = TRUE, top = 70, left = "auto", right = 20, bottom = "auto",
                                  width = 330, height = "auto",
                                  
                                  selectInput("Datatype", 
                                              label = "Datatype",
                                              choices = list("Population", 
                                                             "Unemployment Rate",
                                                             "Disposable Income"),
                                              selected = "Population"),
                                  sliderInput("year", 
                                              label = "Year",
                                              min = 1974, max = 2038, value = c(2017), sep = ""),
                                  conditionalPanel(condition = "input.Datatype == 'Population'", selectInput("sex", 
                                                                                                             label = "Sex",
                                                                                                             choices = list("Total", 
                                                                                                                            "Female",
                                                                                                                            "Male"),
                                                                                                             selected = "Total")),
                                  selectInput("colors", "Color Scheme",
                                              rownames(subset(brewer.pal.info, category %in% c("seq", "div"))),
                                              selected = "Blues"
                                  ),
                                  checkboxInput("legend", "Show legend", TRUE), 
                                  actionButton('myplots', 'Show/Close Attributes', "data-toggle"='collapse', "data-target"='#demo',
                                               style="padding:5px; opacity: .80; color: #fff; background-color: #4169E1; border-color: #4169E1")
                    ), 
                    
                    absolutePanel(id = "controls1", class = "panel panel-default", fixed = TRUE,style="z-index:500;",
                                  draggable = TRUE, top = 60, left = 60, right = "auto", bottom = "auto",
                                  width = 510, height = "auto",
                                  tags$div(id = 'demo',  class="collapse",
                                           tags$div(textOutput("greywarn"), style = "color:grey"),
                                           conditionalPanel(condition = "input.mymap_shape_click", 
                                                            h3(textOutput("countyname")),
                                                            h4(textOutput("county_num")), 
                                                            h5("Population Time Series"), 
                                                            plotOutput("timeseries", height = 300, width = 500), 
                                                            tags$div(textOutput("redwarn"), style = "color:red"), 
                                                            h5("Population Pyramid"), 
                                                            plotOutput("poppyramid", height = 400, width = 500)
                                                            )
                                           )
                                  
                    )
           ),
           
           
           
           tabPanel("Data Exploration",
                    fluidRow(
                      column(3,
                             selectInput("Datatype2", "Data type", c("Select One"="", c("Population", 
                                                                                        "Unemployment Rate",
                                                                                        "Disposable Income")), multiple=FALSE)
                      ),
                      column(3,
                             conditionalPanel("input.Datatype2",
                                              selectInput("county2", "County", c("Select Counties"="", Countylist$COUNTYMIX), multiple=TRUE)
                             )
                      ),
                      column(3,
                             conditionalPanel("input.Datatype2 == 'Population'",
                                              selectInput("sex2", "Sex", c("Select a Sex"="", c("Total", 
                                                                                                "Female",
                                                                                                "Male")), multiple=FALSE)
                             )
                      ),
                      column(3,
                             conditionalPanel("input.Datatype2 == 'Population'",
                                              selectInput("age2", "Age", c("Select Ages"="", unique(formalpop$Age)), multiple=TRUE)
                             )
                      )
                    ),
                    fluidRow(
                      column(1,
                             numericInput("minyear", "Year (From)", min=1974, max=2018, value=1974)
                      ),
                      column(1,
                             numericInput("maxyear", "Year (To)", min=1974, max=2018, value=2038)
                      )
                    ),
                    hr(),
                    DT::dataTableOutput("thetable")
                    
           )
)
