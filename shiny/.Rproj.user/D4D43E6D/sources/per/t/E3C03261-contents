function(input, output, session) {
  
  uploadedData <- reactive({
    uploadedlist <- lapply(input$file1$datapath, FUN = function(x){
      df <- fread(x, stringsAsFactors = FALSE)
      names(df) <- c("Year", "COUNTYNAME", "Total_pop")
      df$COUNTYNAME <- as.character(df$COUNTYNAME)
      for(i in nl) df$COUNTYNAME[grepl(i, df$COUNTYNAME)] <- i
      return(df)
    })
    names(uploadedlist) <- substr(input$file1[[1]], 1, regexpr("\\.csv", input$file1[[1]]) - 1)
    mappinglist2 <- c(mappinglist, uploadedlist)
    return(mappinglist2)
  })
  
  observe({
    updateSelectInput(session, "Datatype", choices = names(uploadedData()))
    updateSelectInput(session, "Datatype_model", choices = c("Select a dataset to train" = "", names(uploadedData())))
  })
  modelData <- reactive({
    if(input$modeltrain == 0){
      return(uploadedData())
    }else {
      if (input$model == "Auto ARIMA"){
        trainedData <- lapply(names(uploadedData()), FUN = function(x){
          df <- uploadedData()[[x]]
          if (x %in% input$Datatype_model){
            if (x == "Population"){
              setDT(df)
              df <- df[order(COUNTYNAME, Sex, Year), ]
              Countynamelist <- unique(df$COUNTYNAME)
              Sexlist <- unique(df$Sex)
              trainedDf <- rbindlist(lapply(Countynamelist, FUN = function(y){
                sexDf <- data.table()
                for (i in Sexlist){
                  trainingData <- df[COUNTYNAME == y& Sex == i, ]
                  setDF(trainingData)
                  resultslist <- data.frame(lapply(names(trainingData)[4:21], FUN = function(k){
                    arimaModel <- auto.arima(trainingData[, k])
                    results <- round(as.numeric(forecast(arimaModel, input$trainyear)$mean), 0)
                    return(results)
                  }))
                  names(resultslist) <- names(trainingData)[4:21]
                  
                  resDf <- data.table(Year = c((max(df$Year) + 1):(max(df$Year) + input$trainyear)), 
                                      COUNTYNAME = rep(y, input$trainyear), 
                                      Sex = rep(i, input$trainyear))
                  resDf <- cbind(resDf, resultslist)
                  resDf$Total_pop <- rowSums(resultslist)
                  resDf <- rbind(df[COUNTYNAME == y& Sex == i, ], resDf)
                  sexDf <- rbind(sexDf, resDf)
                }
                setDT(sexDf)
                return(sexDf)
              }))
              df <- trainedDf
              
            }else {
              setDT(df)
              df <- df[order(COUNTYNAME, Year), ]
              Countynamelist <- unique(df$COUNTYNAME)
              trainedDf <- rbindlist(lapply(Countynamelist, FUN = function(y){
                arimaModel <-auto.arima(df[COUNTYNAME == y, ]$Total_pop)
                results <- as.numeric(forecast(arimaModel, input$trainyear)$mean)
                resDf <- data.table(Year = c((max(df$Year) + 1):(max(df$Year) + input$trainyear)), 
                                    COUNTYNAME = rep(y, input$trainyear), 
                                    Total_pop = results)
                resDf <- rbind(df[COUNTYNAME == y, ][, c(1:3)], resDf)
                return(resDf)
              }))
              df <- trainedDf
            }
          }
          return(df)
        })
        names(trainedData) <- names(uploadedData())
        return(trainedData)
        
      }else {
        return(uploadedData())
      }
      
    }
  })
  observeEvent(input$modeltrain, {
    output$message <- renderText({
      return("Model has trained!")
    })
  })
  
  
  filteredData <- reactive({
    if(input$Datatype == "Population"){
      County2 <- dplyr::left_join(x = County, y = modelData()[[input$Datatype]], by = "COUNTYNAME")
      return(County2[County2$Year == input$year & County2$Sex == input$sex, ])
    }else {
      County2 <- dplyr::left_join(x = County, y = modelData()[[input$Datatype]], by = "COUNTYNAME")
      return(County2[County2$Year == input$year, ])
    }
  })
  colorpal <- reactive({
    df1 <- as.data.frame(filteredData())
    colorNumeric(input$colors, df1$Total_pop)
  })
  output$mymap <- renderLeaflet({
    leaflet(Bound) %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
    
  })
  observe({
    pal <- colorpal()
    
    leafletProxy("mymap", data = filteredData()) %>%
      clearShapes() %>% 
      addPolygons(smoothFactor = 0.2, weight = 1, color = "grey30", fillOpacity = 1, 
                  fillColor = ~pal(Total_pop), layerId = ~COUNTYNAME) %>%
      addLabelOnlyMarkers(data = centers,
                          lng = ~x, lat = ~y, label = ~CountyName,
                          labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE))
    
  })
  
  observe({
    proxy <- leafletProxy("mymap", data = filteredData())
    
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~Total_pop, 
                          title = input$Datatype
      )
    }
  })
  
  
  output$timeseries <- renderPlot({
    event <- input$mymap_shape_click
    if (is.null(event)) {
      return(NULL)
    }else{
      plotdata <- modelData()[["Population"]]
      plotdata <- plotdata[COUNTYNAME == event$id & Sex == input$sex, ]
      p <- ggplot() + geom_line(data = plotdata, aes(x = Year, y = Total_pop), stat = "identity") + 
        geom_point(data = plotdata, aes(x = Year, y = Total_pop)) + theme_bw() + 
        labs(y = "Population") + geom_vline(xintercept = 2018, color = "red") + 
        theme(
          panel.background = element_rect(fill = "transparent")
          , plot.background = element_rect(fill = "transparent", color = NA)
          , panel.grid.major = element_blank()
          , panel.grid.minor = element_blank()
          , legend.background = element_rect(fill = "transparent")
          , legend.box.background = element_rect(fill = "transparent")
        )
      return(p)
    }
    
  })
  output$poppyramid <- renderPlot({
    event <- input$mymap_shape_click
    if (is.null(event)) {
      return(NULL)
    }else{
      plotdata <- modelData()[["Population"]]
      plotdata <- plotdata[COUNTYNAME == event$id & Sex != "Total" & Year == input$year, ]
      plotdata$Total_pop <- NULL
      setDF(plotdata)
      plotdata <- melt(plotdata, id = 1:3)
      plotdata$variable <- as.character(plotdata$variable)
      plotdata$variable <- gsub("c", "", plotdata$variable)
      plotdata$variable <- gsub("to", "-", plotdata$variable)
      plotdata$variable <- gsub("85", "85+", plotdata$variable)
      plotdata$variable <- factor(plotdata$variable, levels = unique(plotdata$variable))
      n1 <- ggplot(data = plotdata, 
                   mapping = aes(x = variable, fill = Sex, 
                                 y = ifelse(test = Sex == "Male", 
                                            yes = -value, no = value))) +
        geom_bar(stat = "identity") +
        scale_y_continuous(labels = abs, limits = max(plotdata$value) * c(-1,1)) +
        labs(x = "Age", y = "Population") +
        coord_flip() + 
        scale_fill_brewer(palette = "Set1") + 
        theme_bw() + 
        theme(
          panel.background = element_rect(fill = "transparent")
          , plot.background = element_rect(fill = "transparent", color = NA)
          , panel.grid.major = element_blank()
          , panel.grid.minor = element_blank()
          , legend.background = element_rect(fill = "transparent")
          , legend.box.background = element_rect(fill = "transparent")
        )
      return(n1)
    }
    
    
  })
  output$countyname <- renderText({
    event <- input$mymap_shape_click
    if (is.null(event)) {
      return(NULL)
    }else{
      countyoutput <- Countylist[Countylist$COUNTYNAME == event$id, ]$COUNTYENG
      return(paste0(event$id, " (", countyoutput, ")"))
    }
  })
  output$county_num <- renderText({
    event <- input$mymap_shape_click
    if (is.null(event)) {
      return(NULL)
    }else{
      countyoutput_num <- as.data.frame(filteredData())
      if(input$Datatype == "Population"){
        countyoutput_num <- round(mean(countyoutput_num[countyoutput_num$COUNTYNAME == event$id & 
                                                          countyoutput_num$Year == input$year & 
                                                          countyoutput_num$Sex == input$sex, ]$Total_pop))
        
      }else{
        countyoutput_num <- round(mean(countyoutput_num[countyoutput_num$COUNTYNAME == event$id & 
                                                          countyoutput_num$Year == input$year, ]$Total_pop))
      }
      return(paste0(input$Datatype, " :", countyoutput_num))
    }
  })
  observeEvent(input$Datatype, {
    changeyear <- modelData()[[input$Datatype]]
    
    maxyear <- max(changeyear$Year)
    minyear <- min(changeyear$Year)
    updateSliderInput(session, "year", max = maxyear, min = minyear)
  })
  output$greywarn <- renderText({
    event <- input$mymap_shape_click
    if(is.null(event)){
      return("Click on a region to show the attributes.")
    }else{
      return(NULL)
    }
  })
  output$redwarn <- renderText({
    event <- input$mymap_shape_click
    if(is.null(event)){
      return(NULL)
    }else{
      return("* Red line means 2018.")
    }
  })
  
  output$thetable <- DT::renderDataTable({
    if(input$Datatype2 == "Population"){
      df <- formalpop 
      df <- left_join(x = df, y = Countylist, by = "COUNTYNAME")
      df$COUNTYNAME <- df$COUNTYMIX
      df <- df[, c(1:5)]
      df <- df %>%
        filter(
          Year >= input$minyear,
          Year <= input$maxyear,
          is.null(input$county2) | COUNTYNAME %in% input$county2,
          is.null(input$sex2) | Sex %in% input$sex2, 
          is.null(input$age2) | Age %in% input$age2
        ) 
      
    }else if(input$Datatype2 == ""){
      df <- formalpop 
      df <- left_join(x = df, y = Countylist, by = "COUNTYNAME")
      df$COUNTYNAME <- df$COUNTYMIX
      df <- df[, c(1:5)]
    }else {
      df <- modelData()[[input$Datatype2]]
      df <- left_join(x = df, y = Countylist, by = "COUNTYNAME")
      df$COUNTYNAME <- df$COUNTYMIX
      df <- df[, c(1:3)]
      names(df) <- c(names(df)[1:2], input$Datatype2)
      df <- df %>%
        filter(
          Year >= input$minyear,
          Year <= input$maxyear,
          is.null(input$county2) | COUNTYNAME %in% input$county2
        ) 
    }
    
    action <- DT::dataTableAjax(session, df)
    
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
    
    
  })
  
  observeEvent(input$Datatype2, {
    if (input$Datatype2 != ""){
      changeyear2 <- modelData()[[input$Datatype2]]
      
      maxyear2 <- max(changeyear2$Year)
      minyear2 <- min(changeyear2$Year)
      updateNumericInput(session, "minyear", max = maxyear2, min = minyear2, value = minyear2)
      updateNumericInput(session, "maxyear", max = maxyear2, min = minyear2, value = maxyear2)
    }
    
  })
  
}