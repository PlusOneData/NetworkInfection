# Define server logic for application that modifies network graphs
shinyServer(function(input, output, session) {
  
  n <- reactive({as.numeric(input$n)})
  ed <- reactive({n() * 4})
  
  prob.infect <- .1
  gmma <- 14
  
  covid_model <- reactive({
    if("faceCovering" %in% input$npi){
      faceCovering <- 0.9
    } else {
      faceCovering <- 1.0
    }
    
    if("eyeProtection" %in% input$npi){
      eyeProtection <- 0.9
    } else {
      eyeProtection <- 1.0
    }
    
    if("distancing" %in% input$npi){
      distancing <- 0.85
    } else {
      distancing <- 1.0
    }
    
    covid_ppe <- default_ppe(faceCovering = faceCovering, eyeProtection = eyeProtection, distancing = distancing, compliance = input$compliance)
    covid_di <- ppe_infect(init_num = input$init_num, rate = prob.infect)
    covid_dr <- default_recover(max_recovery_time = 20)
    covid_dt <- default_testing(testDelay = input$testDelay, testFrequency = input$testFrequency, falseNegRate = 0.03, falsePosRate = 0.001, propTested = input$propTested)
    covid_lv <- default_leave(leaveDuration = input$leaveDuration, max_recovery_time = 20)
    covid_vx <- default_vax(vaxEff = input$vaxEff, propVax = input$propVax, vaxRate = input$vaxRate)
    covid_ex <- external_infect(rate = (input$ext.infect/100000))
    covid_model <- infection_model(components = list(covid_ppe,covid_vx, covid_di, covid_dr,covid_dt, covid_lv,covid_ex ))
    covid_model
  })
  
  #################
  ## Initialize graph networks
  #################
  
  # Discovery Lab Contact Network
  #read in DL Graph
  
  dlContactMatrix <- readxl::read_xlsx("./Data/Discovery Lab Contact Network.xlsx")
  
  groupCols <- names(dlContactMatrix)[-1]
  
  dlContactMatrix <- dlContactMatrix %>% 
    mutate_at(vars(groupCols), as.integer)
  
  dlContactMatrix[is.na(dlContactMatrix)]<- 6
  
  ## edge list 
  
  ## origin not equal to destination 
  
  edgeList <- dlContactMatrix %>% 
    pivot_longer(cols = -Name) %>% 
    filter(value > 2) %>% 
    rename("Origin" = "Name") %>% 
    rename("Dest" = "name") %>% 
    select(-value)
  
  #get graph from edgelist 
  dlContactGraph <- igraph::graph_from_edgelist(el = as.matrix(edgeList),directed = F)
  
  dlContactGraph <- simplify(graph = dlContactGraph,remove.loops = T)
  
  dlGraphSize <- vcount(dlContactGraph)
  
  ## dl contact network
  set.seed(4321) 
  dl <- reactive({dlContactGraph %>% 
    covid_model()$init_model()})
  
  # Erdos-Renyi network: constant probability to connect nodes
  sample_gnm_rep <- repeatable(sample_gnm, seed = 4321)
  
  set.seed(4321)
  rn <- reactive({sample_gnm_rep(n(), ed(), directed = F) %>%
      covid_model()$init_model()})
  
  # Scale free network
  set.seed(4321)
  sfree <- reactive({sample_fitness_pl(n(), ed(), 2.2) %>%
      covid_model()$init_model()})
  
  # Small world network
  set.seed(4321)
  sw <-  reactive({sample_smallworld(1, n(), 4, .1) %>%
      covid_model()$init_model()})
  
  #################
  ## Progress models through time steps
  #################
  
  set.seed(4321)
  
  
  #createTimeline_rep <- repeatable(createTimeline, seed = 4321)
  
  
  test0 <- reactive({createTimeline(rn(), 60, covid_model())})
  test1 <- reactive({createTimeline(sfree(), 60, covid_model())})
  test2 <- reactive({createTimeline(sw(), 60, covid_model())})
  test3 <- reactive({createTimeline(dl(), 60, covid_model())})
  
  
  #################
  ## Create one function to generate all needed force networks
  #################
  
  make_fn <- function(test) {
    # Compute node degrees (#links) and use that to set node size:
    deg <- reactive({igraph::degree(test()[[input$day]])})
    
    # Convert to object suitable for networkD3
    test_graph <- reactive({
      # Ensure nodes are grouped by infection state which is represented by the color
      test_graph <- igraph_to_networkD3(test()[[input$day]], group = V(test()[[input$day]])$color)
      test_graph$nodes$size <- deg()*3
      test_graph
    })
    
    # Create custom JavaScript to view tooltips
    clickJS <- "
      d3.selectAll('.xtooltip').remove();
      d3.select('body').append('div')
        .attr('class', 'xtooltip')
        .style('position', 'absolute')
        .style('border', '1px solid #999')
        .style('border-radius', '3px')
        .style('padding', '5px')
        .style('opacity', '0.85')
        .style('background-color', '#fff')
        .style('box-shadow', '2px 2px 6px #888888')
        .html('<b>Node: </b>' + d.name + '<br>' + 
              '<b>Color: </b>' + d.group + '<br>' +
              '<b>InfProbReduction: </b>' + d.infProbReduction + '<br>' +
              '<b>Infected: </b>' + d.infected + '<br>' +
              '<b>Counter: </b>' + d.counter + '<br>' +
              '<b>Recovered: </b>' + d.recovered + '<br>' +
              '<b>Tested: </b>' + d.tested + '<br>' +
              '<b>TestCounter: </b>' + d.testCounter + '<br>' +
              '<b>Detected: </b>' + d.detected + '<br>' +
              '<b>OutbreakDay: </b>' + d.outbreakDay + '<br>' +
              '<b>Reported: </b>' + d.reported + '<br>' +
              '<b>Leave: </b>' + d.leave + '<br>' +
              '<b>LeaveCounter: </b>' + d.leaveCounter)
        .style('left', (d3.event.pageX) + 'px')
        .style('top', (d3.event.pageY - 28) + 'px');
      //d3.selectAll('.legend text').remove();
      //let test = ['Susceptible', 'Infected', 'Exposed', 'Recovered']
      //d3.select('svg').selectAll('.fnLegend').data(test).enter().append('rect').attr('x', (d, i) => i*15).attr('y', 10).attr('width', 10).attr('height', 10)
      //d3.select('svg').on('mouseover',function(){console.log('Hovering over SVG.'); d3.selectAll('.xtooltip').remove()})
      d3.select('svg').on('mouseover',function(){d3.selectAll('.xtooltip').remove()})
      "
    
    # Create the force network object, set the node size to be based on degree, and ensure the colors match
    fn <- forceNetwork(Links = test_graph()$links, Nodes = test_graph()$nodes, 
                       Source = 'source', Target = 'target',
                       NodeID = 'name', Nodesize = 'size',
                       radiusCalculation = JS(" Math.sqrt(d.nodesize)+5"),
                       # Changing the color displayed to yellow to make it easier to distinguish between red and orange
                       colourScale = JS('d3.scaleOrdinal().domain(["blue", "green", "yellow", "red"]).range(["#0000FF", "#008000", "#FFFF00", "#FF0000"])'),
                       Group = 'group', charge = -100, bounded = FALSE, zoom=TRUE, clickAction = clickJS)
    
    # Assign all vertex.attributes to the forceNetwork object
    names <- c("infProbReduction", "infected", "color", "counter", "recovered", "tested", "detected", "testCounter", "outbreakDay", "reported", "leave","leaveCounter")
    for(i in names){
      fn$x$nodes[[i]] <- vertex.attributes(test()[[input$day]])[[i]]
    }
    
    return(fn)
  }
  
  #################
  ## Output all three models as force networks
  #################
  
  output$randomForce <- renderForceNetwork({
    randomFN <- make_fn(test0)
  })
  
  output$scaleForce <- renderForceNetwork({
    scaleFN <- make_fn(test1)
  })
  
  output$smallForce <- renderForceNetwork({
    smallFN <- make_fn(test2)
  })
  
  output$dlForce <- renderForceNetwork({
    dlFN <- make_fn(test3)
  })
  
  addLeave <- function(test){
    stats <- getStats(test)
    df <- data.frame("time" = 1, "type" = "leave", "value" = 0, stringsAsFactors = FALSE)
    
    for (i in 2:60) {
      
      total <- 0
      
      for(j in vertex.attributes(test[[i]])$color){
        if(j == "yellow"){
          total = total + 1
        }
      }
      
      df = rbind(df,list(i,"leave",total))
    }
    
    stats = rbind(stats, df)
    stats[, c(1,3)] <- sapply(stats[, c(1,3)], as.integer)
    stats$percent <- percent(stats$value / n())
    return(stats)
    
  }
  
  output$curvePlot <- renderPlot({
    # Generate stat blocks of each network
    stats0 <- addLeave(test0())
    stats1 <- addLeave(test1())
    stats2 <- addLeave(test2())
    stats3 <- addLeave(test3())
    
    
    stats0$model <- 'Random'
    stats1$model <- 'Scale Free'
    stats2$model <- 'Small World'
    stats3$model <- 'Discovery Lab'
    
    # Plot stats
    ggplot(rbind(stats1, stats0, stats2, stats3)) +
      geom_line(aes(time, value, color = type), size = 1.5) +
      facet_wrap(~model) +
      theme_bw() +
      #labs(title = "SIR Distribution") +
      scale_color_brewer(palette = "Dark2")
  })
  
  
  # Wait for the update button to be clicked to compare parameters changing
  sirPlot <- eventReactive(input$update,{
    # Generate stat blocks of each network
    stats0 <- addLeave(test0())
    stats1 <- addLeave(test1())
    stats2 <- addLeave(test2())
    stats3 <- addLeave(test3())
    
    stats0$model <- 'Random'
    stats1$model <- 'Scale Free'
    stats2$model <- 'Small World'
    stats3$model <- 'Discovery Lab'
    
    cbPalette <- c("#CC6666","#FFE338", "#66CC99", "#9999CC")
    
    # Plot stats
    sirPlot <- ggplot(rbind(stats1, stats0, stats2,stats3)) +
      geom_line(aes(time, value, color = type), size = 1.50) +
      facet_wrap(~model) +
      theme_bw() +
      #labs(title = "SIR Distribution") +
      #scale_color_brewer(type = 'qual') +
      
      
      scale_color_manual( 
        #values = c("red", "yellow", "green", "blue"))
        values = cbPalette)
    sirPlot
  }, ignoreNULL = FALSE)
  
  output$curvePlot2 <- renderPlot({
    
    sirPlot()
    
  })
  
  #### simulation plots
  
  testSim <- reactive({
    simrResults <- runSims(graphObj = dlContactGraph,modelObj = covid_model(), runs = input$runs,timeSteps = 50)
    
    testSim <- simResults$sirStats 
  })
  
  
  output$simPlot <- renderPlot({
    
    sumSim <- testSim() %>% 
      group_by(type,time) %>% 
      summarize(meanValue = median(value)) %>% 
      ungroup() %>% 
      mutate(typeFac = factor(x = type,levels = c("susceptible","infected","recovered","leave") ))
    
    testSim() %>% 
      mutate(group = paste0(type,simRun)) %>% 
      mutate(typeFac = factor(x = type,levels = c("susceptible","infected","recovered","leave") )) %>% 
      # filter(type == "infected") %>% 
      ggplot() +
      geom_line(aes(x = time, y = value, group = group ), color = "grey", size = 1.5, alpha = 0.2) +
      geom_line(data = sumSim, aes(x = time, y = meanValue, color = typeFac), size = 1.5) +
      theme_bw() +
      labs(title = "Discovery Lab SIR Distribution") +
      scale_color_brewer(palette = "Dark2") +
      facet_wrap(~typeFac, nrow = 2) +
      labs(color = "Status")
  })
  
  
  simPlot2 <- eventReactive(input$update,{
    
    simResults <- runSims(graphObj = dlContactGraph,modelObj = covid_model(), runs = input$runs,timeSteps = 50)
    
    testSim <- simResults$sirStats  
    
    sumSim <- testSim %>% 
      group_by(type,time) %>% 
      summarize(meanValue = median(value)) %>%
      ungroup() %>% 
      mutate(typeFac = factor(x = type,levels = c("susceptible","infected","recovered","leave") ))
    
    
    # Plot stats
    simPlot2 <- testSim %>% 
      mutate(group = paste0(type,simRun)) %>% 
      mutate(typeFac = factor(x = type,levels = c("susceptible","infected","recovered","leave") )) %>% 
      # filter(type == "infected") %>% 
      ggplot() +
      geom_line(aes(x = time, y = value, group = group ), color = "grey", size = 1.5, alpha = 0.2) +
      geom_line(data = sumSim, aes(x = time, y = meanValue, color = typeFac), size = 1.5) +
      theme_bw() +
      labs(title = "Discovery Lab SIR Distribution 2") +
      scale_color_brewer(palette = "Dark2") +
      facet_wrap(~typeFac, nrow = 2) +
      labs(color = "Status")
    simPlot2
  }, ignoreNULL = FALSE)
  
  output$simPlot2 <- renderPlot({
    
    simPlot2()
    
  })
  
  
  ####  
  output$test0Plot <- renderPlot({
    set.seed(4321); plot(test0()[[input$day]], vertex.label = '', vertex.size = 3, main=paste0("Random Network of Size ", n()))
    #legend("bottomleft", 
    #legend = c("Group 1", "Group 2"))
    legend("topleft",c("Susceptible","Infected", "Recovered"),cex=1.25,pch=21,col=c("blue","red", "green"),pt.bg = c("blue","red", "green"), title="SER", text.font=0)
  })
  
  output$test1Plot <- renderPlot({
    set.seed(4321); plot(test1()[[input$day]], vertex.label = '', vertex.size = 3, main=paste0("Scale Free Network of Size ", n()))
    
  })
  
  output$test2Plot <- renderPlot({
    set.seed(4321); plot(test2()[[input$day]], vertex.label = '', vertex.size = 3, main=paste0("Small World Network of Size ", n()))
    
  })
  
  output$test3Plot <- renderPlot({
    set.seed(4321); plot(test3()[[input$day]], vertex.label = '', vertex.size = 3, main=paste0("Discovery Lab Contact Network", dlGraphSize))
    
  })
  
  observeEvent(input$reset_input, {
    shinyjs::reset("side-panel")
  })
  
  
  rfnDeg <- reactive({
    round(mean(igraph::degree(test0()[[input$day]])),2)
  })
  
  scfnDeg <- reactive({
    round(mean(igraph::degree(test1()[[input$day]])),2)
  })
  
  swfnDeg <- reactive({
    round(mean(igraph::degree(test2()[[input$day]])),2)
  })
  
  dlfnDeg <- reactive({
    round(mean(igraph::degree(test3()[[input$day]])),2)
  })
  
  
  avgLeave <- function(test){
    total = 0
    j = 0
    for(i in 1:n()){
      #print(vertex.attributes(test[[input$day]], i)[["leave"]])
      #print(i)
      if(vertex.attributes(test[[input$day]], i)[["leave"]] == TRUE) {
        total = total + vertex.attributes(test[[input$day]], i)[["leaveCounter"]]
        j = j + 1
      }
    }
    if(total == 0  || j == 0) {
      meanLeave <- 0
    } else {
      meanLeave <- total/j
    }
    round(meanLeave,2)
  }
  
  dayDays <- function(days){
    if (days == 1) {"Day"} else {"Days"}
  }
  
  rfnLeaveCnt <- reactive({
    avgLeave(test0())
  })
  
  scfnLeaveCnt <- reactive({
    avgLeave(test1())
  })
  
  swfnLeaveCnt <- reactive({
    avgLeave(test2())
  })
  
  dlfnLeaveCnt <- reactive({
    avgLeave(test3())
  })
  
  testCost <- reactive({
    if("pcr" == input$testTypes){
      testCost <- 100
    } else if ("antigen" == input$testTypes){
      testCost <- 5
    } else if ("lamp" == input$testTypes){
      testCost <- 20
    }
  })
  
  ppeCost <- reactive({
    if("masks" %in% input$ppeBought){
      masks <- 0.75
    } else {
      masks <- 0
    }
    
    if("shields" %in% input$ppeBought){
      shields <- 1.10
    } else {
      shields <- 0
    }
    
    if("handSan" %in% input$ppeBought){
      handSan <- 0.2
    } else {
      handSan <- 0
    }
    if("gloves" %in% input$ppeBought){
      gloves <- 0.15
    } else {
      gloves <- 0
    }
    
    return(masks + shields + handSan + gloves)
  })
  
  output$summary <- renderText({
    if (input$tabs == "Random Force Network") {
      totalLeave <- sum(vertex.attributes(test0()[[input$day]])[["leaveCounter"]])
      paste("Mean Degree:", rfnDeg(), "Connections", "\nMean Time Out Of Office If On Leave:", rfnLeaveCnt(), dayDays(rfnLeaveCnt()), "\nCumulative Time Out On Leave:",  totalLeave, dayDays(totalLeave))
    } else if (input$tabs == "Scale Free Force Network") {
      totalLeave <- sum(vertex.attributes(test1()[[input$day]])[["leaveCounter"]])
      paste("Mean Degree:", scfnDeg(), "Connections", "\nMean Time Out Of Office If On Leave:", scfnLeaveCnt(), dayDays(scfnLeaveCnt()), "\nCumulative Time Out On Leave:",  totalLeave, dayDays(totalLeave))
    } else if (input$tabs == "Small World Force Network") {
      totalLeave <- sum(vertex.attributes(test2()[[input$day]])[["leaveCounter"]])
      paste("Mean Degree:", swfnDeg(), "Connections", "\nMean Time Out Of Office If On Leave:", swfnLeaveCnt(), dayDays(swfnLeaveCnt()), "\nCumulative Time Out On Leave:",  totalLeave, dayDays(totalLeave))
    } else if (input$tabs == "Discovery Lab Contact Network") {
      totalLeave <- sum(vertex.attributes(test3()[[input$day]])[["leaveCounter"]])
      paste("Mean Degree:", dlfnDeg(), "Connections", "\nMean Time Out Of Office If On Leave:", dlfnLeaveCnt(), dayDays(dlfnLeaveCnt()), "\nCumulative Time Out On Leave:",  totalLeave, dayDays(totalLeave))
    }else if (input$tabs == "SIR Distribution") {
      bestAvgLeave <- mean(c(
        sum(vertex.attributes(test0()[[input$day]])[["leaveCounter"]]), 
        sum(vertex.attributes(test1()[[input$day]])[["leaveCounter"]]), 
        sum(vertex.attributes(test2()[[input$day]])[["leaveCounter"]])))
      paste(
        #"Total Cumulative Leave Costs:", dollar(input$avgWage * bestAvgLeave), 
        "Total Cumulative Tests Needed:", ceiling((input$day/input$testFrequency) * input$propTested * n()),
        "\nTotal Cumulative Testing Costs:", dollar((input$day/input$testFrequency) * input$propTested * n() * testCost()),
        "\nTotal Cumulative PPE Costs:", dollar((input$day * n() * ppeCost())))
    } else {
      return()
    }
    
  })
  
  output$table <- renderTable({
    
    if (input$tabs == "SIR Distribution") {
      df <- data.frame("Random" = dollar(input$avgWage * 8 * sum(vertex.attributes(test0()[[input$day]])[["leaveCounter"]])),
                       "ScaleFree" = dollar(input$avgWage * 8 * sum(vertex.attributes(test1()[[input$day]])[["leaveCounter"]])),
                       "SmallWorld" = dollar(input$avgWage * 8 * sum(vertex.attributes(test2()[[input$day]])[["leaveCounter"]])),
                       "DiscoveryLab" = dollar(input$avgWage * 8 * sum(vertex.attributes(test3()[[input$day]])[["leaveCounter"]]))
                       )
      
      df
      
    } else if (input$tabs == "Random Force Network") {
      stats <- addLeave(test0())
      stats[stats$time == input$day,]
      
    } else if (input$tabs == "Scale Free Force Network") {
      stats <- addLeave(test1())
      stats[stats$time == input$day,]
      
    } else if (input$tabs == "Small World Force Network") {
      stats <- addLeave(test2())
      stats[stats$time == input$day,]
      
    } else if (input$tabs == "Discovery Lab Contact Network") {
      stats <- addLeave(test3())
      stats[stats$time == input$day,]
      
    } else {
      return()
    }
  },
  striped = TRUE,
  width = '100%',
  align = 'c',
  bordered = TRUE
  )
  
  # getPage<-function() {
  #   return(includeHTML("NetworkInfection.html"))
  # }
  # output$inc<-renderUI({includeHTML("NetworkInfection.html")})
  
  # Ensure that the number of infected cannot exceed the total number of nodes
  observe({
    val <- input$n
    updateSliderInput(session, "init_num", max = val)
    #View(vertex.attributes(test2()[[input$day]], 415)[["leave"]] == TRUE)
  })
  
})
