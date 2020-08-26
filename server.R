# Define server logic for application that modifies network graphs
shinyServer(function(input, output, session) {
  
  n <- reactive({as.numeric(input$n)})
  ed <- reactive({n() * 4})
  
  prob.infect <- .1
  gmma <- 14
  
  faceCovering <- 1
  eyeProtection <- 1.0
  distancing <- 1.0
  
  # output$policies<-renderText({
  # 
  #   faceCovering <- "face" %in% input$npi
  #   eyeProtection <- "eye" %in% input$npi
  #   distancing <- "dist" %in% input$npi
  # 
  #   if (faceCovering){
  #     s <- 8
  #   } else if (eye){
  #     s <- 5
  #   } else if (distancing){
  #     distancing <- 0.9
  #   }
  # })
  
  
  covid_model <- reactive({
    if("face" %in% input$npi){
      faceCovering <- 0.9
    } else if ("eye" %in% input$npi){
      eyeProtection <- 0.9
    } else if ("dist" %in% input$npi){
      distancing <- 0.85
    }
    covid_ppe <- default_ppe(faceCovering = faceCovering, eyeProtection = eyeProtection, distancing = distancing, compliance = input$compliance)
    covid_di <- ppe_infect(init_num = input$init_num, rate = prob.infect)
    covid_dr <- default_recover(max_recovery_time = input$max_recovery_time)
    covid_dt <- default_testing(testDelay = input$testDelay, testFrequency = input$testFrequency, falseNegRate = 0.03, falsePosRate = 0.001, propTested = input$propTested)
    covid_lv <- default_leave(leaveDuration = input$leaveDuration, max_recovery_time = input$max_recovery_time)
    covid_model <- infection_model(components = list(covid_ppe, covid_di, covid_dr,covid_dt, covid_lv ))
    covid_model
  })
  
  #################
  ## Initialize graph networks
  #################
  
  # Erdos-Renyi network: constant probability to connect nodes
  set.seed(4321); rn <- reactive({sample_gnm(n(), ed(), directed = F) %>%
      covid_model()$init_model()})
  
  # Scale free network
  set.seed(4321); sfree <- reactive({sample_fitness_pl(n(), ed(), 2.2) %>%
      covid_model()$init_model()})
  
  # Small world network
  set.seed(4321); sw <-  reactive({sample_smallworld(1, n(), 4, .1) %>%
      covid_model()$init_model()})
  
  #################
  ## Progress models through time steps
  #################
  
  set.seed(4321)
  test0 <- reactive({createTimeline(rn(), 60, covid_model())})
  test1 <- reactive({createTimeline(sfree(), 60, covid_model())})
  test2 <- reactive({createTimeline(sw(), 60, covid_model())})
  
  
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
                       radiusCalculation = JS(" Math.sqrt(d.nodesize)+6"),
                       # Changing the color displayed to yellow to make it easier to distinguish between red and orange
                       colourScale = JS('d3.scaleOrdinal().domain(["blue", "green", "yellow", "red"]).range(["#0000FF", "#008000", "#FFFF00", "#FF0000"])'),
                       Group = 'group', charge = -500, bounded = TRUE, clickAction = clickJS)
    
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
  
  addLeave <- function(test){
    stats <- getStats(test)
    df <- data.frame("time" = 1, "type" = "on leave", "value" = 0, stringsAsFactors = FALSE)
    
    for (i in 2:60) {
      
      total <- 0
      
      for(j in vertex.attributes(test[[i]])$color){
        if(j == "yellow"){
          total = total + 1
        }
      }
      
      df = rbind(df,list(i,"on leave",total))
    }
    
    stats = rbind(stats, df)
    stats[, c(1,3)] <- sapply(stats[, c(1,3)], as.integer)
    return(stats)
    
  }
  
  output$curvePlot <- renderPlot({
    # Generate stat blocks of each network
    stats0 <- getStats(test0())
    stats1 <- getStats(test1())
    stats2 <- getStats(test2())
    
    stats0$model <- 'Random'
    stats1$model <- 'Scale Free'
    stats2$model <- 'Small World'
    
    # Plot stats
    ggplot(rbind(stats1, stats0, stats2)) +
      geom_line(aes(time, value, color = type), size = 1.5) +
      facet_wrap(~model) +
      theme_bw() +
      #labs(title = "SIR Distribution") +
      scale_color_brewer(type = 'qual')
  })
  
  
  # Wait for the update button to be clicked to compare parameters changing
  sirPlot <- eventReactive(input$update,{
    # Generate stat blocks of each network
    stats0 <- getStats(test0())
    stats1 <- getStats(test1())
    stats2 <- getStats(test2())
    
    stats0$model <- 'Random'
    stats1$model <- 'Scale Free'
    stats2$model <- 'Small World'
    
    # Plot stats
    sirPlot <- ggplot(rbind(stats1, stats0, stats2)) +
      geom_line(aes(time, value, color = type), size = 1.50) +
      facet_wrap(~model) +
      theme_bw() +
      #labs(title = "SIR Distribution") +
      scale_color_brewer(type = 'qual')
    sirPlot
  }, ignoreNULL = FALSE)
  
  output$curvePlot2 <- renderPlot({
    
    sirPlot()
    
  })
  
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
  
  observeEvent(input$reset_input, {
    shinyjs::reset("side-panel")
  })
  
  output$summary <- renderText({
    if (input$tabs == "SIR Distribution") {
      "Summary stats for SIR."
    } else if (input$tabs == "Random Force Network") {
      "Summary stats for RFN."
      #print(vertex.attributes(test0()[[input$day]], "color"))
    } else if (input$tabs == "Scale Free Force Network") {
      "Summary stats for SCFN."
    } else if (input$tabs == "Small World Force Network") {
      "Summary stats for SWFN."
    } else {
      "Summary stats for Original."
    }
  })
  
  
  output$table <- renderTable({
    
    if (input$tabs == "SIR Distribution") {
      "Summary stats for SIR."
      
    } else if (input$tabs == "Random Force Network") {
      "Summary stats for SCFN."
      stats <- addLeave(test0())
      stats[stats$time == input$day,]
      
    } else if (input$tabs == "Scale Free Force Network") {
      "Summary stats for SCFN."
      stats <- addLeave(test1())
      stats[stats$time == input$day,]
      
    } else if (input$tabs == "Small World Force Network") {
      "Summary stats for SWFN."
      stats <- addLeave(test2())
      stats[stats$time == input$day,]
      
    } else {
      "Summary stats for Original."
    }
  },
  striped = TRUE,
  width = '100%',
  align = 'c',
  bordered = TRUE
  )
  
})
