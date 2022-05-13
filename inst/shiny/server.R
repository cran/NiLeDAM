library(NiLeDAM)
library(thematic)

shinyServer(function(input, output, session) {
  
  # Custom plots as to match the app style
  thematic_shiny()
  
  # Initiate a variable identifying the state of file input widget (file uploaded or not yet/anymore?)
  # Make it react with different buttons
  fileinput.values <- reactiveValues(upload.state = NULL)
  observeEvent(input$file1, {fileinput.values$upload.state <- 'uploaded'})
  observeEvent(input$reset, {
    fileinput.values$upload.state <- 'reset'
    reset('file1')
    reset('loadsrilanka')
    reset('header')
    reset('rownames')
    reset('sep')
    reset('quote')
    reset('dec')
    })
  
  tripletInput <- reactive({
    # Not working with the example
    if (input$loadsrilanka == FALSE){
      
      in.file <- input$file1
      
      # When input$file1 empty (initial state)
      if (is.null(in.file)){return(NULL)}
      
      # When file uploaded
      else if(fileinput.values$upload.state == 'uploaded'){
        if (input$rownames){
          read.table(in.file$datapath, header=input$header, sep=input$sep,
                     quote=input$quote, row.names=1, dec=input$dec)
        }else{
          read.table(in.file$datapath, header=input$header, sep=input$sep,
                     quote=input$quote, dec=input$dec)
          }
        }
      
      # When we want to remove it
      else if(fileinput.values$upload.state == 'reset'){return(NULL)}
      
    # Working with the example
    } else {
      srilanka
    }
    
  })
  output$view <- renderTable({
    head(tripletInput(), n=50)
  })

	agesCalculation <- reactive({
		the.triplets <- tripletInput()
    if (input$boot==0)
      return(NULL)
		all.ages <- calculateAges(measures=the.triplets, input$boot, input$risk/100,
                              seed=input$seed) 
	})
  
  agesDataSet <- reactive({
    ages.object <- agesCalculation()
    if (is.null(ages.object))
      return(NULL)
    ages.dataset <- cbind(ages.object@ages, ages.object@sd, t(ages.object@ci))
    ages.dataset <- data.frame(ages.dataset)
    names(ages.dataset) <- c("ages", "sd",
                             paste("CI-",(ages.object@level/2*100),"%",sep=""),
                             paste("CI-",(100-ages.object@level/2*100),"%",
                                   sep=""))
    ages.dataset
  })
  
  output$ages <- renderTable({
    agesDataSet()
  })
  
  output$downloadAges <- downloadHandler(
    filename = "ages.csv",
    content = function(file) {
      write.csv(agesDataSet(), file)
  })

	testsRes <- reactive({
		the.ages <- agesCalculation()
    nbmax <- input$nbmax
    if (nbmax==0) {
      the.tests <- tests(the.ages, nbmin=input$nbmin, level=input$level/100,
                         verbose=FALSE)
    } else {
		  the.tests <- tests(the.ages, nbmin=input$nbmin, nbmax=input$nbmax,
                         level=input$level/100, verbose=FALSE)
    }
	})
  
  output$testres <- renderPrint({
    print(testsRes())
  })
  
  prepareContent <- reactive({
    test.res <- testsRes()
    if (class(test.res)=="oneAgeTest") {
      content <- test.res@which.pop
    } else content <- test.res@best.res@which.pop
    content
  })
  
  output$downloadPops <- downloadHandler(
    filename = "populations.csv",
    content = function(file) {
      write.table(prepareContent(), file, row.names=FALSE, col.names=FALSE)
    }
  )
 
	output$densities <- renderPlot({
		test.res <- testsRes()
		plot(test.res,main=input$title,col=input$color)
  })
	
	output$populationline <- renderPlot({
	  test.res <- testsRes()
	  popline(test.res,main2=input$title2)
	})
	
})



