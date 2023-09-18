library(shinyjs)
library(shinythemes)

# Define UI for dataset viewer application
shinyUI(fluidPage(
  
  # Include shiny js
  useShinyjs(),
  
  # Appearance
  theme = shinytheme("sandstone"),
  
  # Application title
  titlePanel("Welcome to the NiLeDAM online application!"),
  
  sidebarLayout(
    sidebarPanel(
      h2("Data importation"),
      
      h5(HTML("<b>Load an example dataset...</b>")),
      checkboxInput('loadsrilanka', 'Load Srilanka?', FALSE),
      p(HTML("")),
      fileInput('file1', '... or choose CSV/TXT File',
                accept=c('text/csv', 'text/comma-separated-values,text/plain')),
      checkboxInput('header', ' Header?', TRUE),
      checkboxInput('rownames', ' Row names?', FALSE),
      selectInput('sep', 'Separator:', c(Comma=',',Semicolon=';',Tab='\t'),
                  'Comma'),
      selectInput('quote', 'Quote:',
                  c(None='','Double Quote'='"','Single Quote'="'"),
                  'Double Quote'),
      selectInput('dec', 'Decimal mark', c(Dot='.', Comma=','), 'Dot'),
      br(),
      actionButton('reset', 'Reset current settings'),
      br(), br(),
      h2("Age calculation"),
      helpText(HTML("<strong>Note</strong>: When your dataset is properly
                    imported, go to the &ldquo; Ages &rdquo; panel and choose
                    the number of bootstrap samples as well as the risk for 
                    confidence intervals. The calculation will be run 
                    automatically. The table of results will be displayed right 
                    below. <strong>You have to be patient</strong>: the 
                    computation time can be large.")),
      h2("Tests"),
      helpText(HTML("Once the ages are calculated, the &ldquo; Test &rdquo; 
                    panel is used to determine the number of age populations the
                    data are likely to come from.")),
      br(),
      p(HTML("<div style='background-color:#79776F;border:0px solid black;color:white;'>
		         This application is a graphical interface of the 
		         <a href='https://CRAN.R-project.org/package=NiLeDAM'>NiLeDAM</a> 
		         package using the <a href='https://www.r-project.org/'>R</a> 
		         software environment. If you have any trouble using it, please do 
		         not hesitate to contact 
		         <a mailto:'nathalie.vialaneix[AT]inrae.fr'>Nathalie Vialaneix</a>, 
		         the package maintainer.<br>
		         The application scripts are available at:<br>
		         <code><font color='#870500'><b>
		           git clone https://gitlab.nathalievialaneix.eu/nathalie/niledam
		         </b></font></code><br>and distributed <strong>without any 
		         guarantee</strong> under the 
		         <a href='http://cran.r-project.org/web/licenses/GPL-3'>GPL-3</a>
		         license.
           </div>"))
    ),
    
    mainPanel(
      tabsetPanel(
        
        tabPanel("User guide - Data",
                 h3("Import data files"),
                 p(HTML("To run the application, import your file with 
                        (U, Th, Pb) contents and corresponding errors. This 
                        file must be a text file having 
                        <strong>ONLY 6 columns</strong>: U, errU, Th, ErrTh, 
                        Pb, ErrPb, <strong>in this order</strong> (with 
                        eventually a first column containing the sample 
                        names).<br>")),
                 p("See an example of a classic data file below (with default
                   importation settings):"),
                 img(src = "DataFileFormatExample.png", height = '200px', width = '717px'),
                 br(),br(),
                 p(HTML("Once your file local path is specified in the &ldquo; 
                        Browse &rdquo; field of the left panel, the data are 
                        automatically uploaded. If the upload is done properly, 
                        they are displayed in the &ldquo; Data &rdquo; tab. 
                        Then, age calculations can be performed in the &ldquo; 
                        Ages &rdquo; tab and tests for the estimation of the
                        population number are also available in the &ldquo; 
                        Tests &rdquo; tab.")),
                 br(),br(),
                 h3("The dataset you want to use is displayed below:"),
                 h5("(only the first 50 first lines if the dataset contains more than 50 lines)"),
                 tableOutput("view")
        ),
        
        tabPanel("Ages",
                 br(),
                 numericInput("boot", "Number of bootstrap samples (advised:
                              1000):", 1000, width = '450px'),
                 numericInput("risk","Risk (%) for the confidence interval
                              (advised: 5%):", 5, width = '450px'),
                 numericInput("seed",
                              HTML("Set a random seed for reproducible results
                                   <a href='#pseudor'><sup>(1)</sup></a>:"),
                              as.numeric(format(Sys.time(), "%M%S")), width = '450px'),  
                 br(),br(),
                 p(HTML("If your dataset has been properly imported, wait a few
                        minutes to get the results of the age estimations. The
                        estimations are displayed below when available and can
                        be dowloaded.")),
                 br(),
                 downloadButton('downloadAges', 'Download Ages'),br(),br(),
                 tableOutput("ages"),
                 p(HTML("<a name='pseudor'><sup>(1)</sup></a> The age estimation
                        is based on a Monte Carlo method that uses randomness.
                        Setting a seed results in fixing the random procedure in
                        order to obtain reproducible results (runing several
                        times the process with the same random seed will give 
                        the same results). More information on pseudo-random 
                        generators at 
                        <a href='http://en.wikipedia.org/wiki/Pseudorandom_number_generator'>this link</a>."))),
        
        tabPanel("Tests",
                 br(),
                 helpText(HTML("Either supply <code>nbmin</code> and 
                               <code>nbmax</code> to find the most probable age 
                               population number or set <code>nbmax</code>
                               to 0 to perform a test for <code>nbmin</code> age 
                               populations.")),
                 numericInput("nbmin", "Minimal number of populations tested:", 1),
                 numericInput("nbmax","Maximal number of populations tested", 0),
                 numericInput("level","Risk (%) for the confidence interval
                              (advised: 5%):", 5),
                 verbatimTextOutput("testres"),
                 downloadButton('downloadPops', 'Download population numbers'),
                 br(),br(),tableOutput("whichpop")),
        
        tabPanel("Plots",
                 br(),
                 h3("Densities"),
                 fluidRow(
                   column(textInput("title","Title","Ages"), width = 4),
                   column(textInput("color","Color","red"), width = 4)
                 ),
                 plotOutput("densities", height = '500px'),
                 br(),hr(),
                 h3("Populations"),
                 textInput("title2","Title","Populations"),
                 plotOutput("populationline", height = '500px'),
                 br(),br()
        )
      )
    )
  )

))
