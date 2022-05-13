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
      actionButton('reset', 'Effacer la sélection'),
      br(), br(),
      h2("Age calculation"),
      helpText(HTML("<strong>Note</strong>: When your dataset is properly
                   imported, go to the 'Ages' panel and choose the number of
                   bootstrap samples as well as the risk for confidence intervals. 
                   The calculation will run automatically. Table of results 
                   will be displayed right below.
                   <strong>You have to be patient</strong>, 
                    the computation time can be large.")),
      h2("Tests"),
      helpText(HTML("Once the ages are calculated, the 'Test' panel is used to 
                  determine from which number of age population the analyses
                  are coming.")),
      br(),
      p(HTML("<div style='background-color:#79776F;border:0px solid black;color:white;'>
		       This application is a graphical interface of the <a
           href='http://niledam.r-forge.r-project.org/'>NiLeDAM</a> package
           using the <a href='https://www.r-project.org/'>R</a> software
           environment. If you have any trouble using it, please do not hesitate
           to contact <a mailto:'nathalie.vialaneix[AT]inrae.fr'>Nathalie 
           Vialaneix</a>, the package's maintainer.<br>
           The application scripts are available on GitHub:<br> 
           <code><font color='#870500'><b>git clone
           https://github.com/tuxette/niledam.git</b></font></code><br>and
           distributed <strong>without any guarantee</strong> under the licence
           <a href='http://cran.r-project.org/web/licenses/GPL-3'>GPL-3</a>.
           </div>")),
      p(HTML("")),
      p(HTML("<strong>If you are using the application for your publications,
           please
<a href='https://owncloud.nathalievilla.org/apps/files_sharing/get.php?token=2bd2782289949699ce4f8246dc2ef62363806cf2'>
           cite us</a>.</strong>"))
    ),
    
    mainPanel(
      tabsetPanel(
        
        tabPanel("Data",
                 h3("Basic user guide"),
                 p(HTML("To run the application, import your file with (U, Th, Pb)
                      contents and corresponding errors. This file must be a
                      text file having <strong>ONLY 6 columns</strong>: U, errU,
                      Th, ErrTh, Pb, ErrPb, in this order (with eventually a
                      first column containing the sample names). CSV format is
                      the easiest to use: such an example is given <a 
href='http://owncloud.nathalievilla.org/apps/files_sharing/get.php?token=51731049ed775fbe40746670171205596fff16f4'>
                      here</a> that is imported with the default parameters
                      specified on the left panel.<br>")),
                 p("Once your file's URL is pasted, the data are imported and if
                 the importation is done properly, the data are displayed on the
                 main panel. Then, you can perform the age calculation and the
                 tests by increasing the number of bootstrap samples."),
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
                 downloadButton('downloadAges', 'Download Ages'),br(),br(),
                 tableOutput("ages"),
                 p(HTML("<a name='pseudor'><sup>(1)</sup></a> The age estimation
                      is based on Monte Carlo method that uses randomness.
                      Setting a seed results in fixing the random procedure in
                      order to obtain reproducible results (runing several times
                      the process with the same random seed will give the same
                      ages). More information on pseudo-random generators at <a
href='http://en.wikipedia.org/wiki/Pseudorandom_number_generator'
                     >this link</a>."))),
        
        tabPanel("Tests",
                 br(),
                 helpText(HTML("Either supply 'nbmin' and 'nbmax' to find the
                             most probable age population number or set 'nbmax'
                             to 0 to perform a test for 'nbmin' age 
                             populations.")),
                 numericInput("nbmin", "Minimal number of populations tested:",1),
                 numericInput("nbmax","Maximal number of populations tested",0),
                 numericInput("level","Risk (%) for the confidence interval
                            (advised: 5%):", 5),
                 verbatimTextOutput("testres"),
                 downloadButton('downloadPops', 'Download population numbers'),
                 br(),br(),tableOutput("whichpop")),
        
        tabPanel("Graphics",
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
