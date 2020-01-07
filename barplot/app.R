#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Rshiny ideas from on https://gallery.shinyapps.io/multi_regression/
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
require(ggplot2)
library(shiny) 
options(max.print=1000000)
fig.width <- 1200
fig.height <- 850
library(shinythemes)        # more funky looking apps
p1 <- function(x) {formatC(x, format="f", digits=1)}
p2 <- function(x) {formatC(x, format="f", digits=2)}
options(width=100)
set.seed(123)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ui <- fluidPage(theme = shinytheme("journal"),
                
                shinyUI(pageWithSidebar(
                    
                    
                    headerPanel("Horizontal bar plot with counts and percentages"),
                    
                    sidebarPanel( 
                        
                        div(strong("A simple bar plot nothing more, nothing less."),p(" ")), 
                        
                        div(
                            
                         
                            
                            actionButton(inputId='ab1', label="R code",   icon = icon("th"),  
                                         onclick ="window.open('https://raw.githubusercontent.com/eamonn2014/Simple-barplot/master/barplot/app.R', '_blank')"),   
                            actionButton("resample", "Simulate a new sample"),
                            br(), br(),
                           
                            
                            
                            div(("Hit 'Simulate a new sample' for a new bar plot.  
                                  ")),
                            br(),
                            div(("  
                                 The number of bars is chosen randomly from 7 to 30 and the counts from 1:2200.")),  
                          
                            
                            
                        )
                    ),
                    
                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~tab panels
                    mainPanel(
                        
                        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        #    tabsetPanel(type = "tabs", 
                        navbarPage(       
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
                            tags$style(HTML(" 
                            .navbar-default .navbar-brand {color: cyan;}
                            .navbar-default .navbar-brand:hover {color: blue;}
                            .navbar { background-color: lightgrey;}
                            .navbar-default .navbar-nav > li > a {color:black;}
                            .navbar-default .navbar-nav > .active > a,
                            .navbar-default .navbar-nav > .active > a:focus,
                            .navbar-default .navbar-nav > .active > a:hover {color: pink;background-color: purple;}
                            .navbar-default .navbar-nav > li > a:hover {color: black;background-color:yellow;text-decoration:underline;}
                            .navbar-default .navbar-nav > li > a[data-value='t1'] {color: red;background-color: pink;}
                            .navbar-default .navbar-nav > li > a[data-value='t2'] {color: blue;background-color: lightblue;}
                            .navbar-default .navbar-nav > li > a[data-value='t3'] {color: green;background-color: lightgreen;}
                   ")), 
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end of section to add colour     
                            tabPanel("Horizontal bar plot with counts and percentages", 
                                     
                                     div(plotOutput("reg.plot", width=fig.width, height=fig.height)),  
                                     
                                    p(strong("")) ,
                             
                                     
                            ) 
                            
                     
                        )
                        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    )
                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end tab panels
                    
                    #  ) #new
                )
                )
)

server <- shinyServer(function(input, output) {
    
    # --------------------------------------------------------------------------
    # This is where a new sample is instigated only random noise is required to be generated
    random.sample <- reactive({
        
        # Dummy line to trigger off button-press
        foo <- input$resample
        
        n <- input$n 

        return(list(   n =n  
            
        ))
        
    }) 
    
    # --------------------------------------------------------------------------
    # Set up the dataset based on the inputs 
    make.regression <- reactive({
        
        #   https://stats.stackexchange.com/questions/28876/difference-between-anova-power-simulation-and-power-calculation
        
        sample <- random.sample()
        
        n<- sample$n
        
        first <- c("Fear", "Frontier", "Nanny", "Job", "Yard", "Airport",
                   "Half Pint", "Commando", "Fast Food", "Basketball", "Bachelorette",
                   "Diva", "Baggage", "College", "Octane", "Clean", "Sister", "Army", "Drama", 
                   "Backyard", "Pirate", "Shark", "Project", "Model", "Survival", "Justice", 
                   "Mom", "New York", "Jersey", "Ax", "Warrior", "Ancient", "Pawn", "Throttle",
                   "The Great American", "Knight", "American", "Outback", "Celebrity", "Air", 
                   "Restaurant", "Bachelor", "Family", "Royal", "Surf", "Ulitmate", "Date", 
                   "Operation", "Fish Tank", "Logging", "Hollywood", "Amateur", "Craft",
                   "Mystery", "Intervention", "Dog", "Human", "Rock", "Ice Road", "Shipping", 
                   "Modern", "Crocodile", "Farm", "Amish", "Single", "Tool", "Boot Camp", 
                   "Pioneer", "Kid", "Action", "Bounty", "Paradise", "Mega", "Love", "Style", 
                   "Teen", "Pop", "Wedding", "An American", "Treasure", "Myth", "Empire",
                   "Motorway", "Room", "Casino", "Comedy", "Undercover", "Millionaire", 
                   "Chopper", "Space", "Cajun", "Hot Rod", "The", "Colonial", "Dance", 
                   "Flying", "Sorority", "Mountain", "Auction", "Extreme", "Whale",
                   "Storage", "Cake", "Turf", "UFO", "The Real", "Wild", "Duck", 
                   "Queer", "Voice", "Fame", "Music", "Rock Star", "BBQ", "Spouse", 
                   "Wife", "Road", "Star", "Renovation", "Comic", "Chef", "Band", 
                   "House", "Sweet")
        
        number.of.bars <- sample(7:30, 1)
        
        what <- sample(first, number.of.bars, replace=FALSE)
        
        N <- sample(1:2200, number.of.bars, replace=TRUE)
        
        f <- data.frame(what=what, N=N)
        
        return(list(f=f )) 
        
    })  
    
    
    
    # --------------------------------------------------------------------------
    #---------------------------------------------------------------------------
    # Plot a scatter of the data  
    
    output$reg.plot <- renderPlot({         
        
        # Get the current regression data
        data1 <- make.regression()
        
        f <- data1$f
        
        
        
        f$Percentage <- round(f$N/sum(f$N)*100,1)
        z <- f  # data set for plot
        variable <- "what"  # variable of interest
        pN <- sum(z$N)  # 
        roundUp <- function(x) 10^ceiling(log10(x))/4
        gupper <- roundUp((max(z$N)))  # plot upper limit
        glower <- 250                  # plot lower limit
        gstep <- 250                   # grid steps
        
        # text for plot
        ylabel <- "Counts" 
        
        
        Gplotx <- function(data,  l1,l2,l3, varr) {
            
            mlimit=l1
            
            p1 <- ggplot(data = data, aes(x = reorder(eval(parse(text=varr)), N), y = N, 
                                          fill = eval(parse(text=varr)))) + 
                
                geom_hline(yintercept=seq(l2, mlimit, by=l3),linetype =3, size=0.3,
                           alpha=1, color='brown') +
                
                geom_bar(stat = "identity", width =0.7) 
            
            
            p1 <- p1 + ggtitle( "Horizontal bar plot with counts and percentages" ) +
                theme(plot.title = element_text(size = 20, face = "bold")) +
                
                coord_flip()
            
            p1 <- p1 + ylab(ylabel) + 
                
                xlab(" ") +
                
                guides(fill=guide_legend(title=paste0("(",2,"-digit - ICD9 code)")), size = 14) 
            
            p1 <- p1 + geom_text(aes(label=paste0(format(N, big.mark=","
                                                         ,scientific=FALSE)," (",Percentage,"%)")),position = "stack", 
                                 hjust=-0.2, size = 3, check_overlap = F)
            
            p1 <- p1 + scale_y_continuous(limits = c(0, mlimit)) 
            
            p1 <- p1 + theme(panel.background=element_blank(),
                             plot.title=element_text(), plot.margin = unit(c(5.5,12,5.5,5.5), "pt"), 
                             legend.text=element_text(size=12),
                             legend.title=element_text(size=14),
                             axis.text.x = element_text(size=11),
                             axis.text.y = element_text(size=11),
                             axis.line.x = element_line(color="black"),
                             axis.line.y = element_line(color="black"),
                             plot.caption=element_text(hjust = 0, size = 7))
            
            g <- p1 + theme(legend.position="none")
            
            
        }
        
        gx <- Gplotx(data = z,   l1=gupper,l2=glower,l3=gstep, varr=variable) 
        
        
        print(gx)
        
        
        
    })
    #---------------------------------------------------------------------------
    #--------------------------------------------------------------------------
    #---------------------------------------------------------------------------
   
    })
   

# Run the application 
shinyApp(ui = ui, server = server)