library(dplyr)
library(httr)
library(shiny)
library(stringr)

# plot.basho(df)
source("basho-plotly.R")

options(stringsAsFactors = FALSE)


# Look up available tournaments (basho) in GitHub repository: Cervus1983/sumodb
basho <- str_match_all(
	content(GET(url = "https://github.com/Cervus1983/sumodb/tree/master/CSV"), "text"),
	"([0-9]{4}\\.[0-9]{2})\\.results\\.csv"
)[[1]][, 2]

# The right way to to it?
if(FALSE) {
	response <- GET("https://api.github.com/repos/Cervus1983/sumodb/git/trees/2c2c8d5aa51697aa701bd63ed72d13c9294100a2")
	if(response$status_code == 200) tree <- content(response)$tree

	path <- sapply(tree, function(x) x$path)

	basho <- str_match(path, "([0-9]{4}\\.[0-9]{2})\\.results\\.csv")[, 2]
	basho <- basho[!is.na(basho)]
}

year <- as.integer(unique(substr(basho, 1, 4)))


ui <- fluidPage(
	titlePanel("Grand Sumo Tournament Matchups"),

	sidebarLayout(
		# input
		sidebarPanel(
			# http://stackoverflow.com/a/26785047/17216
			tags$head(tags$style("#plot { height: 80vh !important; }")),

			selectInput(
				inputId = "year",
				label =  "Year",
				choices = sort(year, decreasing = TRUE)
			),
			selectInput(
				inputId =  "month",
				label =  "Month",
				choices = c("January", "March", "May", "July", "September", "November")
			)
		),
		
		# output
		mainPanel(
			plotlyOutput("plot", width = "100%", height = "100%")
		)
	)
)


server <- function(input, output) {
	output$plot <- renderPlotly({
		# year (number) + month (string) = yyyy.mm
		basho <- paste(
			input$year,
			sprintf(
				"%02d",
				match(
					input$month,
					c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
				)
			),
			sep = "."
		)

		plot.basho(
			read.csv(
				paste0("https://raw.githubusercontent.com/Cervus1983/sumodb/master/CSV/", basho, ".results.csv")
			)
		)
	})
}


shinyApp(ui, server)
