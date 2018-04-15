library(plotly)
library(stringr)
library(tidyverse)

# level.rank.group
as.rank <- function(s) as.integer(sprintf(
	"%d%02d%d",
	match(
		str_extract(s, "^\\D+"),
		c("Y", "O", "S", "K", "M", "Bg", "J", "Ms", "Sd", "Jd", "Jk")
	),
	as.integer(str_match(s, "^\\D(\\d+)")[, 2]),
	match(str_match(s, "^\\D+\\d+([ew])")[, 2], c("e", "w"))
))

# plot one basho (tournament)
plot.basho <- function(df) {
	df$index <- 1:nrow(df)
	
	# relative order of all ranks found in {df}
	scale <- do.call(
		rbind,
		lapply(
			unique(c(df$rikishi1_rank, df$rikishi2_rank)),
			function(x) data.frame(rank = x, code = as.rank(x))
		)
	) %>% arrange(code)
	
	scale$y <- 1:nrow(scale)
	
	# map ranks to y-values
	df <- merge(
		merge(
			df,
			scale %>% rename(rikishi1_rank = rank)
		) %>% select(-code, y1 = y),
		scale %>% rename(rikishi2_rank = rank)
	) %>% select(-code, y2 = y)
	
	# one subplot for each day of the tournament
	days <- df$day %>% unique() %>% sort()
	
	rows <- ceiling(length(days) / 5)
	cols <- ceiling(length(days) / rows)

	p <- list()
	
	for(d in days) {
		# https://plot.ly/r/dumbbell-plots/
		p[[d]] <- df %>% 
			filter(day == d) %>% 
			plot_ly(
				color = I("gray80")
			) %>% 
			add_segments(
				x = ~index, xend = ~index,
				y = ~y1, yend = ~y2,
				hoverinfo = "none",
				line = list(width = 1),
				showlegend = FALSE
			) %>% 
			add_markers(
				x = ~index, y = ~y1,
				color = I("black"),
				hoverinfo = "text",
				marker = list(size = ~rikishi1_win * 2 + 2),
				text = ~paste(rikishi1_shikona, rikishi1_rank)
			) %>% 
			add_markers(
				x = ~index, y = ~y2,
				color = I("black"),
				hoverinfo = "text",
				marker = list(size = ~rikishi2_win * 2 + 2),
				text = ~paste(rikishi2_shikona, rikishi2_rank)
			) %>% 
			layout(
				hovermode = "x",
				showlegend = FALSE,
				xaxis = list(
					fixedrange = TRUE,
					range = c(min((df %>% filter(day == d))$index) - 0.5, max((df %>% filter(day == d))$index) + 0.5),
					showgrid = FALSE,
					showticklabels = FALSE
				),
				yaxis = list(
					autorange = "reversed",
					fixedrange = TRUE,
					showgrid = FALSE,
					showticklabels = FALSE,
					title = "",
					zeroline = FALSE
				)
			) %>% 
			plotly::config(
				collaborate = FALSE,
				displaylogo = FALSE,
				modeBarButtonsToRemove = c(
					"autoScale2d",
					"hoverClosestCartesian",
					"hoverCompareCartesian",
					"lasso2d",
					"select2d",
					"toggleSpikelines",
					"zoomIn2d",
					"zoomOut2d"
				)
			)
	}

	subplot(p, nrows = rows, shareY = TRUE) %>% 
		layout(
			annotations = list(
				list(
					showarrow = FALSE,
					text = "First day",
					x = 1, xanchor = "left",
					y = 1
				)
			)
		)
}
