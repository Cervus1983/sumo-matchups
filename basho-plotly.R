library(plotly)

# level.rank.group
as.rank <- function(s) sprintf(
	"%s.%02d.%s",
	match(substr(s, 1, 1), c("Y", "O", "S", "K", "M", "J")),
	as.integer(str_match(s, "^\\D([0-9]+)\\D")[, 2]),
	match(substr(s, nchar(s), nchar(s)), c("e", "w"))
)

# plot one tournament (basho)
plot.basho <- function(df) {
	df$index <- 1:nrow(df)
	
	#df <- df %>% mutate(
	#	color1 = ifelse(win1 == 1, "red", "black"),
	#	color2 = ifelse(win2 == 1, "red", "black")
	#)

	# relative order of all ranks found in {df}
	scale <- do.call(
		rbind,
		lapply(
			unique(c(df$rank1, df$rank2)),
			function(x) data.frame(rank = x, code = as.rank(x))
		)
	) %>% arrange(code)
	
	scale$y <- 1:nrow(scale)
	
	# map ranks to y-values
	df <- merge(
		merge(
			df,
			scale %>% rename(rank1 = rank)
		) %>% select(-code, y1 = y),
		scale %>% rename(rank2 = rank)
	) %>% select(-code, y2 = y)
	
	# one subplot for each day of the tournament
	days <- unique(df$day)
	days <- days[order(days)]
	
	rows <- ceiling(length(days) / 5)
	cols <- ceiling(length(days) / rows)

	p <- list()
	
	for(d in days) {
		# https://plot.ly/r/dumbbell-plots/
		p[[d]] <- df %>% filter(day == d) %>% plot_ly(
			color = I("gray80")
		) %>% add_segments(
			x = ~index, xend = ~index,
			y = ~y1, yend = ~y2,
			hoverinfo = "none",
			line = list(width = 1),
			showlegend = FALSE
		) %>% add_markers(
			x = ~index, y = ~y1,
			color = I("black"),
			hoverinfo = "text",
			marker = list(size = ~win1 * 2 + 2),
			text = ~paste(shikona1, rank1)
		) %>% add_markers(
			x = ~index, y = ~y2,
			color = I("black"),
			hoverinfo = "text",
			marker = list(size = ~win2 * 2 + 2),
			text = ~paste(shikona2, rank2)
		) %>% layout(
			hovermode = "x",
			showlegend = FALSE,
			xaxis = list(
				range = c(min((df %>% filter(day == d))$index) - 0.5, max((df %>% filter(day == d))$index) + 0.5),
				showgrid = FALSE,
				showticklabels = FALSE,
				title = paste("day", d)
			),
			yaxis = list(
				autorange = "reversed",
				showgrid = FALSE,
				showticklabels = FALSE,
				title = "",
				zeroline = FALSE
			)
		)
	}

	
	subplot(p, nrows = rows, shareY = TRUE) %>% layout(annotations = list(
		showarrow = FALSE,
		text = "First day",
		x = 1,
		y = 1
	))
}
