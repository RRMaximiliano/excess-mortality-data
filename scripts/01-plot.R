
# Packages ----------------------------------------------------------------

library(here)
library(tidyverse)
library(hrbrthemes)
library(glue)
library(ggtext)
extrafont::loadfonts(device = "win", quiet = TRUE)

# Load data ---------------------------------------------------------------

mortality <- read.csv("https://raw.githubusercontent.com/Financial-Times/coronavirus-excess-mortality-data/master/data/ft_excess_deaths.csv") 

ca <- mortality %>% 
	filter(
		country %in% c("Nicaragua", "Costa Rica", "Belize", "Guatemala", "Honduras", "El Salvador", "Panama")
	)

# Plot --------------------------------------------------------------------

# Languages list
languages <- list(
	en_US = list(
		x = "Month of the year",
		y = "Total deaths",
		title = "Number of deaths per month from all causes", 
		subtitle = "Grey lines represent the total deaths in <span style = 'color:grey70;'>2015-2019</span>. Red line represents the total deaths in <span style = 'color:#b11226;'>2020</span>. Data for Nicaragua are up to August 2020.<br>Dotted line represents March.",
		caption = "Source: Financial Times analysis on mortality data, github.com/Financial-Times/coronavirus-excess-mortality-data\nPlot: @rrmaximiliano",
		excess = "Excess deaths"
	),
	es_ES = list(
		x = "Mes del año",
		y = "Total de muertes",
		title = "Número de muertes por mes por todas las causas",
		subtitle = "Las líneas grises representan el total de muertes en <span style = 'color:grey70;'>2015-2019</span>. La línea roja representa el total de muertes en <span style = 'color:#b11226;'>2020</span>.<br>Los datos para Nicaragua son hasta agosto de 2020. La línea punteada representa el mes de marzo.",
		caption = "Fuente: Análisis de datos de mortalidad del Financial Times, github.com/Financial-Times/coronavirus-excess-mortality-data\nGráfico: @rrmaximiliano",
		excess = "Exceso de muertes"
	)
)
	
# Plot each language
for (l in names(languages)) {
	message(l)
	current <- languages[[l]]
	
	fig <- ca %>% 
		filter(period == "month") %>% 
		mutate(
			yr_ind = year %in% 2020,
			round = round(total_excess_deaths_pct), 
			sub_ex = current$excess,
			name = glue("{country}\n{sub_ex}: +{round}%")
		) %>%
		ggplot(aes(x = month, y = deaths, color = yr_ind, group = year)) + 
		geom_vline(
			aes(xintercept = 3), 
			color = "grey40",
			linetype = "dotted"
		) +
		geom_line(size = 1) + 
		facet_wrap(~ name)  + 
		scale_x_continuous(limits = c(1,12), breaks = c(2, 4, 6, 8, 10, 12)) + 
		scale_y_comma() + 
		scale_color_manual(values = c("grey70", "#b11226")) + 
		labs(
			title = current$title, 
			subtitle = current$subtitle, 
			caption = current$caption, 
			x = current$x, 
			y = current$y
		) + 
		theme_ipsum_rc() +
		theme(
			plot.title = element_text(size = rel(2.25)),
			plot.subtitle = element_markdown(size = rel(1.25)),
			plot.caption = element_text(size = rel(1), hjust = 0),
			axis.text.x = element_text(size = rel(1.5)),
			axis.title.x = element_text(size = rel(1.5), hjust = 0.5),
			axis.text.y = element_text(size = rel(1.5)),
			axis.title.y = element_text(size = rel(1.5), hjust = 0.5),
			strip.text = element_text(size = rel(1.5), face = "bold"),
			legend.position = "none",
			panel.grid.minor = element_blank(),
			plot.margin = margin(20, 40, 20, 40),
			axis.line = element_line(colour = "grey70")
		)
	
	png <- paste0("fig_", l, ".png")
	pdf <- paste0("fig_", l, ".pdf")

	ggsave(fig, 
				 filename = here::here("figs", png), 
				 dpi = 720, height = 8, width = 16, scale = 0.9) 
	
	ggsave(fig, 
				 filename = here::here("figs", pdf),
				 device = cairo_pdf,
				 height = 8, width = 16, scale = 0.9)
	
}

