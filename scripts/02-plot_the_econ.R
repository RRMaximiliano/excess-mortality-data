
# Packages ----------------------------------------------------------------

library(here)
library(tidyverse)
library(hrbrthemes)
library(glue)
library(ggtext)
library(ggrepel)
library(lubridate)

# Read data ---------------------------------------------------------------

df <- read_csv("https://raw.githubusercontent.com/TheEconomist/covid-19-the-economist-global-excess-deaths-model/main/output-data/export_country_per_100k_cumulative.csv")

# Months and CA df --------------------------------------------------------

months <- tibble(
	yday = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
	label = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
	)

df_ca <- df %>% 
	filter(
		iso3c %in% c("NIC", "SLV", "HND", "CRI")
	) %>% 
	arrange(iso3c, date) %>% 
	mutate(
		year = year(date),
		yday = yday(date),
		year = as_factor(year)
	) %>% 
	select(iso3c, year, yday, date, cumulative_estimated_daily_excess_deaths_per_100k, cumulative_daily_covid_deaths_per_100k) %>% 
	rename(
		excess = 5,
		covid = 6
	)

# Plot --------------------------------------------------------------------

jetbrains <- "JetBrains Mono"
	
# Languages list
languages <- list(
	en_US = list(
		x = NULL,
		y = "Cumulative excess deaths per 100,000 people",
		title = "Cumulative excess deaths per 100,000 people in selected countries in <span style = 'color:grey60;'>2020</span> and <span style = 'color:#6141D9;'>2021</span>",
		subtitle = "Excess deaths is defined as the gap between the total number of deaths that occur for any reason and the amount that would be expected under normal circumstances",
		caption = "Data: The Economist, github.com/TheEconomist/covid-19-the-economist-global-excess-deaths-model\nViz: @rrmaximiliano"
	),
	es_ES = list(
		x = NULL,
		y = "Exceso de muertes acumuladas por cada 100.000 personas",
		title = "Exceso acumulativo de muertes por cada 100.000 personas en países seleccionados en <span style = 'color:grey60;'>2020</span> y <span style = 'color:#6141D9;'>2021</span>",
		subtitle = "El exceso de muertes se define como la brecha entre el número total de muertes que ocurren por cualquier motivo y la cantidad que se esperaría en circunstancias normales",
		caption = "Datos: The Economist, github.com/TheEconomist/covid-19-the-economist-global-excess-deaths-model\nViz: @rrmaximiliano"
	)
)

# Plot each language
for (l in names(languages)) {
	message(l)
	current <- languages[[l]]
	
	if(l == "es_ES") {
		months <- months %>% 
			mutate(
				label = case_when(
					label == "Jan" ~ "Ene",
					label == "Apr" ~ "Abr",
					label == "Aug" ~ "Ago",
					TRUE ~ as.character(label)
				)
			)
	}
	
	fig <- df_ca %>% 
		group_by(year) %>% 
		filter(iso3c %in% c("CRI", "NIC", "SLV")) %>%
		mutate(
			name = case_when(iso3c == "CRI" ~ "Costa Rica", 
											 iso3c == "NIC" ~ "Nicaragua", 
											 iso3c == "SLV" ~ "El Salvador"),
			lbl = ifelse(yday == last(yday), round(excess,2), NA)
		) %>% 
		ggplot(aes(x = yday, y = excess, color = year, label = lbl)) +
		geom_hline(yintercept = 0, color = "black") + 
		geom_line(size = 1) + 
		geom_text_repel(
			nudge_x = 0.1, 
			direction = "x", 
			hjust = "right", 
			vjust = -0.15,
			segment.color = "black",
			family = jetbrains) +
		scale_x_continuous(breaks = months$yday, labels = months$label,expand = c(.001, .001)) +
		scale_y_continuous(position = "right") + 
		scale_color_manual(values = c("grey60", "#6141D9")) + 
		coord_cartesian(clip = "off") +
		facet_wrap(~ name) + 
		labs(
			title = current$title, 
			subtitle = current$subtitle, 
			caption = current$caption, 
			x = current$x, 
			y = current$y
		) + 
		theme_ipsum_rc() +
		theme(
			plot.title = element_markdown(size = rel(2)),
			plot.subtitle = element_markdown(size = rel(1.25)),
			plot.caption = element_text(size = rel(1.2), hjust = 0),
			axis.line.x = element_line(color = "black"),
			axis.line.y = element_blank(),
			axis.text.x = element_text(size = rel(1.1), family = jetbrains),
			axis.text.y.right = element_text(size = rel(1.2), family = jetbrains, vjust = -0.25, hjust = 1, margin = margin(l = -25, r = 10)),
			# axis.title.x = element_text(size = rel(1.4), hjust = 0.5),
			axis.title.y.right = element_text(size = rel(1.8), hjust = 1),
			axis.ticks.x = element_line(size = .5, color = "black"), 
			axis.ticks.length.x = unit(.25, "cm"),
			strip.text = element_text(size = rel(1.5), face = "bold", hjust = 0.5),
			panel.grid.minor.x = element_blank(),
			panel.grid.major.x = element_blank(),
			legend.position = "none",
			plot.margin = margin(20, 40, 20, 40),
			axis.line = element_line(colour = "grey70")
		)

	png <- paste0("fig_econ_", l, ".png")
	pdf <- paste0("fig_econ_", l, ".pdf")
	
	ggsave(
		fig, 
		filename = here::here("figs", png),
		dpi = 720, height = 8, width = 18, scale = 0.9
	) 
	
	ggsave(
		fig, 
		filename = here::here("figs", pdf),
		device = cairo_pdf,
		height = 8, width = 18, scale = 0.9
	)

}
