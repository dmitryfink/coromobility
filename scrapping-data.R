rm(list = ls())

library(rvest)
library(dplyr)
library(purrr)
library(lubridate)
library(tidyverse)
library(stringr)

get_wiki_table <- function(html_page, table_no, headers_row){
	cases <- html_page %>% html_nodes("table") %>% .[[table_no]] %>%  html_table(fill = TRUE, header = F)
	headers <- if (headers_row == 1)
		Map(function(x) gsub('\\[[^\\]*\\]|[-. ]*','',x), cases[1,])
	else if (headers_row == 2)
		Map(function(x,y) gsub('\\[[^\\]*\\]|[-. ]*','',paste(x,y, sep = '_')), cases[1,], cases[2,])
	else if (headers_row == 3)
		Map(function(x,y,z) gsub('\\[[^\\]*\\]|[-. ]*','',paste(x,y,z, sep = '_')), cases[1,], cases[2,], cases[3,])
	
	d.cases <- cases %>% slice((headers_row+1):n() ) %>% set_names(headers)

	return (d.cases)
}

cases <- list()
## Starting with Russia

wp_page_url <- "https://ru.wikipedia.org/wiki/Хронология_распространения_COVID-19_в_России"
outbreak_webpage <- read_html(wp_page_url)

## Moscow

cases[['Moscow']] <- get_wiki_table(outbreak_webpage,4,3) %>% .[,c('Москва_ДАТА', 'Москва_смертей_новых')] %>% 
	set_names(c('date', 'deaths')) %>% mutate(date = as_date(strptime(date, '%d.%m.%Y')), deaths = as.numeric(deaths))%>% arrange(date) %>% as_tibble

## St Petersburg
cases[['Saint Petersburg - Russia']] <- get_wiki_table(outbreak_webpage,6,3) %>% .[,c('СанктПетербург_ДАТА', 'СанктПетербург_смертей_новых')] %>% 
	set_names(c('date', 'deaths')) %>% mutate(date = as_date(strptime(date, '%d.%m.%Y')), deaths = as.numeric(deaths)) %>% arrange(date)%>% as_tibble %>% replace_na(list(deaths=0))

# According to Russian authoritis the number of death from pneumonia is about twice the covid:
cases[['Saint Petersburg - Russia']]	<- cases[['Saint Petersburg - Russia']] %>% mutate(deaths = deaths * 3 )

## London
wp_page_url <- "https://en.wikipedia.org/wiki/2020_coronavirus_pandemic_in_London"
outbreak_webpage <- read_html(wp_page_url)
t.cases <- get_wiki_table(outbreak_webpage,2,1)
cases[['London']] <- t.cases %>% select(Date, Newdeaths) %>% mutate(date = as_date( str_c('2020-',Date) ), deaths = as.numeric(str_extract(Newdeaths, '^\\d* '))) %>% select(date, deaths) %>% arrange(date) %>% na.omit %>% as_tibble

# cases[['London']] %>% tail
new_data <- tibble(date=c('2020-04-13', '2020-04-14', '2020-04-15', '2020-04-16', '2020-04-17', '2020-04-18',  '2020-04-19', '2020-04-20',  '2020-04-21','2020-04-22', '2020-04-23', '2020-04-24', '2020-04-25', '2020-04-26', '2020-04-27', 
						'2020-04-28', '2020-04-29', '2020-04-30', '2020-05-01', '2020-05-02',  '2020-05-03', '2020-05-04','2020-05-05','2020-05-06','2020-05-07','2020-05-08','2020-05-09' ), 
		deaths = c(206, 153, 153,145,221, 82, 81, 132, 173, 89, 126, 116, 64, 87, 88, 109, 86, 71, 51, 58, 22, 53, 64, 132, 93, 23, 25) ) %>% mutate(date=as_date(date))

cases[['London']] <- cases[['London']] %>% bind_rows(new_data)
# cases[['London']] %>% tail

## NY
# wp_page_url <- 'https://en.wikipedia.org/wiki/2020_coronavirus_pandemic_in_New_York_City'

wp_page_url <- 'https://commons.wikimedia.org/wiki/Data:COVID-19_cases_in_New_York_City.tab'
outbreak_webpage <- read_html(wp_page_url)
t.cases <- get_wiki_table(outbreak_webpage,1,3)
cases[['New York City']] <- t.cases %>% rename(date = date_string_Date, deaths = deaths_per_day_number_Deathsperday) %>%
	select(date, deaths) %>% mutate(date=as_date(date), deaths = as.numeric(deaths)) %>% arrange(date) %>% as_tibble

# using the data from https://www1.nyc.gov/site/doh/covid/covid-19-data.page
t.cases <- read_csv(file = "data/data-4SfjZ.csv", col_types = list(DATE_OF_INTEREST=col_date(format='%m/%d/%y'))) %>% mutate(date = as_date(DATE_OF_INTEREST)) %>% rename(deaths = Deaths) %>% select(date, deaths)
cases[['New York City']] <- t.cases %>% arrange(date)
# cases[['New York City']] %>% tail

## Berlin

wp_page_url <- 'https://en.wikipedia.org/wiki/COVID-19_pandemic_in_Germany'
outbreak_webpage <- read_html(wp_page_url)
t.cases <- get_wiki_table(outbreak_webpage,3,3)

cases[['Berlin']] <- t.cases %>% rename(date = Date_Date_Date, deaths = States_Berlin_) %>% select(date, deaths) %>% filter(str_detect(date, '^\\d')) %>% 
	mutate(deaths = as.numeric(str_replace_all(str_extract(deaths, '\\(.*\\)'),'[)(]','')), date = paste(date, '2020')) %>% 
	mutate(date = str_replace(date, 'May', '05')) %>% mutate(date = str_replace(date, 'Apr', '04'))%>% mutate(date = str_replace(date, 'Mar', '03')) %>%  mutate(date = str_replace(date, 'Feb', '02')) %>% 
	mutate(date=as_date(strptime(date, '%d %m %y')))  %>% arrange(date) %>% 
	mutate(deaths = c(0,diff(deaths)))

##paris
## https://github.com/opencovid19-fr/data
wp_page_url <- "https://raw.githubusercontent.com/opencovid19-fr/data/master/dist/chiffres-cles.csv"
t.cases <- read_csv(wp_page_url, col_types = cols(cas_ehpad =col_double(),cas_confirmes_ehpad=col_double(), deces_ehpad = col_double(), depistes=col_double(), cas_possibles_ehpad = col_double()))
cases[['Paris']] <- t.cases %>%  filter(str_detect(maille_nom, pattern = regex('paris', ignore_case = T))) %>% 
  select(date, cas_confirmes, deces, deces_ehpad) %>% mutate(deaths = c(0,diff(deces))) %>% arrange(date) %>%   select(date, deaths) 

## madrid
wp_page_url <- 'https://catalegdades.caib.cat/api/views/epkn-vp5r/rows.csv?accessTypeuu=DOWNLOAD'
t.cases <- read_csv(wp_page_url)
cases[['Madrid']] <- t.cases %>% rename(region = !!"Comunitat Autònoma", deaths=Morts, date=Data) %>% filter(region=='MD') %>% select(date, deaths) %>% 
	mutate(date = as_date(strptime(date, '%m/%d/%y'))) %>% na.omit %>% mutate(deaths = c(0,diff(deaths)))

# wp_page_url <- "https://www.socialstyrelsen.se/globalassets/sharepoint-dokument/dokument-webb/statistik/statistik-covid19-avlidna-20200508.xlsx"
# t.cases <- readxl::read_xlsx(wp_page_url)

# Using library(COVID19)
# 
# library(COVID19)
# library(dplyr)
# library(readr)
# library(ggplot2)
# 
# all_res_reg <- covid19(level=2)
# 
# all_res_reg %>% filter(id == 'ITA, Lombardia') %>% tail
# all_res_reg %>% filter(id == 'ITA, Lombardia') %>% tail
# 
# all_res_reg %>% select(id) %>% unique %>% View
# 
# all_res <- covid19(c("ITA"), level = 3)
# all_res %>% filter(deaths != 0)

# 
# all_res %>% select(id) %>% unique %>% filter(str_detect(id, 'USA', negate = T)) %>%  View
# 
# all_res %>% filter(grepl('UK', id)) %>% select(id, deaths, confirmed)
# 
# View( all_res %>% filter(id == 'UK, LO')  )
# 	ggplot(aes(x=date, y=deaths)) + geom_point()

#https://www.statista.com/statistics/1105401/coronavirus-covid-19-cases-cities-districts-germany/

map(cases, ~tail(.x))
# cases[['Berlin']] <- cases[['Berlin']] %>% mutate(deaths = stats::filter(deaths, sides=1, filter = rep(1/4,4)))


amob <- read_csv('data/applemobilitytrends-2020-05-08.csv') 

cases1 <- list()
for (ncase in names(cases)){
	mob.data <- amob %>% filter(region == ncase) %>% select(-geo_type, -region, -alternative_name) %>% 
		pivot_longer(cols= starts_with('20')) %>% rename(date = name) %>% mutate(date=as_date(date)) %>% 
		pivot_wider(names_from  = 'transportation_type') %>% arrange(date) %>% mutate_if(is.numeric, function(x)x/100)
	
	start.values <- mob.data %>% filter(date=='2020-03-01') %>% select(-date) %>% as.matrix()
	mob.data[2:ncol(mob.data)] <- sweep(mob.data[2:ncol(mob.data)], 2, start.values, '/')
	cases1[[ncase]] <- right_join(cases[[ncase]], mob.data) %>% replace_na(list(deaths=0))
}

mob.data %>% filter(date=='2020-03-01')

cases1[map_dbl(cases1, ~ nrow(.x) ) == 0] <- NULL
print(map_dbl(cases1, ~ nrow(.x) ))


save(cases1, cases, file = 'data/city_mobility.rdata')
# load(file = 'data/city_mobility.rdata')

# map(names(cases1), ~cases1[[.x]] %>% mutate(city=.x) %>% select(-deaths) ) %>% bind_rows() %>% select(-transit) %>%
#   ggplot(aes(x=date, col=city))+geom_line(aes(y=driving)) + theme_bw()+ theme(panel.grid.minor = element_blank(), legend.position="bottom") + scale_y_continuous(breaks=seq(0, 1.5, by=0.1))


