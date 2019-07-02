library(readr)
library(ggplot2)
library(scales)
library(shiny)
library(dplyr)
library(tidyr)
library(ggthemes)
library(plotly)

# A function to get the most recent year avaliable from WB data
get_most_recent <- function(dt){
  dt <- dt %>% 
    select(-one_of(c("Country Code", "Indicator Name", "Indicator Code")), Country='Country Name') %>% 
    gather("Year", "Value", -"Country") %>% 
    na.omit() %>% 
    group_by(Country) %>% 
    filter(Year == max(Year)) %>% 
    ungroup() %>% 
    right_join(select(dt, Country='Country Name'), by='Country') 
  dt$Value = as.numeric(dt$Value)
  dt
}

###### import and clean the WB data #######
get_dat <- function(){
  population <- read_csv("data/population_world_bank.csv", skip=4)
  income <- read_csv("data/income_world_bank.csv", skip=4) #GDP per capita, PPP (current international $)
  gini <- read_csv("data/gini_world_bank.csv", skip=4) #GINI index (World Bank estimate)
  rows_to_remove <- read_csv("data/rows_to_remove.csv")$to_remove
  
  # first get the most recent avaliable year
  population <- get_most_recent(population)
  income <- get_most_recent(income)
  gini <- get_most_recent(gini)
  
  # now combine the data step by step
  dat <- population %>% 
    select(country = 'Country', population = 'Value')
  dat <- income %>% 
    select(country = 'Country', income = 'Value') %>% 
    inner_join(dat)
  dat <- gini %>% 
    select(country = 'Country', gini = 'Value') %>% 
    inner_join(dat)
  
  # filter out the world bank summary rows (i.e. World total, Central Asia total)
  dat <- filter(dat, !(country %in% rows_to_remove))
  # 217 counties
  
  # drop countries with NA income ~ 1% of global population
  # (sum(dat$population) - sum(filter(dat, !is.na(income))$population)) / sum(dat$population)
  dat <- dat %>%
    filter(!is.na(income))
  # 194 countries
  
  # there are 33/194 countries without gini data
  # replace the na's with the median of the knowns
  med_gini <- median(dat$gini[!is.na(dat$gini)])
  dat$gini[is.na(dat$gini)] <- med_gini
  dat$gini <- dat$gini/100
  
  dat
}

# see the paper below
# http://www.cgeh.nl/sites/default/files/WorkingPapers/CGEH.WP_.No1_.Vanzandenetal.jan2011_0.pdf
lorenz_cdf <- function(p, gini){
  sigma <- sqrt(2)*qnorm((1+gini)/2)
  pnorm(qnorm(p) - sigma)
}

# get an income from a country
get_inc <- function(nation, n=1){
  inc <- filter(dat, country == nation)$income
  pop <- filter(dat, country == nation)$population
  g <- filter(dat, country == nation)$gini
  
  total_inc <- inc*pop # the country's total income
  person_pct <- 1/pop # percent of total pop each person is
  U <- runif(n)
  while(sum(U > person_pct) < n){
    U[U <= person_pct] <- runif(1)
  }
  # the difference btwn the percentile sampled and one person down; this is percent of total income for one person
  (lorenz_cdf(U, g) - lorenz_cdf(U - 1/pop, g)) * total_inc 
}

# final function
get_person <- function(){
  nation <- sample_n(dat, size=1, weight=population)$country
  inc <- get_inc(nation)
  list(nation, inc)
}

########## Now call the functions to generate the plot #########

# Call the function that generates the data
dat <- get_dat()


##### Note: these lines before the full pound line can be commented out after they are run once ######
# Get a dataset of 50,000 points to draw from
save_dat <- data.frame(matrix(nrow=50000, ncol=1))
colnames(save_dat5) <- c("income")
for(i in 1:50000){
  save_dat[i,1] <- get_person()[2]
}

# Save the 50k as csv
write_csv(save_dat, "fifty-k_points.csv")

# Save the 50k as an RDS for even faster reads 
plot_dat <-
  read_csv("fifty-k_points.csv") %>%
  arrange(income)
saveRDS(plot_dat, "global_50k_dat.Rdata")
#################################################################################################################

# Create the vert line
vline <- function(x = 0, color = 'rgb(10, 10, 10)') {
  list(
    type = "line", 
    y0 = 0, 
    y1 = 1, 
    yref = "paper",
    x0 = x, 
    x1 = x, 
    line = list(color = color, dash = 'dashdot')
  )
}

# Read-in the plot data
plot_dat <- readRDS("global_50k_dat.Rdata")

# Create the distribution and labels
dsty <- density(plot_dat$income, adjust=0.001, n=1024)
final_dist_samp <- plot_dat[seq(1, nrow(plot_dat), nrow(plot_dat)/100),]
dat_dnsty <- approx(dsty$x, dsty$y, final_dist_samp$income)
dat_dnsty$quant <- c(0:99)
dat_dnsty$text <- paste0(dollar_format()(round(dat_dnsty$x, -2)), "\n", dat_dnsty$quant, "%tile income")

# Generate the plot
m <- list(
  l = 50,
  r = 150,
  b = 50,
  t = 50,
  pad = 5
)

global_plot <- plot_ly(x = ~dat_dnsty$x, y = ~dat_dnsty$y, text = ~dat_dnsty$text,
                        hoverinfo = "text", type = 'scatter', mode = 'lines', fill = 'tozeroy') %>%
                layout(title = "Global Number of People by Income",
                        xaxis = list(title = 'Annual Income (PPP Adjusted to US$)', range=c(0,70000)),
                        yaxis = list(title = 'People', showticklabels = FALSE, fixedrange=TRUE),
                        margin = m)

# Generate the plot for mobile devices
global_plot.mobile <- ggplot(plot_dat, aes(x=income)) + geom_density(color = 'white', fill='#66a3ff') +
  theme_hc() +
  xlab('Annual Income (PPP Adjusted to US$)') +
  ylab('People') +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
  scale_x_continuous(labels = dollar, limits = c(0,90000))

# A function that is used to determine whether or not the app was opened on a mobile device
mobileDetect <- function(inputId, value = 0) {
  tagList(
    singleton(tags$head(tags$script(src = "js/mobile.js"))),
    tags$input(id = inputId,
               class = "mobile-element",
               type = "hidden")
  )
}
