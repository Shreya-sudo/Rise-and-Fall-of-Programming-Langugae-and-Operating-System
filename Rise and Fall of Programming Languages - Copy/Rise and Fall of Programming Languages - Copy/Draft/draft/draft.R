library(tidyverse)
library(plotly)
library(DT)
library(kableExtra)
library(knitr)
theme_set(theme_minimal())
yearly_tag <- read_csv("C:/Users/KIIT/Desktop/Data Analytics/Rise and Fall of Programming Languages - Copy/Rise and Fall of Programming Languages - Copy/Draft/draft/by_tag_year.csv")
kable(head(yearly_tag)) %>% 
  kable_styling()
# Add fraction column
yearly_tag <- 
  yearly_tag %>% 
  mutate(fraction = round(number/year_total, 4))

# Print the new table
kable(head(yearly_tag)) %>% 
  kable_styling()
# How have popular programming languages changed over time?

#{r echo=FALSE, fig.height=7, fig.width=10, message=FALSE, warning=FALSE, paged.print=FALSE}
# Get the six largest tags
programming_lang <- c("r", "python", "c#", "java", "JavaScript", "php", "c++", "ruby")
yearly_top <- 
  yearly_tag %>% 
  filter(tag %in% programming_lang)
d_ends <- 
  yearly_top %>% 
  group_by(tag) %>% 
  slice(n()) %>% 
  pull(fraction)
d_ends[1] <- 0.053
d_ends[6] <- 0.024
d_labels <- 
  yearly_top %>% 
  group_by(tag) %>% 
  slice(n()) %>% 
  pull(tag)
# Filter for the six largest tags
ggplot(yearly_top) +
  geom_line(aes(x = year, y = fraction, color = tag), size = 1.5, alpha = .8) +
  geom_point(aes(x = year, y = fraction, color = tag), size = 2) +
  scale_x_continuous(expand = c(0, 0), breaks = c(2008:2018)) +
  scale_y_continuous(labels = scales::percent, breaks = c(0, .025, .05, .075, .1, .125), sec.axis = sec_axis(~ ., breaks = d_ends, labels = d_labels)) +
  labs(title = "Fraction of total questions per year in Stack Overflow",
       subtitle = "for top programming languages",
       x = "", 
       y = "Fraction of total queries in the year") +
  theme(legend.position = "none")


yearly_tag %>% 
  group_by(year) %>% 
  summarise(year_total = first(year_total)) %>%
  filter(year <= 2017) %>% 
  ggplot() +
  geom_line(aes(year, year_total), color = "steelblue", size = 1.5, alpha = .5 ) +
  geom_point(aes(year, year_total), color = "steelblue", size = 1.5) +
  scale_x_continuous(breaks = c(2008:2017)) +
  labs(title = "Total number of questions in Stack overflow per year",
       x = "",
       y = "Num. of questions")



# Predicting the future popularity of programming languages


library(forecast)
library(sweep)
# Get tags for top programming languages
programming_lang <- c("r", "python", "c#", "java", "JavaScript", "php", "c++", "ruby")
# Create the dataset
yearly_nest <- 
yearly_tag  %>% 
  filter(tag %in% programming_lang) %>%
  arrange(tag, year) %>%
  select(tag, fraction) %>% 
  group_by(tag) %>% 
  nest(.key = deprecated()) %>% # nest it
  mutate(data.ts = map(.x    = data.tbl, #create ts object
                       .f    = ts,
                       start = 2008)
                       ) %>% 
  mutate(fit_ets = map(data.ts, ets)) %>%
  mutate(summary_ets = map(fit_ets, summary)) %>%
  mutate(mape_ets = map(summary_ets, 5)) %>% 
  mutate(fit.arima = map(data.ts, auto.arima)) %>%
  mutate(summary_arima = map(fit.arima, summary)) %>%
  mutate(mape_arima = map(summary_arima, 5)) %>% 
  
  mutate(final_model = if_else(as.numeric(mape_arima) <= as.numeric(mape_ets), fit.arima, fit_ets)) %>% 
  
  mutate(predict = map(final_model, forecast, h = 5)) %>% 
  mutate(sweep = map(predict, sw_sweep)) %>% 
  unnest(sweep) %>% 
  mutate(fraction = if_else(fraction < 0, 0, fraction))
table_a <- 
yearly_tag  %>% 
  filter(tag %in% programming_lang) %>%
  arrange(tag, year) %>%
  select(tag, fraction) %>% 
  group_by(tag) %>% 
  nest(.key = "data.tbl") %>% # nest it
  mutate(data.ts = map(.x    = data.tbl, #create ts object
                       .f    = ts,
                       start = 2008)
                       ) %>% 
  mutate(fit_ets = map(data.ts, ets)) %>%
  mutate(summary_ets = map(fit_ets, summary)) %>%
  mutate(mape_ets = map(summary_ets, 5)) %>% 
  mutate(fit.arima = map(data.ts, auto.arima)) %>%
  mutate(summary_arima = map(fit.arima, summary)) %>%
  mutate(mape_arima = map(summary_arima, 5)) %>% 
  select(tag, mape_arima, mape_ets) %>% 
  mutate(mape_arima = round(as.numeric(mape_arima), 2), 
         mape_ets = round(as.numeric(mape_ets), 2)) 


table_a %>% 
  kable() %>% 
  kable_styling()


#{r echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE}
filter(yearly_nest, key == "forecast") %>%
  top_n(15) %>% 
  kable() %>% 
  kable_styling()

#{r echo=FALSE, fig.height=7, fig.width=10, message=FALSE, warning=FALSE, paged.print=FALSE}
d_ends <- yearly_nest %>% 
  group_by(tag) %>% 
  slice(n()) %>% 
  pull(fraction)
d_ends[4] <- 0.005
d_labels <- yearly_nest %>% 
  group_by(tag) %>% 
  slice(n()) %>% 
  pull(tag)
# Create the plot 
yearly_nest %>% 
  ggplot() +
  theme_minimal() +
  geom_line(aes(x = index, y = fraction, color = tag), size = 1.5, alpha = .8) +
  geom_point(aes(x = index, y = fraction, color = tag, shape = key ), size = 2) +
  scale_x_continuous(expand = c(0, 0), breaks = c(2008:2024)) +
  geom_rect(data=data.frame(xmin = 2018, xmax = Inf, ymin = -Inf, ymax = Inf),
            aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="steelblue", alpha=0.2) +
  geom_text(aes(x = 2019, y = 0.15, label = "Prediction", fill = 1), nudge_x = 1.5, colour = "white", size = 5) +
  scale_y_continuous(labels = scales::percent,  sec.axis = sec_axis(~ ., breaks = d_ends, labels = d_labels)) +
  labs(title = "Predicting future fraction of total questions per year in Stack Overflow",
       subtitle = "for top programming languages",
       x = "", 
       y = "Fraction of total queries in the year") +
  theme(legend.position = "none") 


