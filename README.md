# Tidyverse.Introdu-o
Manipulando a base dslabs (sobre homicídios nos EUA) no R usando a biblioteca Tidyverse

library(tidyverse)

library(dslabs)

library(dplyr)

data("murders")

head(murders)

murders = mutate(murders, rate = total/population*10000)

new_table = select(murders, state, region, rate)

filter(new_table, rate <= 0.71)

murders = mutate(murders, rank = rank(-rate))

filter(murders, state == "Vermont")

select(murders, state, population) |> head()

filter(murders, rank >= 46)

no_florida = filter(murders, state != "Florida")

no_south = filter(murders, region != "South")

nrow(no_south)

only_NewYork_Texas = filter(murders, state %in% c("New York", "Texas"))

nrow(only_NewYork_Texas)

West = filter(murders, region %in% c("West"))

nrow(West)

specific_sample = filter(murders, rate < 1 & region %in% c("West", "Northeast"))

my_states = select(specific_sample, state, rate, rank)

murders |> select(state, region, rate) |> filter(rate <= 0.71)

my_states2 = murders |> filter(rate < 1 & region %in% c("West",
                                                        "Northeast")) |> select(state, rate, rank)
identical(my_states, my_states2)

data(heights)

head(heights)

statistc_females = heights |> filter(sex == "Female") |> summarize(avarage = mean(height),
                                                                   standard_desviation = sd(height))
us_murders_rate = murders |> summarize(rate = sum(total)/sum(population)*100000) |> print()

heights |> 
  filter(sex == "Female") |>
  summarize(median_min_max = quantile(height, c(0.5, 0, 1))) |> print()


heights %>%
  filter(sex == "Female") %>%
  summarize(median = quantile(height, 0.5),
            min_height = quantile(height, 0),
            max_height = quantile(height, 1)) %>%
  print()

heights |> group_by(sex) |> summarize(average = mean(height),
                                      standard_deviation = sd(height))
class(us_murders_rate)

us_murders_rate |> pull(rate) |> class()

murders |> arrange(population) |> head()

murders |> arrange(desc(population)) |> head()

murders |> arrange(region, rate) |> head()

murders |> top_n(3, rate)

mean(na_example, na.rm = TRUE)

murders |> group_by(region)

murders |> group_by(region) |> class()

murders |> print()

as.tibble(murders) |> print()

# tem tem as_tibble

2 |> log(8, base = _)

x = c(-2, -1, 0, 1, 2)

case_when(x < 0 ~ "Negative", 
          x > 0 ~ "Positive", 
          TRUE  ~ "Zero")

murders |> 
  mutate(group = case_when(
    abb %in% c("ME", "NH", "VT", "MA", "RI", "CT") ~ "New England",
    abb %in% c("WA", "OR", "CA") ~ "West Coast",
    region == "South" ~ "South",
    TRUE ~ "Other")) |>
  group_by(group) |>
  summarize(rate = sum(total) / sum(population) * 10^5) 

a = 1
b = 3
x = 2
y = 4

between(x, a, b)

between(y, a, b)

murders_tibble = as.tibble(murders) |> group_by(region) 

print(murders_tibble)

summarize(murders_tibble)

exp(mean(log(murders$population)))

murders |> pull(population) |> log() |> mean() |> exp()

result_df = tibble(n = 1:100) |>
  mutate(
    s_n = map_dbl(n, ~sum(1:.x)),
    s_n_2 = map_dbl(n, ~sum((1:.x^2))
                    )
  ) |> print()

Segue uma das tabelas geradas, para fins de ilustração

> print(murders_tibble)
# A tibble: 51 × 7
# Groups:   region [4]
   state                abb   region    population total  rate  rank
   <chr>                <chr> <fct>          <dbl> <dbl> <dbl> <dbl>
 1 Alabama              AL    South        4779736   135 0.282    23
 2 Alaska               AK    West          710231    19 0.268    27
 3 Arizona              AZ    West         6392017   232 0.363    10
 4 Arkansas             AR    South        2915918    93 0.319    17
 5 California           CA    West        37253956  1257 0.337    14
 6 Colorado             CO    West         5029196    65 0.129    38
 7 Connecticut          CT    Northeast    3574097    97 0.271    25
 8 Delaware             DE    South         897934    38 0.423     6
 9 District of Columbia DC    South         601723    99 1.65      1
10 Florida              FL    South       19687653   669 0.340    13
