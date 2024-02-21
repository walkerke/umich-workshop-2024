library(tidycensus)
library(tidyverse)

ages <- c(0:99, rep(100, 3))

male_vars <- paste0("PCT012", str_pad(3:105, 3, "left", "0"))
female_vars <- paste0("PCT012", 107:209)

names(male_vars) <- ages
names(female_vars) <- ages

all_vars <- c(male_vars, female_vars)

pull00 <- get_decennial(
  geography = "state",
  state = "MI",
  variables = all_vars,
  year = 2000
) %>%
  summarize(value = sum(value, na.rm = TRUE), 
            .by = variable) %>%
  mutate(year = "2000")

pull10 <- get_decennial(
  geography = "state",
  state = "MI",
  variables = all_vars,
  year = 2010
) %>%
  summarize(value = sum(value, na.rm = TRUE), 
            .by = variable) %>%
  mutate(year = "2010")

male_vars20 <- paste0("PCT12_", str_pad(3:105, 3, "left", "0"), "N")
female_vars20 <- paste0("PCT12_", 107:209, "N")

names(male_vars20) <- ages
names(female_vars20) <- ages

all_vars20 <- c(male_vars20, female_vars20)

pull20 <- get_decennial(
  geography = "state",
  state = "MI",
  variables = all_vars20,
  year = 2020,
  sumfile = "dhc"
) %>%
  summarize(value = sum(value, na.rm = TRUE), 
            .by = variable) %>%
  mutate(year = "2020")

all_years <- bind_rows(pull00, pull10, pull20)

ggplot(all_years, aes(x = as.numeric(variable), y = value, color = year,
                      group = year)) + 
  geom_line() + 
  theme_minimal() + 
  scale_color_brewer(palette = "Set1") + 
  scale_y_continuous(labels = scales::label_comma()) + 
  labs(y = "Population",
       x = "Single-year age",
       color = "Year",
       title = glue::glue("Age distributions in Michigan"))
