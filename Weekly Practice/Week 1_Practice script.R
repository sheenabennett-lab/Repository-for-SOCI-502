print("hello, world")
2 + 2
install.packages("tidyverse")
library("tidyverse")
install.packages("AustralianPoliticians")
library(AustralianPoliticians)

# Make a table of the counts of genders of the prime ministers
get_auspol("all") |> # Imports data from GitHub
  as_tibble() |>
  filter(wasPrimeMinister == 1) |>
  count(gender)
get_auspol("all") |> # Imports data from GitHub
  head()
# Make a table of the counts of genders of the prime ministers
get_auspol("all") |> # Imports data from GitHub
  as_tibble() |>
  filter(wasPrimeMinister == 1) |>
  count(gender)
library(tidyverse)
library(AustralianPoliticians)

australian_politicians <-
  get_auspol("all")

head(australian_politicians)
install.packages("AustralianPoliticians")
library(AustralianPoliticians)
australian_politicians <-
  get_auspol("all")
head(australian_politicians)
australian_politicians |>
  select(firstName)

australian_politicians$firstName |>
  head()
australian_politicians |>
  select(firstName) |>
  pull() |>
  head()
australian_politicians |>
  select(-firstName)
australian_politicians |>
  select(starts_with("birth"))
australian_politicians <-
  australian_politicians |>
  select(
    uniqueID,
    surname,
    firstName,
    gender,
    birthDate,
    birthYear,
    deathDate,
    member,
    senator,
    wasPrimeMinister
  )

australian_politicians
austrailian_politicians |>
  filter(wasPrimeMinister == 1)

australian_politicians |>
  filter(wasPrimeMinister == 1 & firstName == "Joseph")
australian_politicians |>
  filter(wasPrimeMinister == 1, firstName == "Joseph")
australian_politicians |>
  filter(firstName == "Myles" | firstName == "Ruth")
australian_politicians |>
  filter(firstName == "Ruth" | firstName == "Myles") |>
  select(firstName, surname)
australian_politicians |>
  filter(row_number() == 853)
australian_politicians |>
  slice(853)

australian_politicians |>
  slice(-1)
australian_politicians |>
  slice(1:3)
australian_politicians |>
  slice(1:2, 1:n())
australian_politicians |>
  arrange(birthYear)
australian_politicians |>
  arrange(desc(birthYear))
australian_politicians |>
  arrange(-birthYear)


australian_politicians |>
  arrange(firstName, birthYear)
australian_politicians |>
  arrange(birthYear) |>
  arrange(firstName)
australian_politicians |>
  arrange(birthYear, firstName)
australian_politicians |>
  arrange(across(c(firstName, birthYear)))

australian_politicians |>
  arrange(across(starts_with("birth")))

australian_politicians <-
  australian_politicians |>
  mutate(was_both = if_else(member == 1 & senator == 1, 1, 0))

australian_politicians |>
  select(member, senator, was_both)
australian_politicians <-
  australian_politicians |>
  mutate(was_both = if_else(member == 1 & senator == 1, 1, 0))

australian_politicians |>
  select(member, senator, was_both)

australian_politicians <-
  australian_politicians |>
  mutate(was_both = if_else(member == 1 & senator == 1, 1, 0))

australian_politicians |>
  select(member, senator, was_both)
library(lubridate)
australian_politicians <-
  australian_politicians |>
  mutate(age = 2022 - year(birthDate))
australian_politicians |>
  select(uniqueID, age)
australian_politicians |>
  select(uniqueID, age) |>
  mutate(log_age = log(age))
australian_politicians |>
  select(uniqueID, age) |>
  mutate(lead_age = lead(age))
australian_politicians |>
  select(uniqueID, age) |>
  mutate(lag_age = lag(age))
australian_politicians |>
  select(uniqueID, age) |>
  drop_na(age) |>
  mutate(cumulative_age = cumsum(age))
australian_politicians |>
  mutate(across(c(firstName, surname), str_count)) |>
  select(uniqueID, firstName, surname)
library(lubridate)

australian_politicians |>
  mutate(
    year_of_birth = year(birthDate),
    decade_of_birth =
      case_when(
        year_of_birth <= 1929 ~ "pre-1930",
        year_of_birth <= 1939 ~ "1930s",
        year_of_birth <= 1949 ~ "1940s",
        year_of_birth <= 1959 ~ "1950s",
        year_of_birth <= 1969 ~ "1960s",
        year_of_birth <= 1979 ~ "1970s",
        year_of_birth <= 1989 ~ "1980s",
        year_of_birth <= 1999 ~ "1990s",
        TRUE ~ "Unknown or error"
      )
  ) |>
  select(uniqueID, year_of_birth, decade_of_birth)

australian_politicians |>
  summarise(
    youngest = min(age, na.rm = TRUE),
    oldest = max(age, na.rm = TRUE),
    average = mean(age, na.rm = TRUE)
  )
australian_politicians |>
  summarize(
    youngest = min(age, na.rm = TRUE),
    oldest = max(age, na.rm = TRUE),
    average = mean(age, na.rm = TRUE)
  )

australian_politicians |>
  summarise(
    youngest = min(age, na.rm = TRUE),
    oldest = max(age, na.rm = TRUE),
    average = mean(age, na.rm = TRUE),
    .by = gender
  )
australian_politicians |>
  mutate(days_lived = deathDate - birthDate) |>
  drop_na(days_lived) |>
  summarise(
    min_days = min(days_lived),
    mean_days = mean(days_lived) |> round(),
    max_days = max(days_lived),
    .by = gender
  )
australian_politicians |>
  mutate(days_lived = deathDate - birthDate) |>
  drop_na(days_lived) |>
  summarise(
    min_days = min(days_lived),
    mean_days = mean(days_lived) |> round(),
    max_days = max(days_lived),
    .by = c(gender, member)
  )
australian_politicians |>
  count(gender)
australian_politicians |>
  count(gender) |>
  mutate(proportion = n / (sum(n)))
australian_politicians |>
  summarise(n = n(),
            .by = gender)
australian_politicians |>
  add_count(gender) |>
  select(uniqueID, gender, n)
my_name <- "Sheena"
