load("Data.RData")


# It may seem like I'm repeating myself a lot here and not following clean code principles,
# But R has this cool functionality where you can highlight just a small part of a script and run just that part
# So what breaking it down like this does is let me clean just one year at a time pretty easily
# And if i want to add a year I can add and just highlight that part
# For data collection I prefer the loop, but for deployment I like to be able to be more surgical

# I changed the code to dilter county data by state name. If i was a better person I would just add state names in the "data cleaning" script but it was all ready so I'm doing it here

County2020 <- County2020 %>%
  mutate(
    Statenames = trimws(sub(".*,\\s*", "", NAME))
  )

County2021 <- County2021 %>%
  mutate(
    Statenames = trimws(sub(".*,\\s*", "", NAME))
  )

County2022 <- County2022 %>%
  mutate(
    Statenames = trimws(sub(".*,\\s*", "", NAME))
  )

County2023 <- County2023 %>%
  mutate(
    Statenames = trimws(sub(".*,\\s*", "", NAME))
  )

County2024 <- County2024 %>%
  mutate(
    Statenames = trimws(sub(".*,\\s*", "", NAME))
  )


saveRDS(County2020, "County2020.rds")
saveRDS(County2021, "County2021.rds")
saveRDS(County2022, "County2022.rds")
saveRDS(County2023, "County2023.rds")
saveRDS(County2024, "County2024.rds")

#GitHub won't let me upload the large tract file output by data cleaning, so I made this script to break it down into small files

Tract2020_by_state <- Tract2020 %>%
  group_by(Statenames) %>%
  group_split()

dir.create("tract_by_state2020", showWarnings = FALSE)

Tract2020 %>%
  split(.$Statenames) %>%
  purrr::iwalk(~ {
    file_name <- paste0(
      "tract_by_state2020/Tract2020_",
      gsub(" ", "_", .y),
      ".rds"
    )
    saveRDS(.x, file_name)
  })

Tract2021_by_state <- Tract2021 %>%
  group_by(Statenames) %>%
  group_split()

dir.create("tract_by_state2021", showWarnings = FALSE)

Tract2021 %>%
  split(.$Statenames) %>%
  purrr::iwalk(~ {
    file_name <- paste0(
      "tract_by_state2021/Tract2021_",
      gsub(" ", "_", .y),
      ".rds"
    )
    saveRDS(.x, file_name)
  })

Tract2022_by_state <- Tract2022 %>%
  group_by(Statenames) %>%
  group_split()

dir.create("tract_by_state2022", showWarnings = FALSE)

Tract2022 %>%
  split(.$Statenames) %>%
  purrr::iwalk(~ {
    file_name <- paste0(
      "tract_by_state2022/Tract2022_",
      gsub(" ", "_", .y),
      ".rds"
    )
    saveRDS(.x, file_name)
  })

Tract2023_by_state <- Tract2023 %>%
  group_by(Statenames) %>%
  group_split()

dir.create("tract_by_state2023", showWarnings = FALSE)

Tract2023 %>%
  split(.$Statenames) %>%
  purrr::iwalk(~ {
    file_name <- paste0(
      "tract_by_state2023/Tract2023_",
      gsub(" ", "_", .y),
      ".rds"
    )
    saveRDS(.x, file_name)
  })

Tract2024_by_state <- Tract2024 %>%
  group_by(Statenames) %>%
  group_split()

dir.create("tract_by_state2024", showWarnings = FALSE)

Tract2024 %>%
  split(.$Statenames) %>%
  purrr::iwalk(~ {
    file_name <- paste0(
      "tract_by_state2024/Tract2024_",
      gsub(" ", "_", .y),
      ".rds"
    )
    saveRDS(.x, file_name)
  })


