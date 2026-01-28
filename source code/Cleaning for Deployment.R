load("Data.RData")



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

saveRDS(County2020, "County2020.rds")
saveRDS(County2021, "County2021.rds")
saveRDS(County2022, "County2022.rds")
saveRDS(County2023, "County2023.rds")

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


