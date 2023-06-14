
# Requirement: disable the "unique ID" in input.R BEFORE running this script, otherwise it won't work properly

moved <- data.raw %>%
  select(
    id,
    FollowUpPeriod,
    starts_with("Zip"),
  ) %>%
  pivot_wider(id_cols = c(id, ZipInj, ZipDis), names_from = FollowUpPeriod, values_from = ZipF, names_prefix = "ZipF_") %>%
  group_by(id) %>%
  mutate(
    # Moved since last recorded Zipcode
    Moved_Dis = ZipInj != ZipDis,
    Moved_F1 = ZipDis != ZipF_1,
    Moved_F2 = ZipF_1 != ZipF_2,
    Moved_F5 = ZipF_2 != ZipF_5,
    Moved_F10 = ZipF_5 != ZipF_10,
    # total of migrations
    Moved_n = sum(c(
      Moved_Dis,
      Moved_F1,
      Moved_F2,
      Moved_F5,
      Moved_F10
    ), na.rm = TRUE),
  ) %>%
  ungroup()

moved

write_csv(moved, "dataset/moved.csv")
