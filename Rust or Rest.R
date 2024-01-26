Rust_or_Rest = read_excel('Rust_or_Rest.xlsx')

Rust_or_Rest %>%
  count(`Rest/Rust`, wt = NULL, sort = TRUE) %>%
  group_by(`Rest/Rust`)

Rust_or_Rest = Rust_or_Rest %>%
  mutate(SeriesDifference = abs(T1R1GP - T2R1GP))

OneGameDiff = Rust_or_Rest %>%
  filter(SeriesDifference == 1)

OneGameDiff %>%
  count(`Rest/Rust`, wt = NULL, sort = TRUE) %>%
  group_by(`Rest/Rust`)
