library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

# Parameters
n_taxonomies <- 12
n_props <- 30

# Identify prop_ columns and top taxonomies
prop_cols <- names(wa_or_chestnut_summary)[grepl(
  "^prop_",
  names(wa_or_chestnut_summary)
)]
top_taxonomies <- wa_or_chestnut_summary |>
  count(bp_taxonomy_description, sort = TRUE) |>
  slice_head(n = n_taxonomies) |>
  pull(bp_taxonomy_description)

# Compute mean proportions by taxonomy
summary_df <- wa_or_chestnut_summary |>
  filter(bp_taxonomy_description %in% top_taxonomies) |>
  select(bp_taxonomy_description, all_of(prop_cols)) |>
  group_by(bp_taxonomy_description) |>
  summarise(
    across(all_of(prop_cols), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  pivot_longer(
    -bp_taxonomy_description,
    names_to = "prop",
    values_to = "mean_prop"
  )

# Select top prop variables by overall mean
prop_order <- summary_df |>
  group_by(prop) |>
  summarise(overall_mean = mean(mean_prop, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(overall_mean)) |>
  pull(prop)

top_props <- head(prop_order, n_props)

# Prepare plotting data
taxonomy_order <- wa_or_chestnut_summary |>
  count(bp_taxonomy_description, sort = TRUE) |>
  filter(bp_taxonomy_description %in% top_taxonomies) |>
  pull(bp_taxonomy_description)

plot_df <- summary_df |>
  filter(prop %in% top_props) |>
  mutate(
    prop = factor(prop, levels = rev(top_props)),
    bp_taxonomy_description = factor(
      bp_taxonomy_description,
      levels = taxonomy_order
    )
  )

# Plot heatmap
p <- ggplot(
  plot_df,
  aes(x = prop, y = bp_taxonomy_description, fill = mean_prop)
) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "magma") +
  labs(
    title = "Mean proportion of top prop_* variables by provider taxonomy",
    x = "prop variable",
    y = "Billing provider taxonomy",
    fill = "Mean proportion"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 9)
  )

print(p)

# Save plot
outfile <- file.path(OUTPUT_GRAPHS_DIR, "prop_vars_by_taxonomy.png")
ggsave(outfile, p, width = 12, height = 6, dpi = 300)

outfile
