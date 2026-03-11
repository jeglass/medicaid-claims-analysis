library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(tidymodels)
library(cluster)

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

library(GGally)

wa_or_chestnut_summary |>
  select(starts_with("prop_H0")) |>
  ggpairs()


# K-means clustering on prop variables
set.seed(123)

# 1. Recipe: select variables and handle NAs
kmeans_recipe <- recipe(
  ~ prop_H0005 + prop_H0010,
  data = wa_or_chestnut_summary
) |>
  step_mutate(across(everything(), ~ replace_na(., 0))) |>
  step_normalize(all_predictors()) # optional but common for k-means

# 2. Model specification
kmeans_spec <- k_means(num_clusters = 4) |>
  set_engine("stats")

# 3. Workflow
kmeans_wf <- workflow() |>
  add_recipe(kmeans_recipe) |>
  add_model(kmeans_spec)

# 4. Fit model
kmeans_fit <- fit(kmeans_wf, data = wa_or_chestnut_summary)

# 5. Add cluster assignments back to data
wa_or_chestnut_summary_clusters <- augment(
  kmeans_fit,
  wa_or_chestnut_summary
) |>
  mutate(.cluster = factor(.cluster))

p_cluster <- ggplot(
  wa_or_chestnut_summary_clusters,
  aes(x = prop_H0004, y = prop_H0010, color = .cluster)
) +
  geom_point(alpha = 0.6) +
  labs(
    title = "K-means Clustering of Providers Based on prop_H0004 and prop_H0010",
    x = "prop_H0004",
    y = "prop_H0010",
    color = "Cluster"
  ) +
  theme_minimal()

print(p_cluster)

# Save
outfile_cluster <- file.path(OUTPUT_GRAPHS_DIR, "kmeans_clusters.png")
ggsave(outfile_cluster, p_cluster, width = 8, height = 6, dpi = 300)
cat("\nSaved cluster plot to", outfile_cluster, "\n")
