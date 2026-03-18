attach_analytics <- function(
  df,
  analytics,
  by_cols = c("Video ID", "Channel ID", "Channel Name")
) {
  # 1. Find overlapping column names
  common <- intersect(names(df), names(analytics))
  
  # 2. Overlaps *other than* the join keys
  to_drop_from_df <- setdiff(common, by_cols)
  
  # 3. Drop these from df so analytics' versions win
  df_clean <- df %>%
    dplyr::select(-dplyr::any_of(to_drop_from_df))
  
  # 4. Safe join: no .x/.y suffixes
  dplyr::left_join(df_clean, analytics, by = by_cols)
}
