video_demographic_prep <- function(files, talent = NULL, talent_index = NULL){
    # Prepare the analytics Data
    Analytics <- prepare_analytics(
      files = files,
      talent = talent,
      talent_index = talent_index
    )

    # 2) Get demographic data
    Demo_raw <- .get_type_data(
      files = files,
      type = "video_demographics",
      talent_index = talent_index
    )

    # 3) Join analytics onto demographics
    Df_joined <- Demo_raw %>%
      attach_analytics(Analytics)
    
    return(Df_joined)
    }

#df <- video_demographic_prep(Talents[1])
#glimpse(df)

video_geographic_prep <- function(files, talent = NULL, talent_index = NULL){
    # Prepare the analytics Data
    Analytics <- prepare_analytics(
      files = files,
      talent = talent,
      talent_index = talent_index
    )
    
    # 2 Geographic data 
    Geo_raw <- .get_type_data(
      files = files,
      type = "video_geography",
      talent_index = talent_index
    )

    # 3) Join analytics onto geographic
    Df_joined <- Geo_raw %>%
    attach_analytics(Analytics)

    return(Df_joined)
    }

#df <- video_geographic_prep(Talents[1])
#glimpse(df)

video_monetary_prep <- function(files, talent = NULL, talent_index = NULL){
  # Prepare analytics so we can attach Content Type, duration, etc.
  Analytics <- prepare_analytics(
    files = files,
    talent = talent,
    talent_index = talent_index
  )

  Data <- .get_type_data(
      files = files,
      type = "video_monetary",
      talent_index = talent_index
    ) %>%
    attach_analytics(Analytics) %>%
    clean_published_at() %>%
    clean_duration_cols()

  return(Data)
}

video_analytics_prep <- function(files, talent = NULL, talent_index = NULL) {
  prepare_analytics(
    files = files,
    talent = talent,
    talent_index = talent_index
  ) %>%
    clean_published_at() %>%
    clean_duration_cols()
}
