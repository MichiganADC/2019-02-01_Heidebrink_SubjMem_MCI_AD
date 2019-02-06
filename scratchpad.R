# scratchpad.R

test_df <- tibble(
  ptid = c("A001", "A001", "A001")
  , date = as.Date(c("2018-01-01", "2019-01-01", "2020-01-01"))
  , normcog = c(0L, NA_integer_, NA_integer_)
  , fu_normcog = c(NA_integer_, 0L, NA_integer_)
  , tele_normcog = c(NA_integer_, NA_integer_, 1L)
  , sex = c(1L, NA_integer_, NA_integer_)
  , fu_sex = c(NA_integer_, 1L, NA_integer_)
  , mocatots = c(24L, NA_integer_, NA_integer_)
  , tele_mocatots = c(NA_integer_, NA_integer_, 25L)
)

solo_collapse_ift_cols <- function(df) {
  
  # Get collapsible fields and the correpsonding
  # follow-up visit `fu_` and telephone visit `tele_` fields
  i_cols <- get_ift_dups(names(df)) # collapsible_fields
  f_cols <- paste0("fu_", i_cols)
  t_cols <- paste0("tele_", i_cols)
  
  # Iterate over collapsible fields `i_cols`,
  # coalescing `fu_` and/or `tele_` fields into `i_cols`
  for (i in seq_along(i_cols)) {
    # print(paste(i, i_cols[i], f_cols[i], t_cols[i]))
    # IVP, FVP (fu_), and TVP (tele_) columns are in df
    if (!is.null(df[[i_cols[i]]]) &
        !is.null(df[[f_cols[i]]]) &
        !is.null(df[[t_cols[i]]])) {
      df = df %>%
        mutate(!!i_cols[i] := coalesce(df[[i_cols[i]]],
                                       df[[t_cols[i]]],
                                       df[[f_cols[i]]])) %>%
        select(-!!f_cols[[i]] , -!!t_cols[[i]])
    } 
    # IVP and FVP (fu_) columns are in df
    else if (!is.null(df[[i_cols[i]]]) &
             !is.null(df[[f_cols[i]]]) &
             is.null(df[[t_cols[i]]])) {
      df = df %>%
        mutate(!!i_cols[i] := coalesce(df[[i_cols[i]]],
                                       df[[f_cols[i]]])) %>%
        select(-!!f_cols[i])
    }
    # IVP and TVP (tele_) columns are in df
    else if (!is.null(df[[i_cols[i]]]) &
             is.null(df[[f_cols[i]]]) &
             !is.null(df[[t_cols[i]]])) {
      df = df %>%
        mutate(!!i_cols[i] := coalesce(df[[i_cols[i]]],
                                       df[[t_cols[i]]])) %>%
        select(-!!t_cols[i])
    }
  }
  
  # Return df
  df
} 

solo_collapse_ift_cols(test_df)



# get_latest_visit <- function(df, id_field, date_field) {
#   enquo_id_field <- enquo(id_field)
#   enquo_date_field <- enquo(date_field)
#   
#   df %>% 
#     filter(!is.na(!!enquo_id_field)) %>% 
#     filter(!is.na(!!enquo_date_field)) %>%
#     arrange(!!enquo_id_field, !!enquo_date_field) %>% 
#     mutate(visit_unit = 1L) %>% 
#     group_by(!!enquo_id_field) %>% 
#     mutate(visit_count = cumsum(visit_unit)) %>% 
#     # mutate(max_visit_count = max(visit_count)) %>% 
#     filter(visit_count == max(visit_count)) %>% 
#     ungroup() %>% 
#     select(-visit_unit, -visit_count)
# }
# 
# get_latest_visit(test_df, ptid, date)
# 
# get_earliest_visit <- function(df, id_field, date_field) {
#   enquo_id_field <- enquo(id_field)
#   enquo_date_field <- enquo(date_field)
#   
#   df %>% 
#     filter(!is.na(!!enquo_id_field)) %>% 
#     filter(!is.na(!!enquo_date_field)) %>%
#     arrange(!!enquo_id_field, !!enquo_date_field) %>% 
#     mutate(visit_unit = 1L) %>% 
#     group_by(!!enquo_id_field) %>% 
#     mutate(visit_count = cumsum(visit_unit)) %>% 
#     # mutate(min_visit_count = min(visit_count)) %>% 
#     filter(visit_count == min(visit_count)) %>% 
#     ungroup() %>% 
#     select(-visit_unit, -visit_count)
# }
# 
# get_earliest_visit(test_df, ptid, date) 

get_visit_n <- function(df, id_field, date_field, n) {
  enquo_id_field <- enquo(id_field)
  enquo_date_field <- enquo(date_field)
  
  if (is.finite(n)) { vis_cnt_fltr <- enquo(n) }
  else if (n < 0) { vis_cnt_fltr <- expr(min(visit_count)) }
  else { vis_cnt_fltr <- expr(max(visit_count)) }
  
  df %>% 
    filter(!is.na(!!enquo_id_field)) %>% 
    filter(!is.na(!!enquo_date_field)) %>%
    arrange(!!enquo_id_field, !!enquo_date_field) %>% 
    mutate(visit_unit = 1L) %>% 
    group_by(!!enquo_id_field) %>% 
    mutate(visit_count = cumsum(visit_unit)) %>%
    filter(visit_count == !!vis_cnt_fltr) %>% 
    ungroup() %>% 
    select(-visit_unit, -visit_count)
}

get_visit_n(test_df, ptid, date, -Inf)
get_visit_n(test_df, ptid, date, 0)
get_visit_n(test_df, ptid, date, 1)
get_visit_n(test_df, ptid, date, 2)
get_visit_n(test_df, ptid, date, 3)
get_visit_n(test_df, ptid, date, 4)
get_visit_n(test_df, ptid, date, Inf)


