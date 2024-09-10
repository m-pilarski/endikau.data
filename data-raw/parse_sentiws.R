sentiws_tbl <- 
  fs::dir_ls(
    path=here::here("data_raw/input/SentiWS_v2.0/"),
    glob="*/SentiWS_v2.0_*.txt"
  ) |> 
  purrr::map_dfr(function(.path){
    .path |> 
      readr::read_lines() |> 
      stringi::stri_replace_first_fixed("|", "\t") |>
      stringi::stri_c(collapse="\n") |> 
      readr::read_tsv(
        col_names=c(
          "tok_str", "tok_pos_tag", "tok_pol_num", "tok_str_inflect_csv"
        ),
        col_types=readr::cols(tok_pol_num="d", .default="c")
      ) |> 
      dplyr::mutate(
        tok_str_inflect_lst = stringi::stri_split_regex(
          tok_str_inflect_csv, "\\s*,\\s*"
        ),
        tok_str_lst = as.list(tok_str),
        .keep="unused"
      ) |> 
      tidyr::pivot_longer(
        cols=c(tok_str_lst, tok_str_inflect_lst), values_to="tok_str_lst",
        names_to="tok_is_stem", names_transform=\(.n){.n == "word"}
      ) |> 
      tidyr::unnest_longer(tok_str_lst, values_to="tok_str") |> 
      dplyr::select(tok_str, tok_pol_num, tok_is_stem, tok_pos_tag)
  }) |> 
  dplyr::mutate(dplyr::across(tok_pos_tag, factor)) |> 
  dplyr::mutate(
    tok_str = stringi::stri_trans_tolower(tok_str),
    tok_pol_abs_interval = as.integer(
      ggplot2::cut_number(abs(tok_pol_num), n=3)
    ),
    tok_pol_lab = factor(
      x=sign(tok_pol_num) * tok_pol_abs_interval,
      levels=-3:3,
      labels=c(
        "sen-neg-max", "sen-neg-med", "sen-neg-min", "sen-neu",
        "sen-pos-min", "sen-pos-med", "sen-pos-max"
      )
    )
  ) |> 
  dplyr::select(tok_str, tok_pol_lab, tok_pol_num)
  
usethis::use_data(sentiws_tbl, overwrite=TRUE)