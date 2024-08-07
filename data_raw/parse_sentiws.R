sentiws_data <- 
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
        col_names=c("word", "pos_tag", "polarity", "inflections"),
        col_types=readr::cols(polarity="d", .default="c")
      ) |> 
      dplyr::mutate(
        inflections = stringi::stri_split_fixed(inflections, ","),
        word = as.list(word)
      ) |> 
      tidyr::pivot_longer(
        cols=c(word, inflections), values_to="word",
        names_to="is_stem", names_transform=\(.n){.n == "word"}
      ) |> 
      tidyr::unnest_longer(word) |> 
      dplyr::select(word, is_stem, polarity, pos_tag)
  }) |> 
  dplyr::mutate(pos_tag = factor(pos_tag))
  
fst::write_fst(sentiws_data, here::here("data/sentiws_data.fst"))