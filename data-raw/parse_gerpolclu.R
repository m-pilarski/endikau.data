`%0%` <- vctrs::`%0%`

gerpolclu_tbl <- 
  fs::dir_ls(
    path=here::here("data_raw/input/GermanPolarityClues-2012/"),
    glob="*.tsv$"
  ) |> 
  purrr::map_dfr(
    readr::read_tsv, col_types="c", quote="'",
    col_names=c(
      "tok_str", "tok_lemma_str", "tok_pos_tag", "tok_pol_lab", 
      "tok_pol_pos_neg_neu_prob", "unk"
    )
  ) |> 
  dplyr::distinct() |> 
  tidyr::separate_wider_delim(
    cols=tok_pol_pos_neg_neu_prob, delim="/", 
    names=c("tok_pol_pos_prob", "tok_pol_neg_prob", "tok_pol_neu_prob")
  ) |> 
  dplyr::mutate(
    tok_is_stem = tok_str == tok_lemma_str,
    tok_str = stringi::stri_trans_tolower(tok_str),
    tok_pol_lab = factor(
      tok_pol_lab, levels=c("negative", "neutral", "positive"),
      labels=c("sen-neg-med", "sen-neu", "sen-pos-med")
    ),
    dplyr::across(tidyselect::matches("tok_pol_.+_prob"), \(.x){
      as.numeric(dplyr::na_if(.x, "-"))
    }),
    tok_pol_lab_max_prob = 
      dplyr::pick(c(tok_pol_neg_prob, tok_pol_neu_prob, tok_pol_pos_prob)) |> 
      as.matrix() |> 
      apply(1, which.max) |> 
      purrr::map_chr(\(.x){
        c("sen-neg-med", "sen-neu", "sen-pos-med")[.x] %0% NA_character_
      }) |> 
      factor(levels=levels(tok_pol_lab))
  ) |> 
  dplyr::group_by(tok_str) |> 
  dplyr::group_map(\(.gdata, ...){
    
    if(nrow(.gdata) == 1){return(.gdata)}
    
    if(nrow(.gdata) > 1 & any(!is.na(.gdata$tok_pol_lab_max_prob))){
      .gdata <- dplyr::filter(.gdata, tok_pol_lab == tok_pol_lab_max_prob)
    }
    
    if(nrow(.gdata) > 1){
      .gdata <- dplyr::bind_rows(
        dplyr::slice(.gdata, 0), dplyr::distinct(.gdata, tok_str, tok_pol_lab)
      )
    }
    
    if(nrow(.gdata) > 1){
      .gdata <- dplyr::mutate(
        dplyr::slice(.gdata, 1),
        tok_pol_lab = factor("sen-neu", levels=levels(.gdata$tok_pol_lab))
      )
    }
    
    return(.gdata)
    
  }, .keep=TRUE) |> 
  dplyr::bind_rows() |> 
  dplyr::select(tok_str, tok_pol_lab) |> 
  dplyr::mutate(
    tok_pol_num = dplyr::case_match(
      tok_pol_lab, "sen-neg-med" ~ -1, "sen-neu" ~ 0, "sen-pos-med" ~ 1
    )
  )
  
usethis::use_data(gerpolclu_tbl)
