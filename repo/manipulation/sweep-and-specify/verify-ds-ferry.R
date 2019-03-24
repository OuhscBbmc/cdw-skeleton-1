

col_types_review_required <- readr::cols_only(
  category                = readr::col_character(),
  broad_match_keyword     = readr::col_character())


col_types_review_skip <- readr::cols_only(
  category                = readr::col_character(),
  exact_match_keyword     = readr::col_character())




verify_keyword <- function(ds_sql,review_required){


  if(review_required){

    checkmate::assert_character(ds_sql$category               , any.missing=T , pattern="^.{2,255}$" )
    checkmate::assert_character(ds_sql$broad_match_keyword    , any.missing=T , pattern="^.{2,255}$" , unique=T)
    checkmate::assert_character(ds_sql$project                , any.missing=T , pattern="^.{2,255}$" )
    checkmate::assert_character(ds_sql$sql_similar            , any.missing=F , min.chars = 100)

  }
  else
  {
    checkmate::assert_character(ds_sql$category               , any.missing=T , pattern="^.{2,255}$" )
    checkmate::assert_character(ds_sql$exact_match_keyword    , any.missing=T , pattern="^.{2,255}$" , unique=T)
    checkmate::assert_character(ds_sql$project                , any.missing=T , pattern="^.{2,255}$" )
    checkmate::assert_character(ds_sql$sql_similar            , any.missing=F , min.chars = 100)

  }
}



verify_ds <- function(ds,ss_type){

  if(ss_type == "centricity_diagnosis"){

    checkmate::assert_character(ds$diagnosis_id,           pattern="^.{1,10}$",      any.missing=F,  unique=T)
    checkmate::assert_character(ds$category,               pattern="^.{1,75}$",      any.missing=T,  unique=F)
    checkmate::assert_character(ds$match_keyword,          pattern="^.{1,150}$",     any.missing=F,  unique=F)
    checkmate::assert_character(ds$code,                   pattern="^.{1,10}$",      any.missing=F,  unique=T)
    checkmate::assert_character(ds$code_type,              pattern="^.{1,10}$",      any.missing=F,  unique=F)
    checkmate::assert_character(ds$code_description,       pattern="^.{1,255}$",     any.missing=F,  unique=F)
    checkmate::assert_character(ds$project,                pattern="^.{1,100}$",     any.missing=F,  unique=F)
    checkmate::assert_logical  (ds$desired,                                          any.missing=F           )

  }


  if(ss_type == "centricity_location"){

    checkmate::assert_character(ds$loc_id,                 pattern="^.{1,16}$",      any.missing=F,  unique=T)
    checkmate::assert_character(ds$category,               pattern="^.{1,75}$",      any.missing=T,  unique=F)
    checkmate::assert_character(ds$match_keyword,          pattern="^.{1,150}$",     any.missing=F,  unique=F)
    checkmate::assert_character(ds$loc_name,               pattern="^.{1,70}$",      any.missing=F,  unique=F)
    checkmate::assert_character(ds$loc_abbrevated,         pattern="^.{1,10}$",      any.missing=F,  unique=T)
    checkmate::assert_character(ds$loc_address,            pattern="^.{1,150}$",     any.missing=F,  unique=F)
    checkmate::assert_character(ds$project,                pattern="^.{1,100}$",     any.missing=F,  unique=F)
    checkmate::assert_logical(  ds$desired,                                          any.missing=F           )

  }


  if(ss_type == "centricity_medication"){

    checkmate::assert_character(ds$mid,                    pattern="^.{1,16}$",      any.missing=F,  unique=F)
    checkmate::assert_character(ds$category,               pattern="^.{1,75}$",      any.missing=T,  unique=F)
    checkmate::assert_character(ds$match_keyword,          pattern="^.{1,150}$",     any.missing=F,  unique=F)
    checkmate::assert_character(ds$med_description,        pattern="^.{1,150}$",     any.missing=F,  unique=F)
    #checkmate::assert_character(ds$med_instructions,      pattern="^.{1,10}$",      any.missing=F,  unique=F)
    checkmate::assert_character(ds$med_generic,            pattern="^.{1,75}$",      any.missing=T,  unique=F)
    checkmate::assert_character(ds$project,                pattern="^.{1,100}$",     any.missing=F,  unique=F)
    checkmate::assert_logical(  ds$desired,                                          any.missing=F           )

  }


  if(ss_type == "centricity_obs"){

    checkmate::assert_character(ds$obs_hdid,              pattern="^.{1,10}$",        any.missing=F,  unique=T)
    checkmate::assert_character(ds$category,              pattern="^.{1,75}$",        any.missing=T,  unique=F)
    checkmate::assert_character(ds$match_keyword,         pattern="^.{1,150}$",       any.missing=F,  unique=F)
    checkmate::assert_character(ds$obs_name,              pattern="^.{1,15}$",        any.missing=F,  unique=T)
    checkmate::assert_character(ds$obs_description,       pattern="^.{1,220}$",       any.missing=F,  unique=F)
    checkmate::assert_character(ds$obs_units,             pattern="^.{1,20}$",        any.missing=T,  unique=F)
    checkmate::assert_character(ds$obs_active_flag,       pattern="^.{1,1}$",         any.missing=T,  unique=F)
    checkmate::assert_character(ds$project,               pattern="^.{1,100}$",       any.missing=F,  unique=F)
    checkmate::assert_logical(  ds$desired,                                           any.missing=F           )

  }


  if(ss_type == "idx_cpt"){

    checkmate::assert_character(ds$cpt_id,                pattern="^.{1,10}$",        any.missing=F, unique=T)
    checkmate::assert_character(ds$category,              pattern="^.{1,75}$",        any.missing=T, unique=F)
    checkmate::assert_character(ds$match_keyword,         pattern="^.{1,150}$",       any.missing=F, unique=F)
    checkmate::assert_character(ds$cpt_code,              pattern="^.{1,6}$",         any.missing=F, unique=T)
    checkmate::assert_character(ds$code_description,      pattern="^.{1,250}$",       any.missing=F, unique=F)
    checkmate::assert_character(ds$cpt_deactivated_flag,  pattern="^.{1,1}$",         any.missing=T, unique=F)
    checkmate::assert_character(ds$cpt_deleted_flag,      pattern="^.{1,1}$",         any.missing=T, unique=F)
    checkmate::assert_character(ds$project,               pattern="^.{1,100}$",       any.missing=F, unique=F)
    checkmate::assert_logical(  ds$desired,                                           any.missing=F           )

  }



  if(ss_type == "idx_diagnosis"){

    checkmate::assert_character(ds$diag_id,               pattern="^.{1,10}$",        any.missing=F, unique=T)
    checkmate::assert_character(ds$category,              pattern="^.{1,75}$",        any.missing=T, unique=F)
    checkmate::assert_character(ds$match_keyword,         pattern="^.{1,150}$",       any.missing=F, unique=F)
    checkmate::assert_character(ds$code,                  pattern="^.{1,10}$",        any.missing=F, unique=T)
    checkmate::assert_character(ds$code_type,             pattern="^.{1,10}$",        any.missing=F, unique=F)
    checkmate::assert_character(ds$code_description,      pattern="^.{1,250}$",       any.missing=F, unique=F)
    checkmate::assert_character(ds$code_deactivated_flag, pattern="^.{1,1}$",         any.missing=T, unique=F)
    checkmate::assert_character(ds$code_deleted_flag,     pattern="^.{1,1}$",         any.missing=T, unique=F)
    checkmate::assert_character(ds$project,               pattern="^.{1,100}$",       any.missing=F, unique=F)
    checkmate::assert_logical(  ds$desired,                                           any.missing=F          )

  }


if(ss_type == "idx_location"){

    checkmate::assert_character(ds$loc_id,                pattern="^.{1,10}$",        any.missing=F, unique=T)
    checkmate::assert_character(ds$category,              pattern="^.{1,75}$",        any.missing=T, unique=F)
    checkmate::assert_character(ds$match_keyword,         pattern="^.{1,150}$",       any.missing=F, unique=F)
    checkmate::assert_character(ds$loc_name,              pattern="^.{1,100}$",       any.missing=F, unique=F)
    checkmate::assert_character(ds$loc_abbrevated,        pattern="^.{1,6}$",         any.missing=T, unique=F)
    checkmate::assert_character(ds$loc_deactivated_flag,  pattern="^.{1,1}$",         any.missing=T, unique=F)
    checkmate::assert_character(ds$loc_deleted_flag,      pattern="^.{1,1}$",         any.missing=T, unique=F)
    checkmate::assert_character(ds$project,               pattern="^.{1,100}$",       any.missing=F, unique=F)
    checkmate::assert_logical(  ds$desired,                                           any.missing=F)

  }


  if(ss_type == "idx_sched_location"){

    checkmate::assert_character(ds$loc_id,                pattern="^.{1,10}$",        any.missing=F, unique=T)
    checkmate::assert_character(ds$category,              pattern="^.{1,75}$",        any.missing=T, unique=F)
    checkmate::assert_character(ds$match_keyword,         pattern="^.{1,150}$",       any.missing=F, unique=F)
    checkmate::assert_character(ds$loc_name,              pattern="^.{1,100}$",       any.missing=F, unique=F)
    checkmate::assert_character(ds$loc_abbrevated,        pattern="^.{1,6}$",         any.missing=T, unique=F)
    checkmate::assert_character(ds$loc_billing_name,      pattern="^.{1,100}$",       any.missing=T, unique=F)
    checkmate::assert_character(ds$loc_clinic_name,       pattern="^.{1,40}$",        any.missing=T, unique=F)
    checkmate::assert_character(ds$loc_deactivated_flag,  pattern="^.{1,1}$",         any.missing=T, unique=F)
    checkmate::assert_character(ds$loc_deleted_flag,      pattern="^.{1,1}$",         any.missing=T, unique=F)
    checkmate::assert_character(ds$project,               pattern="^.{1,100}$",       any.missing=F, unique=F)
    checkmate::assert_logical(  ds$desired,                                           any.missing=F)

  }


}
