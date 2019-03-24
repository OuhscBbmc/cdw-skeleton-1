col_types_centricity_diagnosis <- readr::cols_only(
  diagnosis_id	           =       readr::col_integer(),
  category	               =       readr::col_character(),
  match_keyword            =       readr::col_character(),
  code	                   =       readr::col_character(),
  code_type	               =       readr::col_character(),
  code_description         =       readr::col_character(),
  project	                 =       readr::col_character(),
  desired                  =       readr::col_logical()
)

col_types_centricity_location <- readr::cols_only(
  loc_id	                 =       readr::col_double(),
  category	               =       readr::col_character(),
  match_keyword	           =       readr::col_character(),
  loc_name	               =       readr::col_character(),
  loc_abbrevated	         =       readr::col_character(),
  loc_address	             =       readr::col_character(),
  project	                 =       readr::col_character(),
  desired                  =       readr::col_logical()
)

col_type_centricity_medication <- readr::cols_only(
  mid	                     =       readr::col_double(),
  category                 =       readr::col_character(),
  match_keyword	           =       readr::col_character(),
  med_description	         =       readr::col_character(),
  med_generic	             =       readr::col_character(),
  project                  =       readr::col_character(),
  desired                  =       readr::col_logical()
)

col_type_centricity_obs        <- readr::cols_only(
  obs_hdid	               =       readr::col_integer(),
  category	               =       readr::col_character(),
  match_keyword	           =       readr::col_character(),
  obs_name	               =       readr::col_character(),
  obs_description	         =       readr::col_character(),
  obs_units	               =       readr::col_character(),
  obs_active_flag	         =       readr::col_character(),
  project	                 =       readr::col_character(),
  desired                  =       readr::col_logical()
)

col_types_idx_cpt              <- readr::cols_only(
  cpt_id	                 =       readr::col_integer(),
  category	               =       readr::col_character(),
  match_keyword	           =       readr::col_character(),
  cpt_code	               =       readr::col_character(),
  code_description	       =       readr::col_character(),
  cpt_category_name	       =       readr::col_character(),
  cpt_deactivated_flag	   =       readr::col_character(),
  cpt_deleted_flag	       =       readr::col_character(),
  project	                 =       readr::col_character(),
  desired                  =       readr::col_logical()
)

col_types_idx_diagnosis       <- readr::cols_only(
  diag_id                  =       readr::col_integer(),
  category                 =       readr::col_character(),
  match_keyword            =       readr::col_character(),
  code                     =       readr::col_character(),
  code_type                =       readr::col_character(),
  code_description         =       readr::col_character(),
  code_deactivated_flag    =       readr::col_character(),
  code_deleted_flag        =       readr::col_character(),
  project                  =       readr::col_character(),
  desired                  =       readr::col_logical()
)

col_types_idx_location        <- readr::cols_only(
  loc_id	                 =       readr::col_integer(),
  category	               =       readr::col_character(),
  match_keyword	           =       readr::col_character(),
  loc_name	               =       readr::col_character(),
  loc_abbrevated	         =       readr::col_character(),
  loc_deactivated_flag	   =       readr::col_character(),
  loc_deleted_flag	       =       readr::col_character(),
  project                  =       readr::col_character(),
  desired                  =       readr::col_logical()
)

col_types_idx_sched_location  <- readr::cols_only(
  loc_id	                 =       readr::col_integer(),
  category	               =       readr::col_character(),
  match_keyword	           =       readr::col_character(),
  loc_name	               =       readr::col_character(),
  loc_abbrevated	         =       readr::col_character(),
  loc_billing_name	       =       readr::col_character(),
  loc_clinic_name	         =       readr::col_character(),
  loc_deactivated_flag	   =       readr::col_character(),
  loc_deleted_flag	       =       readr::col_character(),
  project	                 =       readr::col_character(),
  desired                  =       readr::col_logical()
)

verify_ds <- function(ds,ss_type){

  if(ss_type == "centricity_diagnosis"){

    checkmate::assert_integer(  ds$diagnosis_id,           lower= 1, upper=100000000,      any.missing=F,  unique=T)
    checkmate::assert_character(ds$category,               pattern="^.{1,75}$",            any.missing=T,  unique=F)
    checkmate::assert_character(ds$match_keyword,          pattern="^.{1,150}$",           any.missing=F,  unique=F)
    checkmate::assert_character(ds$code,                   pattern="^.{1,10}$",            any.missing=F,  unique=T)
    checkmate::assert_character(ds$code_type,              pattern="^.{1,10}$",            any.missing=F,  unique=F)
    checkmate::assert_character(ds$code_description,       pattern="^.{1,255}$",           any.missing=F,  unique=F)
    checkmate::assert_character(ds$project,                pattern="^.{1,100}$",           any.missing=F,  unique=F)
    checkmate::assert_logical  (ds$desired,                                                any.missing=F           )

  }

  if(ss_type == "centricity_location"){

    checkmate::assert_double(ds$loc_id,                 lower= 1, upper=1000000000000000,   any.missing=F,  unique=T)
    checkmate::assert_character(ds$category,               pattern="^.{1,75}$",      any.missing=T,  unique=F)
    checkmate::assert_character(ds$match_keyword,          pattern="^.{1,150}$",     any.missing=F,  unique=F)
    checkmate::assert_character(ds$loc_name,               pattern="^.{1,70}$",      any.missing=F,  unique=F)
    checkmate::assert_character(ds$loc_abbrevated,         pattern="^.{1,10}$",      any.missing=F,  unique=T)
    checkmate::assert_character(ds$loc_address,            pattern="^.{1,150}$",     any.missing=F,  unique=F)
    checkmate::assert_character(ds$project,                pattern="^.{1,100}$",     any.missing=F,  unique=F)
    checkmate::assert_logical(  ds$desired,                                          any.missing=F           )

  }

  if(ss_type == "centricity_medication"){

    checkmate::assert_double(   ds$mid,                    lower= 1, upper=1000000000000000,      any.missing=F,  unique=T)
    checkmate::assert_character(ds$category,               pattern="^.{1,75}$",      any.missing=T,  unique=F)
    checkmate::assert_character(ds$match_keyword,          pattern="^.{1,150}$",     any.missing=F,  unique=F)
    checkmate::assert_character(ds$med_description,        pattern="^.{1,150}$",     any.missing=F,  unique=F)
    #checkmate::assert_character(ds$med_instructions,      pattern="^.{1,10}$",      any.missing=F,  unique=F)
    checkmate::assert_character(ds$med_generic,            pattern="^.{1,75}$",      any.missing=T,  unique=F)
    checkmate::assert_character(ds$project,                pattern="^.{1,100}$",     any.missing=F,  unique=F)
    checkmate::assert_logical(  ds$desired,                                          any.missing=F           )

  }

  if(ss_type == "centricity_obs"){

    checkmate::assert_integer(  ds$obs_hdid,              lower= 1, upper=100000000,  any.missing=F,  unique=T)
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

    checkmate::assert_integer(  ds$cpt_id,                lower= 1, upper=100000000,  any.missing=F, unique=T)
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

    checkmate::assert_integer(  ds$diag_id,               lower= 1, upper=100000000,  any.missing=F, unique=T)
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

    checkmate::assert_integer(  ds$loc_id,                lower= 1, upper=100000000,  any.missing=F, unique=T)
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

    checkmate::assert_integer(  ds$loc_id,                lower= 1, upper=100000000,  any.missing=F, unique=T)
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

columns_to_write_centricity_diagnosis  <- c(
  "diagnosis_id",
  "category",
  #"match_keyword",
  #"code",
  #"code_type",
  #"code_description",
  "project" #,
  #"desired"
)

columns_to_write_centricity_location  <- c(
  "loc_id",
  "category",
  #"match_keyword",
  "loc_name",
  "loc_abbrevated",
  "loc_address",
  "project" #,
  #"desired"
)

columns_to_write_centricity_medication  <- c(
  "mid",
  "category",
  #"match_keyword",
  "med_description",
  "med_generic",
  "project" #,
  #"desired"
)

columns_to_write_centricity_obs  <- c(
  "obs_hdid",
  "category",
  #"match_keyword",
  #"obs_name",
  #"obs_description",
  #"obs_units",
  #"obs_active_flag",
  "project" #,
  #"desired"
)

columns_to_write_idx_cpt  <- c(
  "cpt_id",
  "category",
  #"match_keyword",
  #"cpt_code",
  #"code_description",
  #"cpt_category_name",
  #"cpt_deactivated_flag",
  #"cpt_deleted_flag",
  "project" #,
  #"desired"
)

columns_to_write_idx_diagnosis  <- c(
  "diag_id",
  "category",
  #"match_keyword",
  #"code",
  #"code_type",
  #"code_description",
  #"code_deactivated_flag",
  #"code_deleted_flag",
  "project"  #,
  #"desired"
)

columns_to_write_idx_location   <- c(
  "loc_id",
  "category",
  #"match_keyword",
  #"loc_name",
  #"loc_abbrevated",
  #"loc_deactivated_flag",
  #"loc_deleted_flag",
  "project" #,
  #"desired"
)

columns_to_write_idx_sched_location  <- c(
  "loc_id",
  "category",
  #"match_keyword",
  #"loc_name",
  #"loc_abbrevated",
  #"loc_billing_name",
  #"loc_clinic_name",
  #"loc_deactivated_flag",
  #"loc_deleted_flag",
  "project" #,
  #"desired"
)

