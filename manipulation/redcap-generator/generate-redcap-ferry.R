generate_redcap_ferry <- function(
    schema_data     = NULL,
    dictionary_path = "./data-public/metadata/redcap-data-dictionary.csv",
    output_dir      = "manipulation/redcap-ferry",
    root_table      = NULL,
    link_column     = "mrn_mpi"
) {

  # ---- load-packages ---------------------------------------------------------
  requireNamespace("dplyr")
  requireNamespace("readr")
  requireNamespace("stringr")
  requireNamespace("glue")
  requireNamespace("purrr")
  requireNamespace("tools")
  requireNamespace("fs")
  requireNamespace("config")

  config <- config::get()

  # ---- load-schema-data ------------------------------------------------------
  if (is.null(schema_data)) {
    if (!file.exists(dictionary_path)) {
      stop("No schema_data provided and dictionary not found at: ", dictionary_path,
           "\nRun generate_data_dictionary() first.")
    }

    message("Loading data dictionary from: ", dictionary_path)
    dict <- readr::read_csv(dictionary_path, show_col_types = FALSE)

    # Reconstruct minimal schema_data from dictionary
    schema_data <- dict |>
      dplyr::select(
        table_name = `Form Name`,
        adjusted_col_name = `Variable / Field Name`
      ) |>
      dplyr::mutate(column_name = adjusted_col_name)

    # Set defaults for attributes
    attr(schema_data, "repeat_instruments") <- NULL
    attr(schema_data, "order_by_columns") <- NULL
    attr(schema_data, "id_columns") <- NULL
    attr(schema_data, "link_column") <- link_column
    attr(schema_data, "root_table") <- NULL

    message("  Loaded ", nrow(schema_data), " fields from ", length(unique(schema_data$table_name)), " tables")
  }

  # ---- get-attributes --------------------------------------------------------
  repeat_instruments <- attr(schema_data, "repeat_instruments")
  order_by_columns <- attr(schema_data, "order_by_columns")
  id_columns <- attr(schema_data, "id_columns")
  link_column <- attr(schema_data, "link_column") %||% link_column

  if (is.null(root_table)) {
    root_table <- attr(schema_data, "root_table")
  }

  all_tables <- unique(schema_data$table_name)

  if (is.null(root_table)) {
    # Auto-detect root table
    root_patterns <- c("^pt_pool$", "^patient_pool$", "^patient$", "^pt$", "^pt_", "^patient_")
    for (pattern in root_patterns) {
      matches <- all_tables[grepl(pattern, all_tables, ignore.case = TRUE)]
      if (length(matches) > 0) {
        root_table <- matches[1]
        break
      }
    }
    if (is.null(root_table)) {
      root_table <- sort(all_tables)[1]
    }
    message("Using root table: '", root_table, "'")
  }

  # ---- auto-exclude-staging-tables -------------------------------------------
  # Automatically exclude tables starting with "ss_"
  auto_exclude <- all_tables[grepl("^ss_", all_tables, ignore.case = TRUE)]

  if (length(auto_exclude) > 0) {
    message("\n--- Auto-excluding staging tables (ss_*) ---")
    message("  ", paste(auto_exclude, collapse = ", "))
  }

  # ---- prompt-for-additional-exclusions --------------------------------------
  exclude_tables <- auto_exclude

  if (interactive()) {
    message("\n", strrep("=", 60))
    message("TABLE EXCLUSION")
    message(strrep("=", 60))

    # Show all tables with numbers
    remaining_tables <- setdiff(all_tables, auto_exclude)
    message("\nTables available for ferry (excluding ss_* staging tables):\n")
    for (i in seq_along(remaining_tables)) {
      root_marker <- if (remaining_tables[i] == root_table) " [ROOT]" else ""
      message("  ", sprintf("%2d", i), ". ", remaining_tables[i], root_marker)
    }

    message("\n", strrep("-", 60))
    message("Enter additional table names to EXCLUDE from the ferry.")
    message("These tables will NOT have SQL scripts or ferry code generated.")
    message(strrep("-", 60))
    message("\nFormat: Type table names separated by commas (no quotes needed)")
    message("Example: lookup_codes, reference_data, archive_patients")
    message("\nPress ENTER with no input to include all tables listed above.")
    message("")

    user_input <- readline(prompt = "Tables to exclude: ")

    if (nchar(trimws(user_input)) > 0) {
      # Parse user input - handle various formats
      user_exclude <- trimws(strsplit(user_input, ",")[[1]])
      user_exclude <- user_exclude[nchar(user_exclude) > 0]

      # Validate against available tables
      valid_exclude <- user_exclude[user_exclude %in% remaining_tables]
      invalid_exclude <- user_exclude[!user_exclude %in% remaining_tables]

      if (length(invalid_exclude) > 0) {
        message("\nWarning: These tables were not found and will be ignored:")
        message("  ", paste(invalid_exclude, collapse = ", "))
      }

      if (length(valid_exclude) > 0) {
        exclude_tables <- c(auto_exclude, valid_exclude)
        message("\nAdditional exclusions: ", paste(valid_exclude, collapse = ", "))
      }
    }

    message(strrep("=", 60), "\n")
  }

  # ---- apply-exclusions ------------------------------------------------------
  tables_to_process <- setdiff(all_tables, exclude_tables)

  if (length(tables_to_process) == 0) {
    stop("No tables remaining after exclusions!")
  }

  message("Tables to process: ", paste(tables_to_process, collapse = ", "))

  # Filter schema_data
  schema_data_filtered <- schema_data |>
    dplyr::filter(table_name %in% tables_to_process)

  # ---- setup-directories -----------------------------------------------------
  fs::dir_create(output_dir)
  backup_dir <- file.path(output_dir, ".backups")
  fs::dir_create(backup_dir)

  # ---- generate-sql-scripts --------------------------------------------------
  message("\n--- Generating SQL scripts ---")

  tables <- split(schema_data_filtered, schema_data_filtered$table_name)

  purrr::walk2(tables, names(tables), function(table_data, table_name) {

    filename <- stringr::str_replace_all(table_name, "_", "-")
    file_path <- fs::path(output_dir, paste0(filename, ".sql"))

    # Table alias (first letter, lowercase)
    table_alias <- tolower(substr(table_name, 1, 1))

    # Check if repeat instrument
    is_repeat <- !is.null(repeat_instruments) && isTRUE(repeat_instruments[table_name])

    # Get order_by column
    order_by_col <- if (!is.null(order_by_columns) && !is.na(order_by_columns[table_name])) {
      order_by_columns[table_name]
    } else {
      # Try to find a date column
      date_cols <- table_data$column_name[
        grepl("date|time|instant|_dt$|_dts$|_datetime$", table_data$column_name, ignore.case = TRUE)
      ]
      if (length(date_cols) > 0) date_cols[1] else NULL
    }

    # --- Build SELECT clause ---
    select_clauses <- purrr::map2_chr(
      table_data$column_name,
      table_data$adjusted_col_name,
      ~ {
        column_ref <- glue::glue("{table_alias}.{.x}")
        if (.x != .y) glue::glue("{column_ref} AS {.y}") else column_ref
      }
    )

    # Prepend record_id
    select_clauses <- c("i.record_id", select_clauses)

    # Add repeat instrument columns if needed
    if (is_repeat) {
      if (is.null(order_by_col)) {
        warning("  ", table_name, ": No date column for ordering! Using first column.")
        order_by_col <- table_data$column_name[1]
      }
      repeat_instance <- glue::glue(
        "row_number() over(partition by i.record_id order by {table_alias}.{order_by_col}) as redcap_repeat_instance"
      )
      repeat_instrument <- glue::glue("'{table_name}' as redcap_repeat_instrument")
      select_clauses <- c(select_clauses, repeat_instance, repeat_instrument)
      message("  ", table_name, ": Repeat instrument (order by ", order_by_col, ")")
    } else {
      message("  ", table_name, ": Standard table")
    }

    # Format SELECT with leading commas
    select_lines <- paste0("    , ", select_clauses)
    select_lines[1] <- sub("^\\s*,", "     ", select_lines[1])
    select_clause <- paste(c("SELECT", select_lines), collapse = "\n")

    # --- Build FROM clause ---
    from_clause <- glue::glue(
      "FROM {config$schema_name}.{table_name} {table_alias}\n",
      "    inner join cdw_transaction.{config$schema_name}.pt_identity i ",
      "ON {table_alias}.{link_column} = i.{link_column}"
    )

    sql_statement <- paste(select_clause, from_clause, sep = "\n")

    # --- Handle existing files (backup if changed) ---
    if (file.exists(file_path)) {
      existing_content <- paste(readLines(file_path, warn = FALSE), collapse = "\n")
      normalize <- function(x) gsub("\\s+", " ", trimws(x))

      if (normalize(existing_content) != normalize(sql_statement)) {
        backup_path <- file.path(backup_dir, paste0(
          tools::file_path_sans_ext(basename(file_path)), "_",
          format(Sys.time(), "%Y%m%d_%H%M%S"), ".sql"
        ))
        file.copy(file_path, backup_path)
        message("    Backup: ", basename(backup_path))
        writeLines(sql_statement, file_path)
      } else {
        message("    (unchanged)")
      }
    } else {
      writeLines(sql_statement, file_path)
    }
  })

  # ---- generate-ferry-script -------------------------------------------------
  message("\n--- Generating R ferry script ---")

  ferry_file <- file.path(output_dir, "redcap-ferry.R")

  # Find SQL files and parse them
  sql_files <- list.files(output_dir, pattern = "\\.sql$", full.names = TRUE)

  # Only include SQL files for tables we're processing
  sql_files <- sql_files[tools::file_path_sans_ext(basename(sql_files)) %in%
                           stringr::str_replace_all(tables_to_process, "_", "-")]

  if (length(sql_files) == 0) {
    stop("No SQL files found in: ", output_dir)
  }

  # --- SQL parsing functions ---
  split_sql_columns <- function(text) {
    chars <- strsplit(text, "")[[1]]
    depth <- 0
    current <- ""
    result <- character()

    for (char in chars) {
      if (char == "(") {
        depth <- depth + 1
        current <- paste0(current, char)
      } else if (char == ")") {
        depth <- depth - 1
        current <- paste0(current, char)
      } else if (char == "," && depth == 0) {
        result <- c(result, trimws(current))
        current <- ""
      } else {
        current <- paste0(current, char)
      }
    }
    if (nchar(trimws(current)) > 0) {
      result <- c(result, trimws(current))
    }
    return(result)
  }

  extract_column_name <- function(expr) {
    expr <- trimws(expr)

    # Check for AS alias
    as_match <- stringr::str_match(expr, stringr::regex("\\s+AS\\s+([\\w]+)\\s*$", ignore_case = TRUE))
    if (!is.na(as_match[1, 2])) return(as_match[1, 2])

    # Check for implicit alias after closing paren
    implicit_match <- stringr::str_match(expr, "\\)\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\s*$")
    if (!is.na(implicit_match[1, 2])) return(implicit_match[1, 2])

    # Check for literal with alias
    literal_match <- stringr::str_match(
      expr, stringr::regex("'[^']*'\\s+(?:AS\\s+)?([a-zA-Z_][a-zA-Z0-9_]*)\\s*$", ignore_case = TRUE)
    )
    if (!is.na(literal_match[1, 2])) return(literal_match[1, 2])

    # Simple column: remove table prefix
    simple <- stringr::str_replace(expr, "^[a-zA-Z_][a-zA-Z0-9_]*\\.", "")
    simple <- trimws(simple)

    if (stringr::str_detect(simple, "^[a-zA-Z_][a-zA-Z0-9_]*$")) return(simple)

    warning("Could not parse: ", expr)
    return(expr)
  }

  parse_sql_columns <- function(sql_text) {
    match <- stringr::str_match(
      sql_text, stringr::regex("SELECT\\s+(.*?)\\s+FROM\\s+", ignore_case = TRUE, dotall = TRUE)
    )
    if (is.na(match[1, 2])) return(NULL)

    columns <- split_sql_columns(match[1, 2])
    sapply(columns, extract_column_name, USE.NAMES = FALSE)
  }

  # --- Process SQL files ---
  process_sql_file <- function(sql_path) {
    script_name <- tools::file_path_sans_ext(basename(sql_path))
    object_name <- paste0("ds_", stringr::str_replace_all(script_name, "-", "_"))
    rel_path <- file.path(output_dir, basename(sql_path))

    sql_text <- paste(readLines(sql_path, warn = FALSE), collapse = "\n")
    column_names <- parse_sql_columns(sql_text)

    if (is.null(column_names) || length(column_names) == 0) {
      warning("Could not parse columns from: ", sql_path)
      return(NULL)
    }

    is_repeat <- "redcap_repeat_instance" %in% column_names

    list(
      script_name = script_name,
      object_name = object_name,
      rel_path = rel_path,
      column_names = column_names,
      is_repeat = is_repeat
    )
  }

  data_objects <- purrr::compact(purrr::map(sql_files, process_sql_file))

  if (length(data_objects) == 0) {
    stop("No SQL files could be parsed successfully")
  }

  # Sort: root table first
  root_idx <- which(sapply(data_objects, function(x) {
    stringr::str_replace_all(x$script_name, "-", "_") == root_table
  }))
  if (length(root_idx) > 0) {
    data_objects <- c(data_objects[root_idx], data_objects[-root_idx])
  }

  # --- Build script sections ---
  header <- c(
    'rm(list = ls(all.names = TRUE)) # Clear memory',
    '',
    '# ---- load-packages -----------------------------------------------------------',
    'requireNamespace("dplyr")',
    'requireNamespace("config")',
    'requireNamespace("REDCapR")',
    'requireNamespace("OuhscMunge")',
    '',
    '# ---- declare-globals ---------------------------------------------------------',
    'config <- config::get()',
    'credential <- REDCapR::retrieve_credential_mssql(',
    '  dsn        = config$dsn_security,',
    '  project_id = config$redcap_pid,',
    '  instance   = config$redcap_instance',
    ')',
    '',
    '# ---- load-data ---------------------------------------------------------------'
  )

  load_data <- purrr::map_chr(data_objects, function(x) {
    glue::glue('{x$object_name} <- OuhscMunge::execute_sql_file("{x$rel_path}", config$dsn_staging, execute = FALSE)')
  })

  groom_sections <- purrr::map(data_objects, function(x) {
    col_lines <- paste0("    ", x$column_names)
    col_lines <- paste(col_lines, collapse = ",\n")
    c('', glue::glue('# ---- groom-{x$script_name} ----'),
      glue::glue('{x$object_name} <-'), glue::glue('  {x$object_name} |>'),
      '  dplyr::select(', col_lines, '  )')
  })
  groom <- unlist(groom_sections)

  verify_header <- c('', '# ---- verify-values -----------------------------------------------------------')
  verify <- purrr::map_chr(data_objects, ~ glue::glue('OuhscMunge::verify_value_headstart({.x$object_name})'))

  upload_header <- c('', '# ---- upload-to-redcap --------------------------------------------------------',
                     '# stop("Uncomment to block uploads during development")', '')
  upload_sections <- purrr::map(data_objects, function(x) {
    c(glue::glue('# Upload {x$script_name}'),
      'REDCapR::redcap_write(',
      glue::glue('  ds_to_write = {x$object_name},'),
      '  redcap_uri  = credential$redcap_uri,',
      '  token       = credential$token,',
      '  convert_logical_to_integer = TRUE,',
      '  batch_size  = 1000L', ')', '')
  })
  upload <- unlist(upload_sections)

  script <- c(header, load_data, groom, verify_header, verify, upload_header, upload)

  # --- Preserve custom sections from existing file ---
  if (file.exists(ferry_file)) {
    existing_lines <- readLines(ferry_file, warn = FALSE)

    # Extract custom sections (marked with # ---- custom-* ----)
    custom_sections <- list()
    in_custom <- FALSE
    current_section <- character()
    current_name <- NULL

    for (line in existing_lines) {
      if (grepl("^# ---- custom-", line)) {
        in_custom <- TRUE
        current_name <- line
        current_section <- line
      } else if (in_custom && grepl("^# ---- [a-z]", line) && !grepl("^# ---- custom-", line)) {
        in_custom <- FALSE
        if (length(current_section) > 1) custom_sections[[current_name]] <- current_section
        current_section <- character()
        current_name <- NULL
      } else if (in_custom) {
        current_section <- c(current_section, line)
      }
    }
    if (in_custom && length(current_section) > 1) {
      custom_sections[[current_name]] <- current_section
    }

    if (length(custom_sections) > 0) {
      message("Preserving ", length(custom_sections), " custom section(s)")
      upload_idx <- which(grepl("^# ---- upload-to-redcap", script))
      if (length(upload_idx) > 0) {
        script <- c(script[1:(upload_idx - 1)], "", unlist(custom_sections), script[upload_idx:length(script)])
      }
    }

    # Backup existing file if changed
    if (!identical(existing_lines, script)) {
      backup_path <- file.path(backup_dir, paste0(
        tools::file_path_sans_ext(basename(ferry_file)), "_",
        format(Sys.time(), "%Y%m%d_%H%M%S"), ".R"
      ))
      file.copy(ferry_file, backup_path)
      message("Backup created: ", basename(backup_path))
    }
  }

  writeLines(script, con = ferry_file)
  message("Generated: ", ferry_file)

  # --- Add .gitignore for backups ---
  gitignore_path <- file.path(output_dir, ".gitignore")
  if (!file.exists(gitignore_path)) {
    writeLines(".backups/", gitignore_path)
  } else {
    gitignore_content <- readLines(gitignore_path, warn = FALSE)
    if (!".backups/" %in% gitignore_content) {
      writeLines(c(gitignore_content, ".backups/"), gitignore_path)
    }
  }

  # --- Summary ---
  message("\n", strrep("=", 60))
  message("Ferry generation complete!")
  message(strrep("=", 60))
  message("\nProcessed ", length(data_objects), " tables:")
  purrr::walk(data_objects, function(x) {
    repeat_label <- if (x$is_repeat) " (repeat)" else ""
    message("  - ", x$script_name, ": ", length(x$column_names), " columns", repeat_label)
  })

  if (length(exclude_tables) > 0) {
    message("\nExcluded tables: ", paste(exclude_tables, collapse = ", "))
  }

  message("\nGenerated files:")
  message("  - ", output_dir, "/*.sql (", length(sql_files), " files)")
  message("  - ", ferry_file)

  message("\nNext steps:")
  message("  1. Review generated SQL scripts in ", output_dir)
  message("  2. Run: source('", ferry_file, "')")

  invisible(data_objects)
}
