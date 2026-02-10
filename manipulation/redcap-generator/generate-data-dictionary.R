generate_data_dictionary <- function(
    cnn              = NULL,
    schema_name      = NULL,
    output_path      = "./data-public/metadata/redcap-data-dictionary.csv",
    root_table       = NULL,
    link_column      = "mrn_mpi",
    preserve_edits   = TRUE
) {

# ---- load-packages ---------------------------------------------------------
requireNamespace("DBI")
requireNamespace("odbc")
requireNamespace("dplyr")
requireNamespace("readr")
requireNamespace("glue")
requireNamespace("purrr")
requireNamespace("stringr")
requireNamespace("tools")
requireNamespace("config")

# ---- load-config -----------------------------------------------------------
config <- config::get()

  if (is.null(schema_name)) {
    schema_name <- config$schema_name
  }

  if (is.null(schema_name)) {
    stop("schema_name must be provided or set in config.yml")
  }

  # ---- connect-database ------------------------------------------------------
  close_connection <- FALSE
  if (is.null(cnn)) {
    message("Connecting to database via DSN: ", config$dsn_staging)
    cnn <- DBI::dbConnect(odbc::odbc(), config$dsn_staging)
    close_connection <- TRUE
  }

  on.exit({
    if (close_connection && DBI::dbIsValid(cnn)) {
      DBI::dbDisconnect(cnn)
    }
  })

# ---- user-editable-columns -------------------------------------------------
user_editable_cols <- c(
  "Field Type",
  "Field Label",
  "Choices, Calculations, OR Slider Labels",
  "Field Note",
  "Text Validation Type OR Show Slider Number",
  "Text Validation Min",
  "Text Validation Max",
  "Identifier?",
  "Branching Logic (Show field only if...)",
  "Required Field?",
  "Custom Alignment",
  "Question Number (surveys only)",
  "Matrix Group Name",
  "Matrix Ranking?",
  "Field Annotation",
  "Section Header"
)

# ---- load-existing-dictionary ----------------------------------------------
existing_dict <- NULL
if (preserve_edits && file.exists(output_path)) {
  message("Found existing data dictionary: ", output_path)
  existing_dict <- readr::read_csv(output_path, show_col_types = FALSE)
  message("  Loaded ", nrow(existing_dict), " existing field definitions")
}

# ---- query-schema ----------------------------------------------------------
message("\nQuerying schema: '", schema_name, "'")
query <- paste0(
   "SELECT
     TABLE_SCHEMA AS schema_name
     ,TABLE_NAME AS table_name
     ,COLUMN_NAME AS column_name
   FROM INFORMATION_SCHEMA.COLUMNS
   WHERE TABLE_SCHEMA = '", schema_name, "'
   ORDER BY TABLE_NAME, ORDINAL_POSITION"
 )
schema_data <- DBI::dbGetQuery(cnn, query)

if (nrow(schema_data) > 0) {
   schemas_returned <- unique(schema_data$schema_name)
   if (length(schemas_returned) > 1 || !all(schemas_returned == schema_name)) {
     warning("Query returned unexpected schemas: ", paste(schemas_returned, collapse = ", "))
     schema_data <- schema_data |>
       dplyr::filter(schema_name == !!schema_name)
   }
 }
 if (nrow(schema_data) == 0) {
   stop("No tables found in schema '", schema_name, "'. Verify the schema exists.")
 }
tables_found <- unique(schema_data$table_name)
message("Found ", length(tables_found), " tables: ", paste(tables_found, collapse = ", "))

# ---- detect-root-table -----------------------------------------------------
detect_root_table <- function(table_names) {
   root_patterns <- c(
     "^pt_pool$",
     "^patient_pool$",
     "^patient$",
     "^pt$",
     "^pt_",
     "^patient_"
   )
   for (pattern in root_patterns) {
     matches <- table_names[grepl(pattern, table_names, ignore.case = TRUE)]
     if (length(matches) > 0) {
       return(matches[1])
     }
   }
   return(NULL)
 }

if (is.null(root_table)) {
   root_table <- detect_root_table(tables_found)
   if (is.null(root_table)) {
     root_table <- sort(tables_found)[1]
   }
}

# ---- prompt-to-verify-root-table -------------------------------------------
if (interactive()) {
  message("\n", strrep("=", 60))
  message("VERIFY ROOT TABLE")
  message(strrep("=", 60))
  message("\nDetected root table: ", root_table)
  message("\nPress ENTER to accept, or type a different table name:")
  user_input <- readline(prompt = "Root table: ")
  if (nchar(trimws(user_input)) > 0) {
    if (trimws(user_input) %in% tables_found) {
      root_table <- trimws(user_input)
      message("Using: ", root_table)
    } else {
      warning("Table '", trimws(user_input), "' not found. Using detected: ", root_table)
    }
  } else {
    message("Using: ", root_table)
  }
}

# ---- prompt-to-verify-link-column ------------------------------------------
if (interactive()) {
  message("\n", strrep("=", 60))
  message("VERIFY LINK COLUMN")
  message(strrep("=", 60))
  message("\nLink column (joins tables to pt_identity): ", link_column)
  message("\nPress ENTER to accept, or type a different column name:")
  user_input <- readline(prompt = "Link column: ")
  if (nchar(trimws(user_input)) > 0) {
    link_column <- trimws(user_input)
    message("Using: ", link_column)
  } else {
    message("Using: ", link_column)
  }
  message(strrep("=", 60), "\n")
}

# ---- detect-repeat-instruments ---------------------------------------------
message("\n--- Analyzing tables for repeat instruments ---")

repeat_info <- purrr::map(tables_found, function(tbl) {
  tbl_columns <- schema_data$column_name[schema_data$table_name == tbl]

  if (tbl == root_table) {
    message("  ", tbl, ": Root table (not a repeat instrument)")
    return(list(is_repeat = FALSE, order_by = NULL, id_column = NULL, reason = "root_table"))
  }

  id_col <- NULL
  if (link_column %in% tbl_columns) {
    id_col <- link_column
  } else {
    mrn_cols <- tbl_columns[grepl("^mrn|_mrn|mrn_", tbl_columns, ignore.case = TRUE)]
    if (length(mrn_cols) > 0) {
      id_col <- mrn_cols[1]
    } else {
      record_cols <- tbl_columns[grepl("record_id|recordid", tbl_columns, ignore.case = TRUE)]
      if (length(record_cols) > 0) {
        id_col <- record_cols[1]
      } else {
        id_cols <- tbl_columns[grepl("_id$|^id$", tbl_columns, ignore.case = TRUE)]
        patient_ids <- id_cols[grepl("patient|pt|subject|person", id_cols, ignore.case = TRUE)]
        if (length(patient_ids) > 0) {
          id_col <- patient_ids[1]
        } else if (length(id_cols) > 0) {
          id_col <- id_cols[1]
        }
      }
    }
  }

  if (is.null(id_col)) {
    message("  ", tbl, ": No ID column found - skipping repeat detection")
    return(list(is_repeat = FALSE, order_by = NULL, id_column = NULL, reason = "no_id_column"))
  }

  check_query <- glue::glue(
    "SELECT CASE WHEN EXISTS (
      SELECT {id_col} FROM {schema_name}.{tbl}
      GROUP BY {id_col} HAVING COUNT(*) > 1
    ) THEN 1 ELSE 0 END AS has_repeats"
  )

  has_repeats <- tryCatch({
    result <- DBI::dbGetQuery(cnn, check_query)
    as.logical(result$has_repeats)
  }, error = function(e) {
    message("  ", tbl, ": Error checking repeats - ", e$message)
    FALSE
  })

  date_cols <- tbl_columns[grepl("date|time|instant|_dt$|_dts$|_datetime$", tbl_columns, ignore.case = TRUE)]
  order_by <- if (length(date_cols) > 0) date_cols[1] else NULL

  if (has_repeats) {
    if (!is.null(order_by)) {
      message("  ", tbl, ": REPEAT INSTRUMENT (id: ", id_col, ", order by: ", order_by, ")")
    } else {
      message("  ", tbl, ": REPEAT INSTRUMENT (id: ", id_col, ") [WARNING: No date column for ordering]")
    }
  } else {
    message("  ", tbl, ": Single row per ", id_col)
  }

  list(is_repeat = has_repeats, order_by = order_by, id_column = id_col, reason = "checked")
})
names(repeat_info) <- tables_found

repeat_instruments <- purrr::map_lgl(repeat_info, ~ .x$is_repeat)
order_by_columns <- purrr::map_chr(repeat_info, ~ .x$order_by %||% NA_character_)
id_columns <- purrr::map_chr(repeat_info, ~ .x$id_column %||% NA_character_)

message("--- End repeat instrument analysis ---\n")

# ---- mark-primary-table ----------------------------------------------------
schema_data <- schema_data |>
  dplyr::mutate(is_primary = (table_name == root_table))

# ---- handle-duplicate-column-names -----------------------------------------
duplicate_vars <- schema_data |>
  dplyr::group_by(column_name) |>
  dplyr::filter(dplyr::n() > 1) |>
  dplyr::ungroup()

schema_data <- schema_data |>
  dplyr::mutate(
    adjusted_col_name = dplyr::if_else(
      column_name %in% duplicate_vars$column_name & !is_primary,
      paste0(column_name, "_", table_name),
      column_name
    ),
    field_label = stringr::str_replace_all(adjusted_col_name, "_", " ") |>
      stringr::str_to_title()
  )

# ---- build-data-dictionary -------------------------------------------------
redcap_columns <- c(
    "Variable / Field Name", "Form Name", "Section Header", "Field Type",
    "Field Label", "Choices, Calculations, OR Slider Labels", "Field Note",
    "Text Validation Type OR Show Slider Number", "Text Validation Min",
    "Text Validation Max", "Identifier?", "Branching Logic (Show field only if...)",
    "Required Field?", "Custom Alignment", "Question Number (surveys only)",
    "Matrix Group Name", "Matrix Ranking?", "Field Annotation"
  )

data_dict <- schema_data |>
    dplyr::mutate(
      `Variable / Field Name` = adjusted_col_name,
      `Form Name` = table_name,
      `Field Label` = field_label,
      `Field Type` = "text",
      `Choices, Calculations, OR Slider Labels` = "",
      `Field Note` = "",
      `Text Validation Type OR Show Slider Number` = "",
      `Text Validation Min` = "",
      `Text Validation Max` = "",
      `Identifier?` = "",
      `Branching Logic (Show field only if...)` = "",
      `Required Field?` = "",
      `Custom Alignment` = "",
      `Question Number (surveys only)` = "",
      `Matrix Group Name` = "",
      `Matrix Ranking?` = "",
      `Field Annotation` = "@READONLY",
      `Section Header` = ""
    ) |>
    dplyr::select(dplyr::all_of(redcap_columns))

# ---- add-record-id-and-reorder ---------------------------------------------
message("\nAdding record_id field and reordering...")

# Create record_id row for the root table
record_id_row <- tibble::tibble(
  `Variable / Field Name` = "record_id",
  `Form Name` = root_table,
  `Section Header` = "",
  `Field Type` = "text",
  `Field Label` = "Record ID",
  `Choices, Calculations, OR Slider Labels` = "",
  `Field Note` = "",
  `Text Validation Type OR Show Slider Number` = "",
  `Text Validation Min` = "",
  `Text Validation Max` = "",
  `Identifier?` = "",
  `Branching Logic (Show field only if...)` = "",
  `Required Field?` = "",
  `Custom Alignment` = "",
  `Question Number (surveys only)` = "",
  `Matrix Group Name` = "",
  `Matrix Ranking?` = "",
  `Field Annotation` = "",
)

# Split data_dict into root table and other tables
root_table_rows <- data_dict |>
  dplyr::filter(`Form Name` == root_table)

other_table_rows <- data_dict |>
  dplyr::filter(`Form Name` != root_table)

# Combine: record_id first, then root table rows, then other tables
data_dict <- dplyr::bind_rows(
  record_id_row,
  root_table_rows,
  other_table_rows
)

message("  Placed '", root_table, "' first with record_id as the first field")

# ---- merge-with-existing ---------------------------------------------------
  if (!is.null(existing_dict) && nrow(existing_dict) > 0) {
    message("Merging with existing data dictionary to preserve edits...")

    existing_fields <- existing_dict$`Variable / Field Name`
    new_fields <- data_dict$`Variable / Field Name`

    added_fields <- setdiff(new_fields, existing_fields)
    removed_fields <- setdiff(existing_fields, new_fields)
    common_fields <- intersect(new_fields, existing_fields)

    if (length(added_fields) > 0) message("  New fields: ", paste(added_fields, collapse = ", "))
    if (length(removed_fields) > 0) message("  Removed fields: ", paste(removed_fields, collapse = ", "))
    message("  Preserved fields: ", length(common_fields))

    for (col in user_editable_cols) {
      if (col %in% names(existing_dict)) {
        existing_values <- stats::setNames(existing_dict[[col]], existing_dict$`Variable / Field Name`)
        data_dict[[col]] <- purrr::map2_chr(
          data_dict$`Variable / Field Name`,
          data_dict[[col]],
          function(field_name, default_value) {
            existing_val <- existing_values[field_name]
            if (!is.na(existing_val) && nchar(trimws(as.character(existing_val))) > 0) {
              as.character(existing_val)
            } else {
              as.character(default_value)
            }
          }
        )
      }
    }

    if (length(removed_fields) > 0) {
      removed_rows <- existing_dict |>
        dplyr::filter(`Variable / Field Name` %in% removed_fields) |>
        dplyr::mutate(`Field Note` = paste0("[REMOVED FROM SCHEMA] ", `Field Note`))

      for (col in redcap_columns) {
        if (!col %in% names(removed_rows)) {
          removed_rows[[col]] <- NA_character_
        }
      }
      removed_rows <- removed_rows |> dplyr::select(dplyr::all_of(redcap_columns))
      data_dict <- dplyr::bind_rows(data_dict, removed_rows)
    }

    # Re-order after merge: record_id first, root table, then other tables
    message("  Re-ordering to maintain root table first...")
    record_id_row <- data_dict |> dplyr::filter(`Variable / Field Name` == "record_id")
    root_table_rows <- data_dict |>
      dplyr::filter(`Form Name` == root_table, `Variable / Field Name` != "record_id")
    other_rows <- data_dict |>
      dplyr::filter(`Form Name` != root_table, `Variable / Field Name` != "record_id")

    data_dict <- dplyr::bind_rows(record_id_row, root_table_rows, other_rows)
  }

# ---- save-output -----------------------------------------------------------
output_dir <- dirname(output_path)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

readr::write_csv(data_dict, output_path, na = "")
message("Data dictionary saved: ", output_path)
message("  Total fields: ", nrow(data_dict))

# ---- attach-attributes -----------------------------------------------------
attr(schema_data, "repeat_instruments") <- repeat_instruments
attr(schema_data, "order_by_columns") <- order_by_columns
attr(schema_data, "id_columns") <- id_columns
attr(schema_data, "link_column") <- link_column
attr(schema_data, "root_table") <- root_table

return(schema_data)
}
