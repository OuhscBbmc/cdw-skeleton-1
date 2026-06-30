# SQL Coding Style

## General Files

- Keywords lower case **except**:
  - `CREATE TABLE`
  - `DECLARE`
  - `DROP TABLE`
  - `FROM`
  - `GROUP BY`
  - `HAVING`
  - `INSERT`
  - `ORDER BY`, unless it's inside a window function
  - `SELECT`
  - `UNION` and `UNION ALL`
  - `VALUES`
  - `WITH`
  - `WHERE`

### SELECT clause

- Each variable on its own line.
- Subsequent variables follow a leading comma.
- `distinct` on its own line.

### FROM clause

- Joins indented and nested under `FROM`.
- Extra space in `left  join`.
- Single-variable join on one line:

  ```sql
  FROM patient p
    inner join visit v on p.patient_id = v.patient_id
  ```

- Multi-variable join: each condition on its own line; operators like `and` and `or` on their own line:

  ```sql
  FROM patient p
    inner join visit v on
      p.patient_id = v.patient_id
      and
      p.birth_date <= v.visit_date
  ```

### CREATE TABLE

- Nullable columns omit the `null` keyword — just the type is enough. Only `not null` needs to be stated explicitly.
- Pad column names with spaces so data types are vertically aligned.
- Pad data types with spaces so constraints are vertically aligned.
- Pad the comma with spaces so inline comments are vertically aligned.
- Use a trailing comma on the last column definition (before the closing `)`).
- Add a commented-out `--exec dbo.generate_create_table_sp '{schema_name}.{table_name}'`
  above each table definition so the user can quickly re-create it when making modifications.
- All `CREATE TABLE` statements for a script's output tables must appear before the first
  `SELECT` in the file.
- If a script uses a `#temp` table: define all permanent output tables first, then define and
  populate the temp table, then insert into the permanent tables.

### DECLARE block

- Pad variable names with spaces so data types are vertically aligned.
- Pad data types with spaces so values are vertically aligned (allow for types as long as `varchar(1234)`).
- Pad after the semicolon so inline comments are vertically aligned.
- Avoid hard-coded constants in the body of the script. Use `DECLARE` at the top instead.
- Common static variables:
  - `date_start_legacy` = 2010-01-01
  - `date_stop_legacy` = 2023-06-02
  - `date_start_epic` = 2023-06-03
  - `date_stop_epic` = `cast(getdate() as date)`

### INSERT

- Never include a column list. Use `INSERT table` followed directly by `SELECT`.

### WHERE clause

- Each condition on its own line where it aids readability.

### General

- Use CTEs for readability; use `#temp` tables when intermediary logic is reused enough that a CTE becomes unreadable.
- For the second and subsequent CTE in a file, start the line with a comma.

  Preferred Style:
  
  ```sql
  ...
  )
  ,cte_2 as (
    ...
  )
  ```
  
  Unfavorable Style:
  
  ```sql
  ...
  ),
  cte_2 as (
    ...
  )
  ```

- Every SQL script should produce one or more permanent project-schema tables as its final outputs.
  Intermediary tables should be CTEs or `#temp` tables.
- `ss-` files are an exception: study-specific lookup tables (ss_dx, ss_med, etc.) remain as separate staged tables when PI review is required.
- Avoid declaring obvious defaults (e.g., write `identity` instead of `identity(1, 1)`; write `primary key` instead of `not null primary key`).
- Trim trailing whitespace from every line.
- End every file with a newline character.
- Avoid three consecutive newline characters. If a blank line is needed, use only one.
- 2 spaces per indentation level; spaces not tabs.

## cdw_cache_staging Files

- Start the file with `use cdw_cache_staging;` when the destination table is in that database.
- Do not qualify tables in `cdw_cache_staging` with the database name — it's the default.
