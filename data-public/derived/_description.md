Project: `cdw-skeleton-1`
============================

Data Extracts from the BBMC CDW

3 datasets were derived from the CDW and saved as separate csvs.
The collection of datasets is described in the file `_summary.csv`
which can be opened in Excel, Notepad++, or any program that can read plain text.

The datasets were saved by Will Beasley at 2019-03-31 23:58:29.

|pass |path_output                      |
|:----|:--------------------------------|
|TRUE |data-public/derived/person.csv   |
|TRUE |data-public/derived/obs.csv      |
|TRUE |data-public/derived/medicate.csv |

- <br/><b>path_output</b>: data-public/derived/person.csv
  <br/><b>row_unit</b>: each row represents a distinct patient.
  <br/><b>dimensions</b>: 'Table dim (c x r): 4 x 6  - 2.3 KiB'
  <br/><b>check</b>: Pass
  <br/><b>sql</b>: <code><br/>SELECT<br/> * <br/>FROM main.person</code>
- <br/><b>path_output</b>: data-public/derived/obs.csv
  <br/><b>row_unit</b>: each row represents a distinct measurement
  <br/><b>dimensions</b>: 'Table dim (c x r): 4 x 4  - 1.8 KiB'
  <br/><b>check</b>: Pass
  <br/><b>sql</b>: <code><br/>SELECT<br/> mrn_flowcast, obs_date, obs_term, obs_value <br/>FROM
    main.obs</code>
- <br/><b>path_output</b>: data-public/derived/medicate.csv
  <br/><b>row_unit</b>: each row represents a distinct prescription
  <br/><b>dimensions</b>: 'Table dim (c x r): 5 x 2  - 1.8 KiB'
  <br/><b>check</b>: Pass
  <br/><b>sql</b>: <code><br/>SELECT<br/> * <br/>FROM main.medicate</code>


