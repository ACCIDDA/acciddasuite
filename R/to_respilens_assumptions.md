# `to_respilens()` assumptions and requirements
#### (for output to have functional visualization on MyRespiLens)

## General requirements/notes
* Forecast data (`model_out_tbl`) and corresponding ground truthdata (`oracle_output`) must be for only one location
* Target name in ground truth data must *exactly* match target name in forecast data
    * Any targets with term 'peak' in them will be filtered (MyRespiLens does not yet do peak forecast visualization)
* `output_type` in forecast data must contain some `quantile` values (MyRespiLens only visualizes quantile forecasts)
    * `sample` output type will be filtered out during processing
    * `pmf` output type will not be filtered out, but it will *not* display visualization on MyRespiLens
* Quantiles (found in `output_type_id`) must be represented as decimales (e.g., `0.50`, not `50` or `50%`)
* Quantiles must have "matches" for proper MyRespiLens visualization; i.e. `0.10` and `0.90`, not `0.10` and `0.80`. 


## `to_respilens()` parameter: `accidda_cast$hubcast` object
#### `$model_out_tbl`
| column | expected type |
|--------|---------------|
| `location` | character |
| `target` | character |
| `model_id` | character |
| `reference_date` | date-convertible character |
| `target_end_date` | date-convertible character |
| `horizon` | integer or character |
| `output_type` | character |
| `output_type_id` | numeric or character (depends on `output_type`) |
| `value` | numeric |

#### `$oracle_output`
| column | expected type |
|--------|---------------|
| `target_end_date` | date-convertible character |
| `target` | character |
| `oracle_value` | numeric |


## `loc_data` dependency 
The `to_respilens()` function relies on an external dependency (denoted as `loc_data`). `loc_data` is a tibble with one row per location, and `to_respilens()` requires the following columns:
| column | description |
|--------|-------------|
| `abbreviation` | Location abbreviation. E.g., `"NC"` (chr) |
| `location` | FIPS code for the location (unique identifier). E.g., `"37"` (chr)|
| `location_name` | Full location name. E.g., `"North Carolina"` (chr) |
| `population` | Population of the location (numeric) | 

`to_respilens()` will index the `loc_data` on the `location` column, so the location present in model data must have entries in `loc_data`'s columns.