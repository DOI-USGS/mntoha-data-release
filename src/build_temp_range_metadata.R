
#' Using whisker templating and the existing table that details each of the temp ranges used
#' to generate the metadata describing the column name, the definition, where it came from,
#' the min and max of the column, and what units are used.
#' NOTE: I had to do a manual step once this was generated to copy into the
generate_temp_range_metadata <- function(out_file, in_file) {

  fill_data <- readr::read_tsv(in_file) %>%
    mutate(description_details = sprintf(
      "This range is used to denote %s %s", `Specific metric`,
      ifelse(!is.na(`Fish thermal guild`), sprintf("(%s thermal guild)", `Fish thermal guild`), "")))

  fill_data_list <- lapply(split(fill_data, seq(nrow(fill_data))), as.list)

  temp_range_template <- "
    -
      attr-label: height_{{Temp_Low}}_{{Temp_High}}
      attr-def: >-
        Meters of water between {{Temp_Low}} and {{Temp_High}} degrees C.
        {{description_details}}
      attr-defs: {{Source}}
      data-min: NA
      data-max: NA
      data-units: linear distance (m)
    -
      attr-label: vol_{{Temp_Low}}_{{Temp_High}}
      attr-def: >-
        Volume of water between {{Temp_Low}} and {{Temp_High}} degrees C.
        {description_details}}
      attr-defs: {{Source}}
      data-min: NA
      data-max: NA
      data-units: volume (m^3*1000)
    -
      attr-label: days_{{Temp_Low}}_{{Temp_High}}
      attr-def: >-
        Days in which there is any part of water column between {{Temp_Low}} and {{Temp_High}} degrees C.
        {{description_details}}
      attr-defs: {{Source}}
      data-min: NA
      data-max: NA
      data-units: days"

  rendered_metadata <- lapply(fill_data_list, whisker.render, template = temp_range_template)
  writeLines(unlist(rendered_metadata), out_file)

  message(sprintf("NOW MANUALLY COPY THE CONTENTS OF %s INTO `in_text/text_07_habitat.yml`", out_file))

}


