------------------------------------------------------------------------

# Living Data Rescue: Harvey Janszen Legacy Project

This repository contains data entry templates, digitization protocols, processing scripts, and digitized data outputs for the field notes of Harvey Janszen

## The three types of data entered & processed by these scripts and protocols:

1)  Observation-only occurrence data (in field journals)

2)  Field notes of collected specimens (in field journals)

3)  Digitized herbarium label data (on specimen photographs)

## Data Sources

1.  [**Field Journals**](https://github.com/emench/Harvey_Janszen_Legacy_Project/tree/main/data/existing_data/field-journal-photos)

    -   [HJ5 = Flora of Saturna (1973-1981)](https://github.com/emench/Harvey_Janszen_Legacy_Project/tree/main/data/existing_data/field-journal-photos/HJ5)

    -   [HJ7 = Journal ca. 1981-1996](https://github.com/emench/Harvey_Janszen_Legacy_Project/tree/main/data/existing_data/field-journal-photos/HJ7)

    -   [HJ8 = Journal ca. 1996-2000](https://github.com/emench/Harvey_Janszen_Legacy_Project/tree/main/data/existing_data/field-journal-photos/HJ8)

    -   [HJ9 = Unbound field notes ca. 1999-2003](https://github.com/emench/Harvey_Janszen_Legacy_Project/tree/main/data/existing_data/field-journal-photos/HJ9)

    -   [HJ27 = Field notes ca. 2003-2017](https://github.com/emench/Harvey_Janszen_Legacy_Project/tree/main/data/existing_data/field-journal-photos/HJ27)

2.  **Collected specimens**

    -   Royal British Columbia Museum (RBCM) Herbarium (\~1263 specimens)

        -   Specimens labels

        -   Digitized collection label data in database

    -   University of British Columbia (UBC) Herbarium (\~ 50 specimens)

    -   \~ 65 specimens from later in Harvey's life to be accesseioned at the UBC Herbarium

## General workflow

DwC = [Darwin Core occurrence data standards](https://dwc.tdwg.org/list/#dwc_fieldNotes)

GBIF = [Global Biodiversity Information Facility](https://www.gbif.org/)

## ![A path diagram of this repository for the Harvey Janszen Legacy Project](diagrams/LDP-Project-map.png)

## Ultimate goals for this project

1.  **To publish observation-only (occurrence) data to GBIF** *(potentially via Canadensys)*

2.  **Clean/ review berbarium records** for Harvey's collections housed at the RBCM *(ultimately published to GBIF by the RBCM Herbarium)*

3.  **Deposit & accession \~ 65 new specimens at the UBC Herbarium** *(ultimately published to GBIF by the UBC Herbarium)*

## Grouped Components

1.  **Observation-only occurrence data**

    *Data entry*

    -   [occ-data-entry-template](https://github.com/emench/Harvey_Janszen_Legacy_Project/blob/main/data/data_digitization/occurrence_data/1_raw_data/HJ-occ-entry-template.xlsx) (template to enter data)

    -   [1_raw_data](https://github.com/emench/Harvey_Janszen_Legacy_Project/tree/main/data/data_digitization/occurrence_data/1_raw_data) folder (houses the data entry files for respective journals)

    -   [occ-data-entry-protocol](https://github.com/emench/Harvey_Janszen_Legacy_Project/blob/main/protocols/occurrence_data/occ-data-entry-protocol.Rmd) (follow this protocol to enter data)

    *Data processing*

    -   [post-entry-processing](https://github.com/emench/Harvey_Janszen_Legacy_Project/blob/main/protocols/occurrence_data/post-entry-processing-protocol.Rmd) (follow this protocol to process entered data)

    *Output*

    -   [CSV files of Darwin Core formatted occurrences found here](https://github.com/emench/Harvey_Janszen_Legacy_Project/tree/main/data/data_digitization/occurrence_data/darwin_core_data)

2.  **Collection data**

    *Data entry*

    -   [coll-data-entry-template](https://github.com/emench/Harvey_Janszen_Legacy_Project/blob/main/data/data_digitization/collection_data/1_raw_data/HJ-coll-entry%20template.xlsx) (template to enter data)

    -   [1_raw_data](https://github.com/emench/Harvey_Janszen_Legacy_Project/tree/main/data/data_digitization/collection_data/1_raw_data) folder (houses the data entry files for respective journals)

    -   [coll-data-entry-protocol](https://github.com/emench/Harvey_Janszen_Legacy_Project/tree/main/protocols/collection_data) (follow this protocol to enter data)

    *Data processing*

    -   [post-entry-processing](https://github.com/emench/Harvey_Janszen_Legacy_Project/blob/main/protocols/collection_data/coll-data-entry-protocol.Rmd) (follow this protocol to process entered data)

    *Output*

    -   [CSV files of Darwin Core formatted collection data found here](https://github.com/emench/Harvey_Janszen_Legacy_Project/tree/main/data/data_digitization/collection_data/darwin_core_data)

3.  **RBCM data checking** \|\|\|\|\| **[under construction] \|\|\|\|**

    *Label data entry* \|\|\|\|\| **[under construction] \|\|\|\|**

    -   [RBCM-label-entry.xlsx](https://github.com/emench/Harvey_Janszen_Legacy_Project/blob/main/data/data_digitization/rbcm_data/label_data/raw_data/RBCM-label-entry.xlsx)

    -   [label-data-entry-protocol](https://github.com/emench/Harvey_Janszen_Legacy_Project/blob/main/protocols/rbcm_review/label-entry-protocol.Rmd).Rmd

    *Processing & comparing labels to database* \|\|\|\|\| **[under construction] \|\|\|\|**

    -   [part-1_label-processing.R](https://github.com/emench/Harvey_Janszen_Legacy_Project/blob/main/scripts/rbcm_data_processing/label-data/part-1_label-processing.R)

    -   [part-2_label-to-database-comparison.R](https://github.com/emench/Harvey_Janszen_Legacy_Project/blob/main/scripts/rbcm_data_processing/label-data/part-2_label-to-database-comparison.R)

    *Processing & reviewing* *field notes to database* \|\|\|\|\| **[under construction] \|\|\|\|**

    -   [part-1_field-note-processing.R](https://github.com/emench/Harvey_Janszen_Legacy_Project/blob/main/scripts/rbcm_data_processing/field-note-data/part1_field-note-processing.R)

    -   [part-2_field-note-database-comparison.R](https://github.com/emench/Harvey_Janszen_Legacy_Project/blob/main/scripts/rbcm_data_processing/field-note-data/part2-field-note-database-comparison.R)

4.  **Observation-only & Collection data aggregation**

    -   [total-occurrence-aggregation.R](https://github.com/emench/Harvey_Janszen_Legacy_Project/tree/main/scripts/aggregation)

# Recommended workflows

## [For observation-only (occurrence) data](https://github.com/emench/Harvey_Janszen_Legacy_Project/tree/main/protocols/occurrence_data)

## [For collection data](https://github.com/emench/Harvey_Janszen_Legacy_Project/tree/main/protocols/collection_data)

-   To further use this data to review RBCM herbarium data, follow the protocols [here](https://github.com/emench/Harvey_Janszen_Legacy_Project/tree/main/protocols/rbcm_review)
