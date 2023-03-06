# Aggregating Collection and Observation-Only Occurrence Data

When all collection data and observation-only occurrence data have been digitized and processed,

the final processing stage is to run this script:

<https://github.com/emench/Harvey_Janszen_Legacy_Project/blob/main/scripts/aggregation/total-occurrence-aggregation.R>

It will take collection and occurrence only data, assign associated
occurrences and taxa to the collection data, place this in the collected data folder,
and then aggregate both types of data into one large Darwin Core formatted
spreadsheet.

*So it outputs two main outputs:*

**1) Complete DwC formatted collection records** (because it now contains associated observation-only occurrences!)

**2) An aggregated DwC formatted file of collection and observation-only records for *internal use*** (not to be published to GBIF, since this would duplicate herbarium records on GBIF which we don't want!)

**NOTE:** to combine two sheets (collection data and occurrence data), the taxonomy
reference system used in part 3 (darwin core conversion) on both types of data
must have been the SAME!

## ![Workflow diagram for aggregation] (diagrams/LDP-data_aggregation-flow.png)
