## About this dashboard

### Why we built it

The 2025–26 Kenya Mini Demographic and Health Survey (KMDHS) is Kenya's most recent
nationally representative source on fertility, family planning, maternal and newborn
health, child health, malaria, water and sanitation, health insurance under the Social
Health Authority (SHA), and technology-facilitated gender-based violence. It gives
estimates for all 47 counties.

The final report is thorough, but it runs to more than 600 pages. A county health
officer, a journalist or a student who needs one number, or wants to see how their
county compares, has to search through hundreds of tables. This dashboard turns
those tables into an interactive tool. You can:

- see the national picture at a glance,
- map any county-level indicator and rank all 47 counties,
- read a one-page profile of a single county against the national figure,
- open any extracted table, search it and download it as CSV.

Every number shows the report table and page it came from, so you can check it
against the original.

The dashboard was built as a portfolio project in health data science and the design
of health information systems.

### How it was made

1. **Extraction.** An R script reads the text layer of the report PDF, finds each
   table by its title and parses the rows. Column names come from a hand-checked
   registry, one entry per table.
2. **Validation.** The build stops if any check fails. Each county table must list
   all 47 counties, percentage distributions must add up to 100, and county totals
   must match the national tables. A random sample of values was also checked by
   hand against the PDF.
3. **Dashboard.** The app is written in R Shiny with bslib, leaflet and plotly. It runs
   in the browser through Shinylive, so it can be hosted for free on GitHub Pages.

County boundaries come from geoBoundaries (gbOpen, KEN ADM1, CC BY 4.0).

### Reading the numbers

- **Values in brackets or faded marks** are based on 25–49 unweighted cases. Treat
  them with caution.
- **Suppressed values** (shown as \* in the report) are based on fewer than 25 cases
  and are not shown.
- County estimates rest on small samples, and the differences between neighbouring
  counties are often within sampling error.
- A higher rank is not always better. Rank 1 is simply the highest value.
- A few source issues are documented in the extraction registry. For example, the
  Wajir row is missing from Table 3.8C, and the Mombasa row in Table 7.14.2C is blank
  in the PDF.

### Source and licence

KNBS and ICF. 2026. *2025–26 Kenya Mini Demographic and Health Survey.* Nairobi, Kenya,
and Rockville, Maryland, USA: KNBS and ICF. DOI: 10.63814/k7m3xq21.

The report is published under the Creative Commons Attribution-NonCommercial 4.0
licence (CC BY-NC 4.0). This dashboard is a non-commercial derivative and is not an
official product of KNBS, the Ministry of Health or ICF. Values come from the
published tables, not the survey microdata.
