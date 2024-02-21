# Regional transit peer service and ridership recovery analysis
This repository stores the source data, scripts, charts, and tables related to a transit service and ridership recovery analysis conducted by CMAP staff. These data serve as supporting evidence for the policy brief: Transit ridership and service trends at the RTA service boards and peer agencies.

The analysis explores recent changes in transit ridership and service levels in northeastern Illinois. It compares the performance of the region’s public transit operators – the Chicago Transit Authority (CTA), Metra, and Pace – with that of their peers. 

Since the onset of the COVID-19 pandemic, the regional transit system has faced enduring challenges. The pandemic led to abrupt shifts in travel, such as a significant reduction in ridership related to remote work. It also exacerbated longstanding issues, such as frequency, reliability, safety, and security. Other regions throughout the U.S. are facing similar challenges, with an uneven and tumultuous post-pandemic transit recovery. 

With three years of hindsight, peer comparisons can highlight areas of opportunity and improvements that the region could look to as inspiration and reflection as it seeks to increase transit ridership and improve the system’s long-term financial sustainability.

CMAP’s Plan of Action for Regional Transit (PART) report, approved on October 11, 2023 and delivered to the Illinois General Assembly in December 2023, identifies recommendations on the strategies, reforms , and funding solutions necessary to create a stronger regional transit system. For more information, see the [PART report webpage](https://www.cmap.illinois.gov/programs/regional-transit-action).

## Repo structure
### src
- `src/peer_analysis.R`: Main source file. Runs `get_data.R`, cleans/formats/exports the data for the charts to the `processed_data` folder, and creates/exports [cmapplot](https://github.com/CMAP-REPOS/cmapplot)'s to the `charts` folder. 
- `src/peer_table.R`: Builds a table of the RTA's peer systems with their associated identifiers from the National Transit Database (NTD).
- `src/get_data.R`: Collects, cleans, and formats NTD service and ridership data. It sources `peer_table.R` and merges its output with NTD data.
- `src/manual_data_fixes.R`: Because this is preliminary monthly data, there may be reporting issues from peer agencies. The script manually calculates and adjusts data if issues are identified.
### raw_data
- Stores the raw [monthly data](https://www.transit.dot.gov/ntd/data-product/monthly-module-adjusted-data-release) from the National Transit Database. 
### processed_data
- Stores monthly and annual ridership and vehicle revenue hour data for the RTA service boards and peer systems, as well as the output of `peer_table.R`. CSV files ending in `*_monthly_clean.csv` report combined directly operated and purchased transportation figures for each agency and mode.
### charts
- Stores the charts from `peer_analysis.R`.
