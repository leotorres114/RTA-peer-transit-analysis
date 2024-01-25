# Regional transit peer service and ridership recovery analysis
This repository stores the source data, scripts, charts, and tables related to a transit service and ridership recovery analysis conducted by CMAP staff. These data serve as supporting evidence for the policy brief: Transit ridership and service trends at the RTA service boards and peer agencies.

The analysis explores recent changes in transit ridership and service levels in northeastern Illinois. It compares the performance of the region’s public transit operators – the Chicago Transit Authority (CTA), Metra, and Pace – with that of their peers. Since the onset of the COVID-19 pandemic, the regional transit system has faced enduring challenges. The pandemic led to abrupt shifts in travel, such as a significant reduction in ridership related to remote work. It also exacerbated longstanding issues, such as frequency, reliability, safety, and security. Other regions throughout the U.S. are facing similar challenges, with an uneven and tumultuous post-pandemic transit recovery. With three years of hindsight, peer comparisons can highlight areas of opportunity and improvements that the region could look to as inspiration and reflection as it seeks to increase transit ridership and improve the system’s long-term financial sustainability.

CMAP’s Plan of Action for Regional Transit (PART) report, approved on October 11, 2023 and delivered to the Illinois General Assembly in December 2023, identifies recommendations on the strategies, reforms , and funding solutions necessary to create a stronger regional transit system. For more information, see the [PART report webpage](https://www.cmap.illinois.gov/programs/regional-transit-action).

## Folder structure and descriptions
### src
- `src/peer_table.R`
- `src/get_data.R`
- `src/projections.R`
- `src/peer_analysis.R`
### raw_data
- `ntd_monthly_nov_2023.xlsx`
### processed_data
- `ridership_peers_annual.csv`
- `ridership_peers_monthly.csv`
- `ridership_peers_monthly_clean.csv`
- `rta_peers_ntd.csv`
- `vrh_peers_annual.csv`
- `vrh_peers_monthly.csv`
- `vrh_peers_monthly_clean.csv`
- `vrm_peers_annual.csv`
- `vrm_peers_monthly.csv`
- `vrm_peers_monthly_clean.csv`
### charts
- `[mode]_[metric].png`
- `[mode]_summary.png`
