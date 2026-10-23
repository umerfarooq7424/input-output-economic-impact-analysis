# Input–Output Economic Impact Analysis — R

> Leontief input–output modelling in R, built on BEA Supply–Use tables and the WIOD world input–output database: a Northeast gas-pipeline impact study for New Jersey and Pennsylvania, sector aggregation, direct requirements matrices, and a NAFTA global value-chain analysis.

## Flagship result — gas pipeline impact, New Jersey

Summing the sector results in `results/Economic_Impact_of_Gas_pipeline_in_New_Jersey.csv` (15 sectors, values as reported by the model):

| Total change (NJ) | Value |
|---|---|
| Output | 121,417.7 |
| Jobs | 813 |
| Earnings | 41,652.4 |
| GDP | 63,485.4 |

Largest effects: Manufacturing (output 40,846; 102 jobs) and Professional & Business Services (output 34,281; **402 of the 813 jobs**).

## The projects (`src/`)

| Script | What it does |
|---|---|
| `pipeline_impact_nj_pa.R` | Leontief impact of the Northeast pipeline upgrade on NJ + PA, using state technical-coefficient matrices (`data/A_nj.csv`, `A_pa.csv`), multiplier vectors, and project spending by sector |
| `pipeline_impact_aggregation_15sector.R` | Aggregates the US Use table (BEA 2022) to 15 sectors and reruns the NJ pipeline impact at that resolution |
| `direct_requirements_sut_2017.R` | Builds direct requirements matrices from BEA 2017 Supply–Use (SUT) framework tables |
| `nafta_gvc_wiot_2008.R` | Global value-chain analysis of NAFTA (USA–Canada–Mexico) on the WIOD 2008 world table: 56 industries × 44 countries, CPI-deflated to 1995 dollars, Leontief inverse and value-added decomposition |

## Reports (`reports/`)

- **Electric Car Manufacturing Proposal** — an EV plant proposal analysed with the same IO toolkit
- **NAFTA Value Added Analysis** — written companion to the GVC script

## Workbooks & data

- `workbooks/` — EconomicImpactAnalysis, NJ impact sheets, and the Northeast Pipeline Upgrade spending file (in-state summary + total spending)
- `data/` — BEA Use tables (US 2022 at 15- and 72-sector detail; US 2019), NAICS codes, trade data, state A-matrices and multiplier vectors

## Notes

- Scripts were written for Rutgers coursework (Spring 2024); original absolute Windows paths have been replaced with relative ones.
- The NAFTA script expects the WIOD 2008 RData file (`WIOT2008_October16_ROW.RData`, from WIOD) in the working folder — it is not redistributed here.
- BEA Supply–Use detail tables for the SUT script: [bea.gov/industry/input-output-accounts-data](https://www.bea.gov/industry/input-output-accounts-data)

## Author

**Umer Farooq** — Economic & Climate Risk Analyst · [LinkedIn](https://www.linkedin.com/in/umer-farooq-gojra/) · [GitHub](https://github.com/umerfarooq7424)

## License

MIT — see LICENSE. Underlying data: BEA and WIOD (public).
