# Cabinet formation dataset

Create dataset with potential coalitions based on [ParlGov](https://parlgov.org)
data.

Original version for Thürk/Hellström/Döring (2021) project, last update R code
in 2022, and publication on [ParlGov
Snippets](https://github.com/hdigital/parlgov-snippets/) in 2024.

Data from [ParlGov
Dataverse](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/2VZ5ZC)
is licensed [CC0 1.0](https://creativecommons.org/publicdomain/zero/1.0/).

_Note_ — Running `03-cabinet-formation.R` requires considerable time.

## Information

Author — Holger Döring

- _01-cabinet-dataset.R_ — create dataset for analysis
- _02-cabinet-potential.R_ — create dataset with potential coalitions
- _03-cabinet-formation.R_ — create dataset with coalition formation parameters
  (_performance bottleneck_)
- _04-cabinet-formation-check.R_ — select sample to check some coalition
  formations
- csv-files zipped to save disk space (use R `readr::read_csv()`)
- R packages — tidyverse (esp. dplyr, purrr, tidyr)

## Variables

Variables coded for potential coalitions

- cabinet_id — ParlGov cabinet ID
- coalition — number of potential coalition for cabinet_id
- formed — potential coalition that formed
- minority, min-win, surplus — cabinet type
- incumbent — incumbent coalition
- incumbent_pm — previous PM party in coalition
- n — number of parties in coalition
- enp — effective number of parties in coalition
- seats_share — seats share of coalition
- largest — largest party in coalition
- median — median party in coalition
- lr_range — ideological divisions in coalition
- anti_system — anti-system presence in coalition

---

## Variables used in studies

### Variables in Thürk/Hellstrom/Döring 2021

- Effective number of parties
- Polarization — Ideological Divisions in the Coalition
- Seats share largest party — Largest Party in the Coalition
- Median-largest — Largest / Median Party in the Coalition
- Seats share radical parties — Anti-System Presence
- Cabinet level only
  - Investiture vote
  - Bicameralism
  - Semi-presidential
  - Veto-points
  - CEE-country
  - Majority situation

### Variables in Glasgow/Golder/Golder 2012 — [Martin/Stevenson 2001 replication]

- Minority Coalition
- Minimal Winning Coalition
- Number of Parties in the Coalition
- Largest Party in the Coalition
- Median Party in the Coalition
- Ideological Divisions in the Coalition
  - absolute distance between the most distant pair of parties
- Ideological Divisions within Majority Opposition
- Previous Prime Minister in the Coalition
- Incumbent Coalition
- Minority Coalition where Investiture Vote Required
- Anti-System Presence in the Coalition
- Pre-Electoral Pact associated with the Coalition
- Anti-Pact associated with the Coalition

### Variables in Eppner/Ganghof 2017

- Veto control
- Veto control+ Non-NCV
- Veto control+ Non-reform
- Lower chamber seat share of coalition
- Lower chamber seat share of minority coalition
- Oversized coalition
- Number of parties in the coalition
- Largest party in the coalition
- Median party in the coalition
- Ideological divisions in the coalition
- Ideological divisions within majority opposition
- Previous prime minister in the coalition incumbent coalition
- Minority coalition where investiture vote required
- Coalition splits pre-electoral pact

---

## References

Eppner, Sebastian, and Steffen Ganghof. 2017. “Institutional Veto Players and
Cabinet Formation: The Veto Control Hypothesis Reconsidered.” European Journal
of Political Research 56(1): 169–86. doi:10.1111/1475-6765.12172.

Glasgow, Garrett, Matt Golder, and Sona N. Golder. 2012. “New Empirical
Strategies for the Study of Parliamentary Government Formation.” Political
Analysis 20(2): 248–70. doi:10.1093/pan/mpr058.

Martin, Lanny W., and Randolph T. Stevenson. 2001. “Government Formation in
Parliamentary Democracies.” American Journal of Political Science 45(1): 33–50.
doi:doi.org/10.2307/2669358.

Thürk, Maria, Johan Hellström, and Holger Döring. 2021. “Institutional
Constraints on Cabinet Formation: Veto Points and Party System Dynamics.”
European Journal of Political Research 60(2): 295–316.
doi:10.1111/1475-6765.12407.
