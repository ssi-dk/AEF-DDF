# Table maintenance

This folder contains the .R scripts that generate and update tables in the database
using `SCDB::update_snapshot()`.

## Todo:

- [ ] Install missing packages
  - [x] SCDB (0.5.1)
  - [x] Rpostgres (1.4.8)
  - [ ] PostgresSQL not being translated correctly by dbplyr?
    - [ ] Upgrade dbplyr (2.3.3) -> (2.5.1)

- [ ] Get the table maintenance scripts running
  - [ ] lpr_courses_of_contacts
  - [ ] mutation_info
  - [ ] epicpr_adresse
  - [ ] miba
  - [ ] dmi
  - [ ] epilpr_contact2course
  - [ ] vars
  - [ ] epicpr
  - [ ] epilpr_contact_expanded
  - [ ] epicpr_c_status
  - [ ] vaccine_history
  - [ ] lineage_info
    - [ ] Update lin_use file

- [ ] Use SCDB to store lin_use output
- [ ] Automatically generate lin_use files on Github

## Database structure

These are the tables maintained by us

| table                        | New NGC     |
|------------------------------|-------------|
| `mg.lpr_courses_of_contacts` | Not running |
| `mg.mutation_info`           | Not running |
| `mg.epicpr_adresse`          | Not running |
| `mg.miba`                    | Not running |
| `mg.dmi`                     | Not running |
| `mg.epilpr_contact2course`   | Not running |
| `mg.vars`                    | Not running |
| `mg.epicpr`                  | Not running |
| `mg.epilpr_contact_expanded` | Not running |
| `mg.epicpr_c_status`         | Not running |
| `mg.vaccine_history`         | Not running |
| `mg.lineage_info`            | Not running |

## Last update for tables and their dependencies

| table                        | Last update |
|------------------------------|-------------|
| `mg.lpr_courses_of_contacts` | 2023-06-28  |
| - ?                          | ?           |

| table                        | Last update |
|------------------------------|-------------|
| `mg.mutation_info`           | 2022-12-05  |
| - `prod.covid_19_wgs`        | 2025-06-30  |

| table                        | Last update |
|------------------------------|-------------|
| `mg.epicpr_adresse`          | 2024-06-15  |
| - ?                          | ?           |

| table                        | Last update |
|------------------------------|-------------|
| `mg.miba`                    | 2023-10-30  |
| - `prod.variant_covid_19`    | 2025-05-20  |
| - `prod.covid_19_wgs`        | 2025-06-30  |
| - `prod.basis_samples`       | 2025-05-20  |
| - `mg.lineage_info`          | 2025-06-30  |

| table                        | Last update |
|------------------------------|-------------|
| `mg.dmi`                     | 2023-03-22  |
| - File transfers             | ?           |

| table                        | Last update |
|------------------------------|-------------|
| `mg.epilpr_contact2course`   | 2023-06-28  |
| - ?                          | ?           |

| table                        | Last update |
|------------------------------|-------------|
| `mg.vars`                    | static?     |

| table                        | Last update |
|------------------------------|-------------|
| `mg.epicpr`                  | 2024-06-15  |
| - `mg.epicpr_c_status`       | 2024-06-15  |
| - `prod.municipalities`      | static      |
| - `mg.epicpr_adresse`        | 2024-06-15  |

| table                        | Last update |
|------------------------------|-------------|
| `mg.epilpr_contact_expanded` | 2023-06-28  |
| - `lpr_contact2course.R`     | ?           |

| table                        | Last update |
|------------------------------|-------------|
| `mg.epicpr_c_status`         | 2024-06-15  |
| - `prod.cpr3_t_person`       | 2024-06-15  |

| table                        | Last update |
|------------------------------|-------------|
| `mg.vaccine_history`         | 2025-05-17  |
| - `prod.covid_19_maalgruppe` | 2025-05-20  |

| table                        | Last update |
|------------------------------|-------------|
| `mg.lineage_info`            | 2023-10-30  |
| - `prod.covid_19_wgs`        | 2025-06-30  |
