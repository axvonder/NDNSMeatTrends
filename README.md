Hi! Thanks for accessing these open-access files for our project.
Any questions should be directed to: a.vonderschmidt@sms.ed.ac.uk

Project: Contribution of Meat-Free Days, Meat-Free Meals, and Portion Sizes to Declines in Meat Consumption in the UK

Project Description

This project aims to analyze the decline in meat consumption in the UK from 2008 to 2019 and identify which behaviors have contributed to this decline. The focus is on changes in the frequency of eating meat, the number of daily eating occasions with meat, and the change in portion size. The findings will help tailor behavior change interventions and public health policies to accelerate the reduction in meat consumption towards national goals for population and environmental health.

Table of Contents

1. Introduction
2. Data Source
3. Repository Structure
4. Installation and Setup
5. Usage
6. Contributing
7. License
8. Contact
9. Acknowledgments

## 1. Introduction

The study is based on an analysis plan developed by Alexander Vonderschmidt (lead author), Alexandra Bellows, Lindsay Jaacks, Peter Alexander, and Cristina Stewart (senior author). The publication can be found [HERE-CITE PUBLICATION WHEN PUBLIC]. The main objectives of the analysis are:

- Investigate changes in the frequency of meat consumption, the number of daily eating occasions with meat, and the change in portion size.
- Determine what proportion of declines in meat consumption can be attributed to each of these three behaviors.
- Investigate how frequency and portion size changed by standard mealtime (breakfast, lunch, and dinner) and key population subgroups such as age, sex, and deprivation level.

## 2. Data Source

This analysis used data from the National Diet and Nutrition Survey (NDNS) Rolling Programme years 2008/09–2018/19. This data is open access and available for download from the UK Data Service: https://beta.ukdataservice.ac.uk/datacatalogue/series/series?id=2000033.

## 3. Repository Structure

README.md: This file, providing an overview of the project as well as instructions on installation and usage.
Data Dictionary.docx: A document containing detailed information about the variables used in this project.
transformation.R: An R script that cleans, preprocesses, and transforms the raw NDNS diet data.
combining.R: An R script that combines the transformed dataset (which only works with diet data) with other individual-level and household-level NDNS datasets.
calculations.R: An R script that performs the statistical analysis and generates figures and tables.

## 4. Installation and Setup

1. Clone this repository to your local machine: git clone https://github.com/axvonder/NDNSMeatTrends.git
2. Install the required R packages: install.packages(c("chron", "tidyverse", "lubridate", "janitor", "dplyr", "srvyr", "survey", "effects", "ggplot2", "RColorBrewer", "scales", "gridExtra", "cowplot"))
3. Set your working directory to the cloned repository folder in your R environment.

## 5. Usage

Run the R scripts in the following order:

1. transformation.R: Cleans, preprocesses, transforms the raw NDNS data.
2. combining.R: Combines the transformed dataset (that only works with diet data) with other individual-level and household-level NDNS datasets.
3. calculations.R: Performs the statistical analysis and generates figures and tables.

## 6. Contributing

Contributions to this project are welcome. If you would like to contribute, please follow these steps:
1. Fork the repository to your GitHub account.
2. Clone your fork to your local machine: `https://github.com/axvonder/NDNSMeatTrends.git`
3. Create a new branch for your changes: `git checkout -b feature/my-new-feature`
4. Make your changes and commit them to your branch: `git add .` and `git commit -m "Add my new feature"`
5. Push your changes to your fork: `git push origin feature/my-new-feature`
6. Open a pull request on the original repository with a clear and concise description of your changes.

Please ensure that your changes are consistent with the project's style and that you have tested your code before submitting a pull request. Also, include any relevant documentation or comments in your code.

## 7. License

This project is open access.

## 8. Contact

For any questions or concerns, please contact the project maintainers:

- Alexander Vonderschmidt: a.vonderschmidt@sms.ed.ac.uk
- Cristina Stewart: Cristina.Stewart@ed.ac.uk
- Alexandra Bellows: abellows@exseed.ed.ac.uk

## 9. Acknowledgments

We thank all the co-authors of this paper for their contributions, as well as all the scientists who have contributed previous research to NDNS. We would also like to thank the UK National Diet and Nutrition Survey (NDNS) team for providing the data used in this project and all the contributors who have helped improve this project.