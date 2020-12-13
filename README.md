# Research Paper Validation: 'Open Data on Industry Payments to Healthcare Providers Reveal Potential Hidden Costs to the Public'

### Table of Contents
- [Abstract](#Abstract)
- [Data Collection](#Data-Collection)
- [Data Summary](#Data-Summary)
- [Limitations](#Limitations)
- [Analysis](#Analysis)
- [Conclusion](#Conclusion)
- [Future Work](#Future-Work)
- [Sources](#Sources)

## Abstract
Healthcare industry players make payments to medical providers in an attempt to influence them. The effect these payments have on overall healthcare costs remains unknown. One study examined this relationship by establishing if a correlation exists between high payments and high costs. The study concluded that such a relationship does exist, specifically, 'a 10% increase in industry payments is associated with 1.3% higher medical and 1.8% higher drug costs.' This project aims to replicate these results by independently gathering the same datasets and applying the same analysis. At time of writing, this project has found a 10% increase in industry payments is associated with 0.13% higher medical and 1.0% higher drug costs. Note these values may change as more data is added to the project.

## Data Collection
Fortunately, original research data and the code they used to make their observations are available for download. However, for maximum integrity, the data used for validating has been gathered from scratch. 

The original research paper cites five data sources:
1. Industry Payments: The Open Payments (OP) 2016 system publicly available through the Centers for Medicare & Medicaid Services (CMS). One issue with this dataset is that it lacks NPI, an unique identifier used for medical providers in all other datasets. Sadly, this ommission is required by the law that established the Open Payments system (citation needed). As a result, an additional dataset was needed in order to programmatically find NPIs for the providers listed in this dataset.

2. Resource Utilization and Costs of Care: Again, we use the data publicy available through the CMS. CMS catalogs costs charged to medicare by providers. Of the categories available under Medicare Provider Utilization data, the original research paper used the Medicare Provider Physician and Other Supplier (MPOS) data.

3. NPI Public Registry. CMS is three for three for publicly available big data. This is the dataset used to search for NPIs in the OP dataset so they can join with MPOS. My ability to match NPIs is what makes or break this project.

4. Remaining data was downloaded from the original research paper. Attempts were made to gather that data, but roadblocks appeared in the form of paywalls and access requests. Remaining data includes: American Community Survey (2016) from the US Census Bureau which contains social, economic, housing, and dmeographic information about US population; Gallup Poll on Conservative Ideology; McKinsey & Co. Leading Health Care Index; and the KFF Professionally Active Specialist Physicians by Field dataset.

This project was 90% data cleaning.

Tools used:
- Google Cloud Platform: BigQuery - to match NPIs, and merge OP & MPOS
- R - to run analysis on matched providers

### Data Sources
Data gathered from scratch:
- Open Payments (Source: CMS)
- MPOS (Source: MPOS)
- NPI Public Registry

Data used from original paper:
- Gallup poll
- Census survey
- McKinsley Healthcare Index
- KFF Specialist by Field

### Data Cleaning
Tools used:
- Google Cloud Platform: BigQuery - to match NPIs, and merge OP & MPOS
- R - to run analysis on matched providers

In order to match the most number NPIs, data cleaning steps were taken.
Data cleaning steps:
1. Upper case
2. Remove punctuation, specifically: ;,\'.]|^
3. 4 typo corrections for city names/consolidation, i.e. ALAMONTE SPGS into ALTAMONTE SPRINGS 
4. 8 manual address changes, i.e. if a provider had four different addresses

A provider's NPI was found by matching:
- Street Address 1 as either primary practice address 1 or primary mailing address 1
- City
- Last Name

These parameters were chosen due to getting the highest number of results while avoiding duplication, i.e. two different providers being matched with the same NPI.

As a result of the above cleaning and matching criteria, this validation project was able to match 506,520 NPIs (79.41%) into the Open Payments dataset. In comparison, the original research paper was able to match 603,714 NPIs (90.4%). Of the number of NPIs matched in this validation project, 345,743 providers were matched with MPOS and used in this analysis. In comparison, the original paper was able to match 374,766 NPIs from Open Payments into MPOS, a 29,043 provider difference.

## Limitations
- Summing average medicare allowed costs. 
- Limited to individual providers.
- Limited to only medicare data.

## Analysis 

### Original vs Validation
**Model 1:** Log-reg fit. Target: total medical medicare allowed costs. Features: total payments, number of beneficiaires, and state of the provider

**Result:** A 10% difference in industry payments was associated with 0.13% higher total medical (non-drug) costs per provider, of 0.0137(95% CI: (0.009, 0.019)). 

Note: This value kept increasing as more NPIs were matched. As a result, it is safe to assume this value will increase as more NPIs are analyzed. How much it will increase is up to speculation.

**Model 2:** Log-reg fit. Target: total medical medicare allowed costs.Features: total pay, number of beneficiaires, state of the provider, and total drug medicare allowed costs

**Result:** A 10% difference in industry payments was associated with no change, -0.005502 (95% CI: (-0.013 0.002), in medical costs, given fixed drug costs. 

Note: this value became closer and closer to 0 as more data came in. As a result, this project is unable to assume where the number will go as more data comes in.

**Model 3**: Log-reg fit. Target: total drug medicare allowed costs. Features: total payments, number of beneficiaries, and state of the provider.

**Correlation between drug and medical costs:** -0.01453288

Note: This value decreased to 0 as more data came in.

**Result:** A 10% difference in industry payments associated with 1.0% increased drug costs, controlling for num beneficiaries and state. Estimate 0.101 (95% CI:(0.079, 0.123)).

**Context:** A typical provider within the middle range of industry payments, with industry payments equal to the median of \\$191 and total Medicare medical costs equal to the median of \\$850. A 10% difference of \\$19 in industry payments would then be associated with an expected difference of ~\\$1.10. 

Controlling for drug costs, 70% of the providers in this analysis had no drug costs. Among providers within the middle range of industry payments, the median, non-zero drug costs were \\$53.93, so a 10% difference in industry payments would be associated with a difference in drug costs of 1.0% or ~\\$0.53.

**Geographical variation in the payments-costs association:**
The estimated state-level log-log regression slope varied from -0.08 in Utah to 0.15 in Minnesota. A 10% difference in medical costs in Utah, and 1.5% difference in Minnesota. 

Note: the average pearson correlation among the states in this research validation was 0.10. The original paper had average pearson correlation of 0.26 mong the 50 states.

**Strongest association among all continuous variables:** Conservative advantage: -0.72144; p = 0.00539.

### Discrepancies
1. Unable to run analysis code provided by original research paper.

The analysis code provided by the original research paper cannot be run on the dataset it provided. This is due to the provided dataset missing columns. It's missing 'Entity.Type.of.the.Provider', used to limit individual versus enterprise providers, and 'State.Code.of.the.Provider', which separates providers into the their state. 

Relatedly, the provided dataset contains numerous superfluous columns containing the demographic information of the patients for each provider. These demographics include race, age, and percent of paitents with well-known diagnosises, such as alzheimer's disease. These columns cannot possibly come from any of the datasets cited in the original paper. 

2. There exists discrepancies between original and validated dataset on the same provider. 

One example was found.

|NPI|Source|Number of HCPCS|Number of Medicare Beneficiaries|
|---|---|---|---|
|1003000142|Original|43|208|
|1003000142|Me|9|438|

At time of writing, it is unknown how many of these discrepancies exist. 

## Conclusion
This project was able to validate the some of the data analysis completed by the original research paper. Using 345,743 providers, the validation project found a very small correlation between industry payments and medical costs of 0.13%, off by a power of ten compared to the original research paper of 1.3%. Robustness tests offered by the original paper resulted in a mix of agreement and discrepent results. 

The validation project was able to agree with absolute certainty that conservative ideology showed the strongest association among all economic, social, health, and political control variables. 

At the time of writing, I am unsure how to call my results in comparison to the original paper. I'd like to resolve the discrepencies found in my results before drawing a conclusion. 

## Future Work
1. Cross reference OP with MPOS in addition to the NPI database
2. Perform more cleaning to increase conversion rate
3. Perform EDA on MPOS & OP datasets
4. Reverse-engineer how original research programmatically assigned NPIs
5. Discover number and source of discrepencies
6. Last, but not least, discuss findings with original writers.

## Sources
1. Mejia, J., Mejia, A. & Pestilli, F. Open data on industry payments to healthcare providers reveal potential hidden costs to the public. Nat Commun 10, 4314 (2019). https://doi.org/10.1038/s41467-019-12317-z
2. https://www.cms.gov/OpenPayments/Explore-the-Data/Dataset-Downloads
3. https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/Physician-and-Other-Supplier
4. https://npiregistry.cms.hhs.gov/
5. https://stackoverflow.com/questions/9366021/checking-whether-an-item-does-not-exist-in-another-table