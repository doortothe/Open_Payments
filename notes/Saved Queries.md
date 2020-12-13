## Crossreference missing NPIs
SELECT n.NPI, Provider_Last_Name__Legal_Name_, Provider_First_Name, Provider_First_Line_Business_Mailing_Address, Provider_Second_Line_Business_Mailing_Address, Provider_Business_Mailing_Address_City_Name, Provider_First_Line_Business_Practice_Location_Address, Provider_Second_Line_Business_Practice_Location_Address, Provider_Business_Practice_Location_Address_City_Name, m.Total_Pay, Total_Medicare_Allowed_Amount
FROM `carbide-server-294318.op2016.NPIs` AS n
  JOIN `carbide-server-294318.op2016.missing_NPIs3` AS m
    ON CAST(m.NPI_number AS NUMERIC) = n.NPI

GROUP BY n.NPI, Provider_Last_Name__Legal_Name_, Provider_First_Name, Provider_First_Line_Business_Mailing_Address, Provider_Second_Line_Business_Mailing_Address, Provider_Business_Mailing_Address_City_Name, Provider_First_Line_Business_Practice_Location_Address, Provider_Second_Line_Business_Practice_Location_Address, Provider_Business_Practice_Location_Address_City_Name, m.Total_Pay, Total_Medicare_Allowed_Amount

ORDER BY m.Total_Pay DESC, Total_Medicare_Allowed_Amount DESC

## MPOS OP Merge
SELECT 
  OP.NPI,
  Provider_Type,
  Medicare_Participation_Indicator,
  Number_of_HCPCS,
  total_medicare_drug_allowed_amount,
  total_medicare_medical_allowed_amount,
  total_medicare_allowed_amount,
  total_paid,
  total_payments ,
  Entity_Type_of_the_Provider,
  State_Code_of_the_Provider,
  Number_of_Services,
  Number_of_Medicare_Beneficiaries,
  Physician_Profile_ID
FROM `carbide-server-294318.op2016.grouped_MPOS2` AS MPOS
  INNER JOIN `carbide-server-294318.op2016.grouped_OP_NPI_match3` AS OP
    ON OP.NPI = MPOS.NPI
    
## Missing NPIs in OG MPOS_OP_merge
-- Source: https://stackoverflow.com/questions/9366021/checking-whether-an-item-does-not-exist-in-another-table
SELECT DISTINCT
  NPI_number, Total_Pay, OP_MPOS.Total_Medicare_Allowed_Amount
FROM `carbide-server-294318.op2016.MPOS_OP_Merge2` AS MPOS
  RIGHT JOIN `carbide-server-294318.op2016.og_MPOS_OP_merge` AS OP_MPOS
    ON CAST(OP_MPOS.NPI_number AS NUMERIC) = NPI
WHERE OP_MPOS.NPI_number IS NOT NULL
  AND NPI IS NULL
ORDER BY Total_Pay DESC, OP_MPOS.Total_Medicare_Allowed_Amount DESC

## OP NPI matcher
SELECT 
  NPI
  ,SUM( Total_Amount_of_Payment_USDollars) AS total_paid
  ,SUM( Number_of_Payments_Included_in_Total_Amount) AS total_payments
  , Physician_First_Name
  , Physician_Last_Name
  , Physician_Profile_ID
FROM `carbide-server-294318.op2016.NPIs` as n
  INNER JOIN `carbide-server-294318.op2016.OP2016` as OP
    ON (Recipient_Primary_Business_Street_Address_Line1 = Provider_First_Line_Business_Mailing_Address 
      OR Recipient_Primary_Business_Street_Address_Line1 = Provider_First_Line_Business_Practice_Location_Address ) AND
    Recipient_City = Provider_Business_Practice_Location_Address_City_Name 
    AND Physician_Last_Name = Provider_Last_Name__Legal_Name_
GROUP BY NPI, Physician_First_Name, Physician_Last_Name, Physician_Profile_ID
ORDER BY NPI

## OP missing in MPOS_OP_Merge
-- Source: https://stackoverflow.com/questions/9366021/checking-whether-an-item-does-not-exist-in-another-table
SELECT 
  OP.Physician_Profile_ID, 
  Physician_Last_Name, 
  Physician_First_Name, 
  SUM( Total_Amount_of_Payment_USDollars ) AS total_paid,
  Recipient_Primary_Business_Street_Address_Line1,
  Recipient_City
FROM `carbide-server-294318.op2016.OP2016` AS OP
  LEFT JOIN `carbide-server-294318.op2016.MPOS_OP_Merge2` AS OP_MPOS
    ON OP_MPOS.Physician_Profile_ID = OP.Physician_Profile_ID
WHERE OP_MPOS.Physician_Profile_ID IS NULL
  AND OP.Physician_Profile_ID IS NOT NULL
GROUP BY 
  Physician_Profile_ID,
  Physician_Last_Name,
  Physician_First_Name,
  Recipient_Primary_Business_Street_Address_Line1,
  Recipient_City
ORDER BY total_paid DESC

## Pivot HCPCS drug indicator (did not use in final project)
-- Source: https://corecompete.com/how-to-build-pivot-tables-in-bigquery-fast-and-easy/
WITH wide_ranked AS (
  SELECT 
    SUM(IF( HCPCS_Drug_Indicator = True, rank, null)) AS total_medicare_drug_allowed_amount,
    SUM(IF( HCPCS_Drug_Indicator = False, rank, null)) AS total_medicare_medical_allowed_amount
  FROM (
    SELECT 
      '1' AS groupby_only_col,
      HCPCS_Drug_Indicator,
      RANK() OVER (ORDER BY HCPCS_Drug_Indicator) AS rank
    FROM (
      SELECT DISTINCT HCPCS_Drug_Indicator
      FROM `carbide-server-294318.op2016.MPOS`
    )
  )  
),
long_array_aggregated AS (
  SELECT id,
    ARRAY_AGG(values ORDER BY rank) AS values
    FROM (
      SELECT 
        ranked_classes_by_id.National_Provider_Identifier AS id,
        ranked_classes_by_id.rank AS rank,
        source.Average_Medicare_Allowed_Amount AS values
      FROM `carbide-server-294318.op2016.MPOS` AS source
      RIGHT JOIN(
        SELECT
          National_Provider_Identifier ,
          HCPCS_Drug_Indicator ,
          rank() over(PARTITION BY National_Provider_Identifier ORDER BY HCPCS_Drug_Indicator ) AS rank
        FROM(
          SELECT DISTINCT HCPCS_Drug_Indicator
          FROM `carbide-server-294318.op2016.MPOS`)
            CROSS JOIN (
              SELECT DISTINCT National_Provider_Identifier
              FROM `carbide-server-294318.op2016.MPOS`)
      ) AS ranked_classes_by_id
    USING( National_Provider_Identifier , HCPCS_Drug_Indicator )
    )
  GROUP BY id
)

SELECT 
  long_array_aggregated.id,
  long_array_aggregated.values[ordinal(total_medicare_drug_allowed_amount)] AS total_medicare_drug_allowed_amount,
  long_array_aggregated.values[ordinal(total_medicare_medical_allowed_amount)] AS total_medicare_medical_allowed_amount
FROM long_array_aggregated, wide_ranked

