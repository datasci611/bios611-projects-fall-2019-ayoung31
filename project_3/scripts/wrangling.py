import pandas as pd
from datetime import datetime

# Read Data
client_df = pd.read_csv("https://raw.githubusercontent.com/biodatascience/datasci611/gh-pages/data/project2_2019/CLIENT_191102.tsv", sep='\t')
dis_enter = pd.read_csv("https://raw.githubusercontent.com/biodatascience/datasci611/gh-pages/data/project2_2019/DISABILITY_ENTRY_191102.tsv", sep='\t')
dis_exit = pd.read_csv("https://raw.githubusercontent.com/biodatascience/datasci611/gh-pages/data/project2_2019/DISABILITY_EXIT_191102.tsv", sep='\t')
ee_reviews = pd.read_csv("https://raw.githubusercontent.com/biodatascience/datasci611/gh-pages/data/project2_2019/EE_REVIEWS_191102.tsv", sep='\t')
ee_udes = pd.read_csv("https://raw.githubusercontent.com/biodatascience/datasci611/gh-pages/data/project2_2019/EE_UDES_191102.tsv", sep='\t')
entry_exit = pd.read_csv("https://raw.githubusercontent.com/biodatascience/datasci611/gh-pages/data/project2_2019/ENTRY_EXIT_191102.tsv", sep='\t')
health_ins_entry = pd.read_csv("https://raw.githubusercontent.com/biodatascience/datasci611/gh-pages/data/project2_2019/HEALTH_INS_ENTRY_191102.tsv", sep='\t')
health_ins_exit = pd.read_csv("https://raw.githubusercontent.com/biodatascience/datasci611/gh-pages/data/project2_2019/HEALTH_INS_EXIT_191102.tsv", sep='\t')
income_entry = pd.read_csv("https://raw.githubusercontent.com/biodatascience/datasci611/gh-pages/data/project2_2019/INCOME_ENTRY_191102.tsv", sep='\t')
income_exit = pd.read_csv("https://raw.githubusercontent.com/biodatascience/datasci611/gh-pages/data/project2_2019/INCOME_EXIT_191102.tsv", sep='\t')
noncash_entry = pd.read_csv("https://raw.githubusercontent.com/biodatascience/datasci611/gh-pages/data/project2_2019/NONCASH_ENTRY_191102.tsv", sep='\t')
noncash_exit = pd.read_csv("https://raw.githubusercontent.com/biodatascience/datasci611/gh-pages/data/project2_2019/NONCASH_EXIT_191102.tsv", sep='\t')
fam = pd.read_csv("https://raw.githubusercontent.com/biodatascience/datasci611/gh-pages/data/project2_2019/VI_SPDAT_FAM_V2_191102.tsv", sep='\t')
ind = pd.read_csv("https://raw.githubusercontent.com/biodatascience/datasci611/gh-pages/data/project2_2019/VI_SPDAT_IND_V2_191102.tsv", sep='\t')
v1 = pd.read_csv("https://raw.githubusercontent.com/biodatascience/datasci611/gh-pages/data/project2_2019/VI_SPDAT_V1_191102.tsv", sep='\t')

# Restructure Disability dataset so that it can be merged with others
temp = dis_enter[['EE UID','Disability Determination (Entry)','Disability Type (Entry)']]
temp = temp.drop_duplicates()
temp2 = temp.pivot_table(index=['EE UID'],
                             columns=['Disability Type (Entry)'],
                             values=['Disability Determination (Entry)'],
                             aggfunc=lambda x: ' '.join(str(v) for v in x))

# Merge all datasets of interest
df = client_df.merge(entry_exit, on='EE UID', how='inner').merge(ee_udes, on="EE UID", how='inner').merge(temp2, on="EE UID", how='outer')

# Remove 1st column
df = df.iloc[:,1:59]

# Remove other columns that are either duplicates or not of interest
df = df.drop(['Client Unique ID_x', 'EE Provider ID_y','Client ID_y','Client Unique ID_y','Entry Exit Group Id','Entry Exit Household Id','Unnamed: 6','Client Unique ID','Client ID','Zip Code (of Last Permanent Address, if known)(1932)','Relationship to Head of Household(4374)','If yes for Domestic violence victim/survivor, when experience occurred(1917)','Date of Birth(893)'], axis=1)

# Rename all remaining columns
df.columns = ['EE_UID', "Client_ID", "Age_Entry", "Age_Exit", "Gender", "Race", "Ethnicity", "Veteran", "Entry_Date", "MoveIn_Date", "Exit_Date", "Destination", "Reason_for_Leaving", "EE_Type", "remove1", "remove2", "remove3", "remove4", "remove5", "Prior_Living", "Length_at_Prior", "Less_7days", "Less_90days", "streetsESSH_night_before", "times_street_ESSH_past3yrs_includetoday", "month_homeless_streetESSH_past3yrs", "Housing_Status", "Disabling_Condition", "Health_Ins", "Dom_Violence", "Alc_Abuse", "DrugAndAlc_Abuse", "Chronic_Health_Cond", "Devel_Disability", "Drug_Abuse", "Dual_Diagnosis", "HIV_AIDS", "Hearing_Impaired", "Mental_Health", "Other", "Learning", "Speech", "Physical", "Physical_Medical", "Visual"]

# Convert dates to date/time variable types
df['Date_Enter'] = pd.to_datetime(df["Entry_Date"], format="%m/%d/%Y")
df['Date_Exit'] = pd.to_datetime(df["Exit_Date"], format="%m/%d/%Y")
df['days_at_shelter'] = (df['Date_Exit'] - df['Date_Enter']).dt.days

# Remove a few more columns I don't need
df = df.drop(['remove1', 'remove2', 'remove3', 'remove4', 'remove5', 'EE_UID','Entry_Date','Exit_Date'], axis=1)

# Save data as csv
df.to_csv("data/cleaned.csv")
