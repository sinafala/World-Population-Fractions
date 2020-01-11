
# coding: utf-8

# In[ ]:

import pandas as pd
import numpy as np
import requests
import seaborn as sns
import matplotlib.pyplot as plt
get_ipython().magic('matplotlib inline')


# ### Download file from URL and save in directory

# In[ ]:

url = 'https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_Population/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx'
r = requests.get(url, allow_redirects=True)
open('WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx', 'wb').write(r.content)


# ### Grab two sheets from the excel file to dataframe

# In[ ]:

df1 = pd.read_excel("WPP2017_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx", sheet_name="ESTIMATES", header=16, index_col='Region, subregion, country or area *')
df2 = pd.read_excel("WPP2017_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx", sheet_name="MEDIUM VARIANT", header=16, index_col='Region, subregion, country or area *')


# ### Grab the necessary cells in the right format

# In[ ]:

df1 = df1.loc[['WORLD','Africa','China','India','Europe','Latin America and the Caribbean','Northern America']]
df1 = df1.iloc[:,6:]
df2 = df2.loc[['WORLD','Africa','China','India','Europe','Latin America and the Caribbean','Northern America']]
df2 = df2.iloc[:,7:]


# ### Merge two sheets

# In[ ]:

out = pd.concat([df1, df2], axis=1)
out.index=['World', 'Africa', 'China', 'India', 'Europe', 'South America', 'North America']


# In[ ]:

add = []
for i in range(len(out.columns)):
    a = (int(out.iloc[:1,i]) - sum(out.iloc[1:,i]))
    add.append(a)


# In[ ]:

add = pd.DataFrame([add], index=["Other"], columns = out.columns)
out = pd.concat([out, add], axis=0)


# ### Output csv file for R plotting

# In[ ]:

pops = out[['1950', '1970', '1990', '2010', '2020', '2040', '2060', '2080', '2100']]
pops.to_csv("pops.csv")


# ### Calculate fraction for each region/country relative to the World population

# In[ ]:

a = pd.DataFrame()


# In[ ]:

for i in range(1,8):
    a[i] = out.iloc[i]/out.iloc[0]


# In[ ]:

a.columns = ['Africa', 'China', 'India', 'Europe', 'South America', 'North America', 'Other']
a = a.apply(pd.to_numeric)


# ### Plot using Seaborn Lineplot

# In[ ]:

sns.set_style("darkgrid", {"axes.facecolor": ".95"})
plot = sns.lineplot(data=a,dashes=False)
fig = plot.get_figure()
plot.set_ylabel('Fraction of World Population')
plot.set_xlabel('Year')
fig.get_axes()[0].legend(loc='upper left', fontsize='8')
vals = plot.get_yticks()
plot.set_yticklabels(['{:.0%}'.format(x) for x in vals], fontsize=8)


# ### Export figure as png file

# In[ ]:

fig.savefig('frac.png', dpi=400)


# ### Final Outputs
# - UN WPP excel file
# - pops.csv for R plotting
# - frac.png with the line plot
