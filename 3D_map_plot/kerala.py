import geopandas as gpd
import pandas as pd

geo_df=gpd.read_file('district.shp')
data=pd.read_csv('district_covid.csv')
combined=geo_df.merge(data, on = 'DISTRICT')
pp=combined.plot(figsize=(15, 15), alpha=0.8, cmap='OrRd', column="Deaths", legend=True, edgecolor='black', linewidth=0.4)
pp.plot()
