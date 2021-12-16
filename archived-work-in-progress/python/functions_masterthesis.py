def my_load_data(): 
	import pandas as pd
	## read in dataframe and select only North Kivu province
	try: 
		df = pd.read_csv("~/Desktop/Masterarbeit Statistik/data/Data_ DRC Ebola Outbreak, North Kivu and Ituri - MOH-By-Health-Zone.csv")
	except:
		try: 
			df = pd.read_csv("../data/Data_ DRC Ebola Outbreak, North Kivu and Ituri - MOH-By-Health-Zone.csv")
		except: 
			print("Data File not Found")

	nkivu = df.loc[df["province"] == "North Kivu", ]

	## get rid of all NaN values and convert to integer, then sum up 
	## all cases in the North Kivu province and set the ones
	## smaller than 0 to 0. 
	## note: this should probably be done before aggregation, but well...
	nkivu = nkivu[["report_date", "total_cases_change"]].fillna(0)
	nkivu.loc[:, "total_cases_change"] = nkivu.loc[:, "total_cases_change"].astype(int)
	inc = nkivu.groupby("report_date").sum()
	inc.loc[inc["total_cases_change"] < 0,] = 0
	return(inc)


# ================================================= # 
# ================================================= # 

def my_test_train_data(incidences, train_proportion = 0.7):
	import pandas as pd
	import numpy as np

	y_nextday = incidences.shift(-1)

	tmp = pd.concat([incidences, y_nextday], axis = 1)
	tmp = tmp[:-1]
	tmp.columns = ["inc", "inc_next_day"]

	tmp['date'] = pd.to_datetime(tmp.index)
	tmp['weekday'] = tmp['date'].dt.dayofweek
	tmp['date'] = pd.to_numeric(tmp['date'])

	split_date = tmp.index[np.round(len(tmp) * train_proportion)]

	X_train = tmp.loc[tmp.index <= split_date, ["inc", "date", "weekday"]]
	X_test = tmp.loc[tmp.index > split_date, ["inc", "date", "weekday"]]

	y_train = tmp.loc[tmp.index <= split_date, ["inc_next_day"]]
	y_test = tmp.loc[tmp.index > split_date, ["inc_next_day"]]

	
	return X_train, X_test, y_train, y_test


